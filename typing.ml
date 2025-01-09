
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos (*used when there is no meaningful or specific location to associate with an error or data*)

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* Environment types *)
type env = {
  vars : (string, var) Hashtbl.t;
  funcs : (string, fn) Hashtbl.t;
}

(*defining data type*)
let type_infer (e:texpr) : string =
  match e with
  | TEcst (Cint _) -> "int"
  | TEcst (Cbool _) -> "bool"
  | TEcst (Cstring _) -> "string"
  | _ -> "unknown"

let rec type_expr (env:env) (e:expr) : texpr =
  match e with
  | Ecst c -> TEcst c
  | Eident id ->
      let var =
        try Hashtbl.find env.vars id.id
        with Not_found -> error ~loc:id.loc "unbound variable %s" id.id
      in
      TEvar var
      | Ebinop (op, e1, e2) ->
        let te1 = type_expr env e1 in
        let te2 = type_expr env e2 in
        let inf1 = type_infer te1 in
        let inf2 = type_infer te2 in
        if inf1 == inf2 then
          if op.kind = Bdiv then
            match te2 with
            | TEcst (Cint 0L) -> 
                error ~loc: op.loc "division by zero"
            | _ -> TEbinop (op, te1, te2)
          else
            TEbinop (op, te1, te2)
        else if (inf1 == "bool" && inf2 == "int") then
          TEbinop (op, te1, te2)
        else
          error ~loc: op.loc "unsupported operand type(s) for : %s and %s" inf1 inf2
  | Eunop (op, e) ->
      let te = type_expr env e in               (* Type-check the operand *)
      let inferred_type = type_infer te in      (* Infer the operand's type *)
      if op.kind = Uneg then (* Unary minus for numeric types *)
        if inferred_type = "int" || inferred_type = "float" then
          TEunop (op, te)                      (* Valid type for unary minus *)
        else
          error ~loc:op.loc                     (* Use the operand's location *)
            "unsupported operand type for unary minus: %s" inferred_type
      else if op.kind = Unot then (* Logical NOT for boolean types *)
        if inferred_type = "bool" then
          TEunop (op, te)                      (* Valid type for logical NOT *)
        else
          error ~loc:op.loc                     (* Use the operand's location *)
            "unsupported operand type for 'not': %s" inferred_type
      else
        error ~loc:op.loc                       (* Unsupported operator *)
          "unsupported unary operator or operand type"

  | Ecall (id, args) ->
    if id.id = "range" then
      match args with
      | [end_expr] ->
          let tend = type_expr env end_expr in
          let tend_type = type_infer tend in
          if (tend_type = "int" || match tend with | TEunop (op,e) -> op.kind = Uneg | _ -> false) then
            TErange (TEcst (Cint 0L), tend, TEcst (Cint 1L))  (* range(0, end, 1) *)
          else
            error ~loc:id.loc "range argument must be an integer"
      | [start_expr; end_expr] ->
          let tstart = type_expr env start_expr in
          let tend = type_expr env end_expr in
          let tstart_type = type_infer tstart in
          let tend_type = type_infer tend in
          if ((tstart_type = "int" || match tstart with | TEunop (op,e) -> op.kind = Uneg | _ -> false) &&
              (tend_type = "int" || match tend with | TEunop (op,e) -> op.kind = Uneg | _ -> false)) then
            TErange (tstart, tend, TEcst (Cint 1L))  (* range(start, end, 1) *)
          else
            error ~loc:id.loc "range arguments must be integers"
      | [start_expr; end_expr; step_expr] ->
          let tstart = type_expr env start_expr in
          let tend = type_expr env end_expr in
          let tstep = type_expr env step_expr in
          let tstart_type = type_infer tstart in
          let tend_type = type_infer tend in
          let tstep_type = type_infer tstep in
          if ((tstart_type = "int" || match tstart with | TEunop (op,e) -> op.kind = Uneg | _ -> false) &&
              (tend_type = "int" || match tend with | TEunop (op,e) -> op.kind = Uneg | _ -> false) &&
              (tstep_type = "int" || match tstep with | TEunop (op,e) -> op.kind = Uneg | _ -> false)) then
            if (match tstep with | TEcst (Cint 0L) -> true | _ -> false) then
              error ~loc:id.loc "step argument must not be zero"
            else
              TErange (tstart, tend, tstep)  (* range(start, end, step) *)
          else
            error ~loc:id.loc "range arguments must be integers"
      | _ -> error ~loc:id.loc "range expects 1, 2, or 3 arguments"
    
    else
      (* Handle normal function calls *)
      let fn =
        try Hashtbl.find env.funcs id.id
        with Not_found -> error ~loc:id.loc "unbound function %s" id.id
      in
      let targs = List.map (type_expr env) args in
      TEcall (fn, targs)
  | Elist elems ->
      let telems = List.map (type_expr env) elems in
      TElist telems
  | Eget (list_expr, index_expr) ->
      let tlist = type_expr env list_expr in
      let tindex = type_expr env index_expr in
      TEget (tlist, tindex)
      
let rec type_stmt (env: env) (s: stmt) : tstmt =
  match s with
  | Sassign (id, e) ->
    let var =
      try Hashtbl.find env.vars id.id
      with Not_found ->
        let new_var = { v_name = id.id; v_ofs = 0 } in
        Hashtbl.add env.vars id.id new_var;
        new_var
    in
    let te = type_expr env e in
    TSassign (var, te)
  | Sreturn e ->
    let te = type_expr env e in
    TSreturn te
  | Sprint e ->
    let te = type_expr env e in
    TSprint te
  | Sblock stmts ->
    let tstmts = List.map (type_stmt env) stmts in
    TSblock tstmts
  | Sif (cond, then_branch, else_branch) ->
    let tcond = type_expr env cond in
    let tthen = type_stmt env then_branch in
    let telse = type_stmt env else_branch in
    TSif (tcond, tthen, telse)
  | Sfor (id, range_expr, body) ->
    let trange = type_expr env range_expr in
    let var = { v_name = id.id; v_ofs = 0 } in
    Hashtbl.add env.vars id.id var;
    let tbody = type_stmt env body in
    TSfor (var, trange, tbody)
  | Seval e ->
    let te = type_expr env e in
    TSeval te
  | Sset (list_expr, index_expr, value_expr) ->
    let tlist = type_expr env list_expr in
    let tindex = type_expr env index_expr in
    let tvalue = type_expr env value_expr in
    TSset (tlist, tindex, tvalue)

let type_function (env: env) (fn: def) : tdef =
  let (id, params, body) = fn in
  let param_vars =
    List.map (fun param -> { v_name = param.id; v_ofs = 0 }) params
  in
  let func_record = { fn_name = id.id; fn_params = param_vars } in
  Hashtbl.add env.funcs id.id func_record;
  let local_env = { vars = Hashtbl.create 16; funcs = env.funcs } in
  List.iter (fun param -> Hashtbl.add local_env.vars param.v_name param) param_vars;
  let tbody = type_stmt local_env body in
  (func_record, tbody)

(*takes the parsed AST (and converts it into the typed AST (Ast.tfile) *)
let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  let env = { vars = Hashtbl.create 16; funcs = Hashtbl.create 16 } in
  let defs, main = p in
  let typed_defs = List.map (type_function env) defs in
  let main_fn = ({ fn_name = "main"; fn_params = [] }, type_stmt env main) in
  let tfile = typed_defs @ [main_fn] in
  Printf.printf "Generated Typed AST:\n%s\n" (Ast.string_of_tfile tfile);
  tfile