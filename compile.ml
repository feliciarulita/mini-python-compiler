
open Format
open X86_64
open Ast
open Typing

let debug = ref false

let collect_strings (strings: string list) : [`data] asm =
  List.fold_left
    (fun acc s ->
      let label_name = Printf.sprintf "str_%s" (Digest.to_hex (Digest.string s)) in
      acc ++ label label_name ++ string s)
    nop
    strings


let rec compile_expr (expr : texpr) : [`text] asm =
  match expr with
  | TEcst (Cint i) ->
      movq (imm64 i) !%rax
  | TEcst (Cbool i) ->
      let value = if i then 1 else 0 in
      movq (imm value) !%rax
  | TEcst (Cstring s) ->
    let label_name = Printf.sprintf "str_%s" (Digest.to_hex (Digest.string s)) in
    leaq (lab label_name) rax 
  | TEvar var ->
    movq (ind ~ofs:(var.v_ofs) rbp) !%rax  
  | TEbinop ({ kind = Badd; _ }, e1, e2) ->
      (* Compile left operand *)
      compile_expr e1 ++
      pushq !%rax ++         (* Save the result of e1 on the stack *)

      (* Compile right operand *)
      compile_expr e2 ++

      (* Retrieve the saved left operand and perform the addition *)
      popq rbx ++            (* Pop e1 result into %rbx *)
      addq !%rbx !%rax       (* Add e1 (%rbx) and e2 (%rax), result in %rax *)
  
  | TEcall (fn, args) ->
        let arg_moves =
          args
          |> List.mapi (fun i arg ->
             compile_expr arg ++ movq !%rax (match i with
               | 0 -> !%rdi
               | 1 -> !%rsi
               | 2 -> !%rdx
               | 3 -> !%rcx
               | _ -> failwith "Too many arguments (only 4 supported)"))
          |> List.fold_left (++) nop
        in
        arg_moves ++
        call fn.fn_name
  | _ -> failwith "Unhandled expression"

let rec compile_stmt (stmt : tstmt) : [`text] asm =
  match stmt with
  | TSblock stmts ->
    List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop stmts
  | TSprint expr ->
    compile_expr expr ++
    movq !%rax !%rsi ++
    leaq (lab "fmt") rdi ++
    movq (imm 0) !%rax ++ 
    call "printf"
  | TSassign (var,expr) ->
    compile_expr expr ++
    movq !%rax (ind ~ofs:(var.v_ofs) rbp)
  | TSreturn expr ->
    compile_expr expr ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret
  | _ -> nop

let rec extract_local_vars stmt =
    match stmt with
    | TSassign (var, _) -> [var]  (* Collect assigned variables *)
    | TSblock stmts -> List.flatten (List.map extract_local_vars stmts)
    | _ -> []

    
let compile_def ((fn, body) : tdef) : [`text] asm =
  (* Assign offsets for function parameters *)
  List.iteri (fun i param ->
    param.v_ofs <- match i with
      | 0 -> 0          (* First parameter in %rdi *)
      | 1 -> -8         (* Second parameter in %rsi *)
      | 2 -> -16        (* Third parameter in %rdx *)
      | 3 -> -24        (* Fourth parameter in %rcx *)
      | _ -> failwith "Too many arguments (only 4 supported)"
  ) fn.fn_params;
  
  let local_var_offset = ref (-8) in  (* Start below parameters *)
  List.iter (fun local_var ->
    local_var.v_ofs <- !local_var_offset;
    local_var_offset := !local_var_offset - 8;  (* Allocate 8 bytes per variable *)
  ) (extract_local_vars body);
  
  let prologue =
    pushq !%rbp ++
    movq !%rsp !%rbp
  in
  let epilogue_main =
    popq rbp ++
    movq (imm 0) !%rdi ++
    call "exit"
  in
  let epilogue =
    movq !%rbp !%rsp ++
    popq rbp ++
    ret
  in
  
  if fn.fn_name = "main" then
    globl fn.fn_name ++
    label fn.fn_name ++
    prologue ++
    compile_stmt body ++
    epilogue_main
  else
    globl fn.fn_name ++
    label fn.fn_name ++
    prologue ++
    subq (imm 48) !%rsp ++
    movq !%rdi (ind ~ofs:(0) rbp) ++
    movq !%rsi (ind ~ofs:(-8) rbp) ++
    movq !%rdx (ind ~ofs:(-16) rbp) ++
    movq !%rcx (ind ~ofs:(-24) rbp) ++
    movq !%r8 (ind ~ofs:(-32) rbp) ++
    movq !%r9 (ind ~ofs:(-40) rbp) ++
    compile_stmt body ++
    epilogue

let file ?debug:(b=false) (tfile: Ast.tfile) : X86_64.program =
  let rec collect_expr_strings expr acc =
    match expr with
    | TEcst (Cstring s) -> s :: acc  (* Add the string constant to the accumulator *)
    | TEbinop (_, e1, e2) -> collect_expr_strings e1 acc |> collect_expr_strings e2
    | TEunop (_, e) -> collect_expr_strings e acc
    | _ -> acc
  in

  let rec collect_stmt_strings stmt acc =
    match stmt with
    | TSassign (_, expr) -> collect_expr_strings expr acc
    | TSprint expr -> collect_expr_strings expr acc
    | TSblock stmts -> List.fold_left (fun acc stmt -> collect_stmt_strings stmt acc) acc stmts
    | _ -> acc
  in

  let string_constants =
    List.fold_left
      (fun acc (_, body) -> collect_stmt_strings body acc)
      []
      tfile
    |> List.sort_uniq String.compare  (* Ensure strings are unique *)
  in

  (* Compile text and data sections *)
  let text = List.fold_left (fun acc def -> acc ++ compile_def def) nop tfile in
  let data = collect_strings string_constants ++ label "fmt" ++ string "%d\n" in
  { text; data }
