
(** {2 Abstract Syntax of Mini-Python} *)

(** {3 Parsed trees}

   This is the output of the parser and the input of the type checker. *)

type location = Lexing.position * Lexing.position

type ident = { loc: location; id: string; }

type unop =
  | Uneg (** -e *)
  | Unot (** not e *)
  
  
type comparison = 
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (** == != < <= > >= *)

type binop_kind =
    | Badd | Bsub | Bmul | Bdiv | Bmod    (** + - * // % *)
    | Band | Bor                          (** and or *)
    | Bcmp of comparison
  
type binop = {
  kind: binop_kind;       (* The specific binary operator, like Badd, Bsub, etc. *)
  loc: location;          (* The location of the operator *)
}

type constant =
  | Cnone
  | Cbool of bool
  | Cstring of string
  | Cint of int64

type expr =
  | Ecst of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecall of ident * expr list
  | Elist of expr list (** {[ [e1,e2,...] ]} *)
  | Eget of expr * expr (** {[ e1[e2] ]} *)

and stmt =
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sassign of ident * expr
  | Sprint of expr
  | Sblock of stmt list
  | Sfor of ident * expr * stmt
  | Seval of expr
  | Sset of expr * expr * expr (** {[ e1[e2] = e3 ]} *)

and def = ident * ident list * stmt

and file = def list * stmt

(** {3 Typed trees}

   This is the output of the type checker and the input of the code
   generation. *)

(** In the typed trees, all the occurrences of the same variable
   point to a single record of the following type. *)
type var = {
  v_name: string;
  mutable v_ofs: int; (** position wrt %rbp *)
}

(** Similarly, all the occurrences of a given function all point
   to a single record of the following type. *)
type fn = {
  fn_name: string;
  fn_params: var list;
}

type texpr =
  | TEcst of constant
  | TEvar of var
  | TEbinop of binop * texpr * texpr
  | TEunop of unop * texpr
  | TEcall of fn * texpr list
  | TElist of texpr list
  | TErange of texpr (** list(range(e1)) *)
  | TEget of texpr * texpr (** {[ e1[e2] ]} *)
  | TEconvert of string * texpr

type tstmt =
  | TSif of texpr * tstmt * tstmt
  | TSreturn of texpr
  | TSassign of var * texpr
  | TSprint of texpr
  | TSblock of tstmt list
  | TSfor of var * texpr * tstmt
  | TSeval of texpr
  | TSset of texpr * texpr * texpr (** {[ e1[e2] = e3 ]} *)

and tdef = fn * tstmt

and tfile = tdef list
  (** the block of global statements is now a `main` function *)

(*for debug*)
let string_of_constant = function
  | Cnone -> "None"
  | Cbool b -> string_of_bool b
  | Cstring s -> "\"" ^ s ^ "\""
  | Cint i -> Int64.to_string i

  let string_of_unop = function
  | Uneg -> "-"
  | Unot -> "not"

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "//"
  | Bmod -> "%"
  | Band -> "and"
  | Bor -> "or"

let string_of_comparison = function
  | Beq -> "=="
  | Bneq -> "!="
  | Blt -> "<"
  | Ble -> "<="
  | Bgt -> ">"
  | Bge -> ">="

let rec string_of_expr = function
  | Ecst c -> string_of_constant c
  | Eident id -> id.id
  | Ebinop (op, e1, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op.kind ^ " " ^ string_of_expr e2 ^ ")"
  | Eunop (op, e) ->
      string_of_unop op ^ string_of_expr e
  | Ecall (id, args) ->
      id.id ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Elist elems ->
      "[" ^ String.concat ", " (List.map string_of_expr elems) ^ "]"
  | Eget (list_expr, index_expr) ->
      string_of_expr list_expr ^ "[" ^ string_of_expr index_expr ^ "]"

let rec string_of_stmt = function
  | Sif (cond, then_stmt, else_stmt) ->
    "if " ^ string_of_expr cond ^ ":\n  " ^ string_of_stmt then_stmt ^
    (match else_stmt with
      | Sblock [] -> ""
      | _ -> "\nelse:\n  " ^ string_of_stmt else_stmt)
  | Sreturn e -> "return " ^ string_of_expr e
  | Sassign (id, expr) -> id.id ^ " = " ^ string_of_expr expr
  | Sprint expr -> "print(" ^ string_of_expr expr ^ ")"
  | Sblock stmts ->
    "{\n  " ^ String.concat "\n  " (List.map string_of_stmt stmts) ^ "\n}"
  | Sfor (id, range_expr, body) ->
    "for " ^ id.id ^ " in " ^ string_of_expr range_expr ^ ":\n  " ^ string_of_stmt body
  | Seval expr -> string_of_expr expr
  | Sset (list_expr, index_expr, value_expr) ->
    string_of_expr list_expr ^ "[" ^ string_of_expr index_expr ^ "] = " ^ string_of_expr value_expr
  
  let string_of_file (f: file) =
    let defs, main = f in
    let string_of_def (id, params, body) =
      id.id ^ "(" ^ String.concat ", " (List.map (fun p -> p.id) params) ^ ") {\n" ^
      string_of_stmt body ^ "\n}"
    in
    String.concat "\n\n" (List.map string_of_def defs) ^
    "\n\nmain:\n" ^ string_of_stmt main
    
let rec string_of_texpr = function
  | TEcst c -> string_of_constant c
  | TEvar v -> v.v_name
  | TEbinop (op, e1, e2) ->
      "(" ^ string_of_texpr e1 ^ " " ^ string_of_binop op.kind ^ " " ^ string_of_texpr e2 ^ ")"
  | TEunop (op, e) ->
      string_of_unop op ^ string_of_texpr e
  | TEcall (fn, args) ->
          fn.fn_name ^ "(" ^ String.concat ", " (List.map string_of_texpr args) ^ ")"
  | TElist elems ->
    "[" ^ String.concat ", " (List.map string_of_texpr elems) ^ "]"
  | TErange e -> "range(" ^ string_of_texpr e ^ ")"
  | TEget (list_expr, index_expr) ->
    string_of_texpr list_expr ^ "[" ^ string_of_texpr index_expr ^ "]"
  | TEconvert ("int", e) -> "Convert to integer"

let rec string_of_tstmt = function
  | TSif (cond, then_stmt, else_stmt) ->
    "if " ^ string_of_texpr cond ^ ":\n  " ^ string_of_tstmt then_stmt ^
    (match else_stmt with
      | TSblock [] -> ""
      | _ -> "\nelse:\n  " ^ string_of_tstmt else_stmt)
  | TSreturn e -> "return " ^ string_of_texpr e
  | TSassign (var, expr) -> var.v_name ^ " = " ^ string_of_texpr expr
  | TSprint expr -> "print(" ^ string_of_texpr expr ^ ")"
  | TSblock stmts ->
    "{\n  " ^ String.concat "\n  " (List.map string_of_tstmt stmts) ^ "\n}"
  | TSfor (var, range_expr, body) ->
    "for " ^ var.v_name ^ " in " ^ string_of_texpr range_expr ^ ":\n  " ^ string_of_tstmt body
  | TSeval expr -> string_of_texpr expr
  | TSset (list_expr, index_expr, value_expr) ->
    string_of_texpr list_expr ^ "[" ^ string_of_texpr index_expr ^ "] = " ^ string_of_texpr value_expr

let string_of_file (tfile: tfile) =
  let string_of_tdef (fn, body) =
    fn.fn_name ^ "(" ^ String.concat ", " (List.map (fun p -> p.v_name) fn.fn_params) ^ ") {\n" ^
    string_of_tstmt body ^ "\n}"
  in
  String.concat "\n\n" (List.map string_of_tdef tfile)

  let rec string_of_texpr = function
  | TEcst (Cint i) -> Printf.sprintf "TEcst (Cint %Ld)" i
  | TEcst (Cbool b) -> Printf.sprintf "TEcst (Cbool %b)" b
  | TEcst (Cstring s) -> Printf.sprintf "TEcst (Cstring \"%s\")" s
  | TEbinop (op, e1, e2) ->
      Printf.sprintf "TEbinop (%s, %s, %s)"
        (string_of_binop op.kind)
        (string_of_texpr e1)
        (string_of_texpr e2)
  | TEunop (op, e) ->
      Printf.sprintf "TEunop (%s, %s)" (string_of_unop op) (string_of_texpr e)
  | TEvar var -> Printf.sprintf "TEvar (%s)" var.v_name
  | TEcall (fn, args) ->
      Printf.sprintf "TEcall (%s, [%s])" fn.fn_name (String.concat ", " (List.map string_of_texpr args))
  | TElist elems ->
      Printf.sprintf "TElist [%s]" (String.concat "; " (List.map string_of_texpr elems))
  | TEget (list_expr, index_expr) ->
      Printf.sprintf "TEget (%s, %s)" (string_of_texpr list_expr) (string_of_texpr index_expr)
  | _ -> "Unhandled texpr"

let rec string_of_tstmt = function
  | TSassign (var, expr) ->
      Printf.sprintf "TSassign (%s, %s)" var.v_name (string_of_texpr expr)
  | TSreturn expr ->
      Printf.sprintf "TSreturn (%s)" (string_of_texpr expr)
  | TSprint expr ->
      Printf.sprintf "TSprint (%s)" (string_of_texpr expr)
  | TSblock stmts ->
      Printf.sprintf "TSblock [%s]" (String.concat "; " (List.map string_of_tstmt stmts))
  | TSif (cond, then_branch, else_branch) ->
      Printf.sprintf "TSif (%s, %s, %s)"
        (string_of_texpr cond)
        (string_of_tstmt then_branch)
        (string_of_tstmt else_branch)
  | TSfor (var, range_expr, body) ->
      Printf.sprintf "TSfor (%s, %s, %s)" var.v_name (string_of_texpr range_expr) (string_of_tstmt body)
  | TSeval expr ->
      Printf.sprintf "TSeval (%s)" (string_of_texpr expr)
  | TSset (list_expr, index_expr, value_expr) ->
      Printf.sprintf "TSset (%s, %s, %s)"
        (string_of_texpr list_expr)
        (string_of_texpr index_expr)
        (string_of_texpr value_expr)

let string_of_tdef (fn, body) =
  Printf.sprintf "%s(%s) {\n%s\n}"
    fn.fn_name
    (String.concat ", " (List.map (fun var -> var.v_name) fn.fn_params))
    (string_of_tstmt body)

let string_of_tfile (tfile : tfile) =
  String.concat "\n\n" (List.map string_of_tdef tfile)

    
  

