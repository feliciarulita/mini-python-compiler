
open Format
open X86_64
open Ast

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
    leaq (lab label_name) rax  (* Load the address of the string into %rax *)
    
  | TEvar var ->
    movq (ind ~ofs:(-var.v_ofs) rbp) !%rax
  | TEbinop ({ kind = Badd; _ }, e1, e2) ->
      compile_expr e1 ++  (* Compile left operand *)
      pushq !%rax ++      (* Save result on stack *)
      compile_expr e2 ++  (* Compile right operand *)
      popq rbx ++         (* Retrieve left operand into %rbx *)
      addq !%rbx !%rax    (* Add %rbx and %rax, result in %rax *)
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
    movq !%rax (ind ~ofs:(-var.v_ofs) rbp)
  | _ -> nop
let compile_def ((fn, body) : tdef) : [`text] asm =
  let prologue =
    pushq !%rbp ++
    movq !%rsp !%rbp
  in
  let epilogue_main =
    popq rbp ++
    movq (imm 0) !%rdi ++
    call "exit"
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
    compile_stmt body ++
    ret

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
  let data = collect_strings string_constants ++ label "fmt" ++ string "%s\n" in
  { text; data }
