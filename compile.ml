
open Format
open X86_64
open Ast

let debug = ref false

let rec compile_expr (expr : texpr) : [`text] asm =
  match expr with
  | TEcst (Cint i) ->
      movq (imm64 i) !%rax (* Load constant into %rax *)
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
    movq !%rax !%rsi ++   (* Move result into second argument for printf *)
    leaq (lab "fmt") rdi ++ (* Load format string into %rdi *)
    movq (imm 0) !%rax ++ (* Clear %rax for variadic functions *)
    call "printf"         (* Call printf *)
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
  debug := b;
  Printf.printf "Debugging Typed AST (tfile):\n%s\n" (Ast.string_of_tfile tfile);
  (* Translate all function definitions *)
  let text = List.fold_left (fun acc def -> acc ++ compile_def def) nop tfile in
  (* Add a data section with the printf format string *)
  let data = label "fmt" ++ string "%d\n" in
  { text; data }
