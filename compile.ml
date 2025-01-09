
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

let is_truthy (expr : texpr) : [`text] asm =
      match expr with
      | TEcst Cnone -> xorq !%rax !%rax (* None is false, set %rax to 0 *)
      | TEcst (Cbool b) -> movq (imm (if b then 1 else 0)) !%rax
      | TEcst (Cint i) -> cmpq (imm 0) !%rax (* Compare integer with 0 *)
      | TEcst (Cstring s) ->
          let label_name = Printf.sprintf "str_%s" (Digest.to_hex (Digest.string s)) in
          leaq (lab label_name) rax ++
          cmpq (imm 0) !%rax (* Compare string pointer with 0 *)
      | TEvar var ->
          movq (ind ~ofs:(var.v_ofs) rbp) !%rax ++
          cmpq (imm 0) !%rax (* Compare variable with 0 *)
      | _ -> failwith "Unsupported type for truthiness evaluation"

let rec compile_expr (expr : texpr) : [`text] asm =
  match expr with
  | TEcst Cnone -> 
    xorq !%rax !%rax
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
    let is_string_type expr =
      match expr with
      | TEcst (Cstring _) -> true
      | TEvar var -> var.v_type = "string"
      | _ -> false
    in
    if is_string_type e1 && is_string_type e2 then
      compile_expr e1 ++                (* Compile the first string into %rax *)
      pushq !%rax ++                    (* Push the first string address onto the stack *)
      compile_expr e2 ++                (* Compile the second string into %rax *)
      movq !%rax !%rsi ++               (* Move the second string address to %rsi *)
      popq rdi ++                       (* Pop the first string address into %rdi *)
      movq !%rdi !%rdi ++               (* Store the destination in %rdi *)
      call "my_strcat" ++               (* Call the custom strcat function *)
      movq !%rdi !%rax                  (* Store the result in %rax *)                    (* Concatenate the second string *)
    else
      compile_expr e1 ++
      pushq !%rax ++

      compile_expr e2 ++

      popq rbx ++
      addq !%rbx !%rax
  | TEbinop ({ kind = Bsub; _ }, e1, e2) ->
      compile_expr e1 ++
      pushq !%rax ++
    
      compile_expr e2 ++
    
      popq rbx ++
      subq !%rax !%rbx ++
      movq !%rbx !%rax
  | TEbinop ({ kind = Bmul; _ }, e1, e2) ->
        compile_expr e1 ++
        pushq !%rax ++
        compile_expr e2 ++
        popq rbx ++
        imulq !%rbx !%rax
  | TEbinop ({ kind = Bdiv; _ }, e1, e2) ->
      let runtime_error_division_by_zero = "runtime_error_division_by_zero" in
      compile_expr e1 ++
      pushq !%rax ++
      compile_expr e2 ++
      cmpq (imm 0) !%rax ++
      je runtime_error_division_by_zero ++
      movq !%rax !%rbx ++
      popq rax ++
      xorq !%rdx !%rdx ++
      idivq !%rbx
  | TEbinop ({ kind = Bmod; _ }, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    movq !%rax !%rbx ++
    popq rax ++
    xorq !%rdx !%rdx ++
    idivq !%rbx ++
    movq !%rdx !%rax
  | TEbinop ({ kind = Band; _ }, e1, e2) ->
    let false_label = "false_branch" in
    let end_label = "end_label" in
    compile_expr e1 ++
    cmpq (imm 0) !%rax ++
    je false_label ++ 
    compile_expr e2 ++ 
    jmp end_label ++ 
    label false_label ++ 
    movq (imm 0) !%rax ++
    label end_label
  | TEbinop ({ kind = Bor; _ }, e1, e2) ->
    let true_label = "true_branch" in
    let end_label = "end_label" in
    compile_expr e1 ++
    cmpq (imm 1) !%rax ++
    je true_label ++ 
    compile_expr e2 ++ 
    jmp end_label ++ 
    label true_label ++ 
    movq (imm 1) !%rax ++
    label end_label
  | TEbinop ({ kind = Bcmp Beq; _ }, e1, e2) ->
    let true_branch = "true_branch" in
    let false_branch = "false_branch" in
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    je true_branch ++
    jmp false_branch
  | TEbinop ({ kind = Bcmp Bneq; _ }, e1, e2) ->
    let true_branch = "true_branch" in
    let false_branch = "false_branch" in
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    je false_branch ++
    jmp true_branch
  | TEbinop ({ kind = Bcmp Blt; _ }, e1, e2) -> (* "<" *)
    let true_branch = "true_branch" in
    let false_branch = "false_branch" in
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++ (* Compare %rbx (e1) and %rax (e2) *)
    jl true_branch ++   (* Jump to true_branch if %rbx < %rax *)
    jmp false_branch

  | TEbinop ({ kind = Bcmp Ble; _ }, e1, e2) -> (* "<=" *)
      let true_branch = "true_branch" in
      let false_branch = "false_branch" in
      compile_expr e1 ++
      pushq !%rax ++
      compile_expr e2 ++
      popq rbx ++
      cmpq !%rax !%rbx ++
      jle true_branch ++
      jmp false_branch

  | TEbinop ({ kind = Bcmp Bgt; _ }, e1, e2) -> (* ">" *)
      let true_branch = "true_branch" in
      let false_branch = "false_branch" in
      compile_expr e1 ++
      pushq !%rax ++
      compile_expr e2 ++
      popq rbx ++
      cmpq !%rax !%rbx ++
      jg true_branch ++
      jmp false_branch

  | TEbinop ({ kind = Bcmp Bge; _ }, e1, e2) -> (* ">=" *)
      let true_branch = "true_branch" in
      let false_branch = "false_branch" in
      compile_expr e1 ++
      pushq !%rax ++
      compile_expr e2 ++
      popq rbx ++
      cmpq !%rax !%rbx ++
      jge true_branch ++
      jmp false_branch
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
        if fn.fn_name = "malloc" then
          call "my_malloc"
        else if fn.fn_name = "printf" then
          call "my_printf"
        else if fn.fn_name = "putchar" then
          call "my_putchar"
        else
          call fn.fn_name
  | TEunop ({ kind = Uneg; _ }, e) ->
            compile_expr e ++
            negq !%rax  (* Negate the value in RAX *)
  | TEunop ({ kind = Unot; _ }, e) ->
            compile_expr e ++
            cmpq (imm 0) !%rax ++
            movq (imm 0) !%rax ++
            sete !%al
  | _ -> failwith "Unhandled expression"

let rec compile_stmt (stmt : tstmt) : [`text] asm =
  match stmt with
  | TSblock stmts ->
    List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop stmts
  | TSprint expr ->
      let is_string_type expr =
        match expr with
        | TEcst (Cstring _) -> true
        | TEvar var -> var.v_type = "string"
        | _ -> false
      in
      if is_string_type expr then
        compile_expr expr ++
        movq !%rax !%rdi ++
        call "puts"
      else
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
  | TSif (cond, then_stmt, else_stmt) ->
    let true_label = "true_branch" in
    let false_label = "false_branch" in
    let end_label = "end_label" in
    compile_expr cond ++
    is_truthy cond ++
    je false_label ++
    label true_label ++
    compile_stmt then_stmt ++
    jmp end_label ++
    label false_label ++
    compile_stmt else_stmt ++
    label end_label
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
      let collect_strings (strings: string list) : [`data] asm =
        List.fold_left
          (fun acc s ->
            let label_name = Printf.sprintf "str_%s" (Digest.to_hex (Digest.string s)) in
            acc ++ label label_name ++ string s)
          nop
          strings
      in
      let rec collect_expr_strings expr acc =
        match expr with
        | TEcst (Cstring s) -> s :: acc
        | TEbinop (_, e1, e2) -> collect_expr_strings e1 acc |> collect_expr_strings e2
        | TEunop (_, e) -> collect_expr_strings e acc
        | _ -> acc
      in
      let rec collect_stmt_strings stmt acc =
        match stmt with
        | TSassign (_, expr) -> collect_expr_strings expr acc
        | TSprint expr -> collect_expr_strings expr acc
        | TSif (cond, then_stmt, else_stmt) ->
            collect_expr_strings cond acc
            |> collect_stmt_strings then_stmt
            |> collect_stmt_strings else_stmt
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
      let text = 
        List.fold_left (fun acc def -> acc ++ compile_def def) nop tfile ++
        label "runtime_error_division_by_zero" ++
        movq (imm 1) !%rdi ++
        call "exit" ++
        label "my_malloc" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        call "malloc" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret ++
        label "my_strlen" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        subq (imm 16) !%rsp ++
        call "strlen" ++
        addq (imm 16) !%rsp ++
        popq rbp ++
        ret ++
        label "my_strcpy" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        subq (imm 16) !%rsp ++
        call "strcpy" ++
        addq (imm 16) !%rsp ++
        popq rbp ++
        ret ++

        (* Wrapper for strcat *)
        label "my_strcat" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++

        xorq !%rax !%rax ++      (* Clear %rax *)
        movq !%rsi !%rdx ++      (* Save the pointer to the first string list in %rdx *)
        
        
        label "concat_length_loop" ++
        testq !%rdx !%rdx ++     (* Check if the pointer is NULL *)
        je "concat_length_done" ++ (* Exit loop if NULL *)
        movq !%rdx !%rdi ++      (* Copy the string pointer into %rdi *)
        call "strlen" ++         (* Get the length of the current string *)
        addq !%rax !%rcx ++      (* Add length to total (in %rcx) *)
        addq (imm 1) !%rcx ++    (* Account for null terminator *)
        addq (imm 8) !%rdx ++    (* Move to the next string pointer *)
        jmp "concat_length_loop" ++

        label "concat_length_done" ++
        
        movq !%rcx !%rdi ++      (* Pass total length in %rdi for malloc *)
        call "my_malloc" ++      (* Allocate memory *)
        movq !%rax !%rsi ++      (* Store allocated memory address in %rsi *)

        
        movq !%rsi !%rdx ++      (* Save base address of allocated memory *)
        xorq !%rcx !%rcx ++      (* Clear counter register *)
        movq !%rdx !%rdi ++      (* Restore pointer to the first string list *)

        label "concat_copy_loop" ++
        testq !%rdi !%rdi ++     (* Check if the pointer is NULL *)
        je "concat_copy_done" ++   (* Exit loop if NULL *)
        movq !%rdi !%rsi ++      (* Set source string for strcpy/strcat *)
        cmpq (imm 0) !%rcx ++    (* Check if it’s the first string *)
        je "first_copy" ++         (* Use strcpy for the first string *)

        call "strcat" ++
        addq (imm 8) !%rdi ++    (* Move to the next string pointer *)
        jmp "concat_copy_loop" ++

        label "first_copy" ++
        call "strcpy" ++         (* Use strcpy for the first string *)
        addq (imm 8) !%rdi ++    (* Move to the next string pointer *)
        incq !%rcx ++            (* Increment string counter *)
        jmp "concat_copy_loop" ++

        label "concat_copy_done" ++
        movq !%rdx !%rax ++      (* Store address of the result in %rax *)

        movq !%rbp !%rsp ++      (* Restore stack *)
        popq rbp ++
        ret

      in
    
      let data = 
        collect_strings string_constants ++
        label "fmt" ++ string "%d\n" ++
        label "true_msg" ++ string "True\n" ++
        label "false_msg" ++ string "False\n"
      in
    
      { text; data }
    
