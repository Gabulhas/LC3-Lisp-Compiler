open Assembly
open BuiltinSubroutines
open Lexer



(*Sets origin
  loads bottom of the stack to r6 and r5
  bottom of the stack is at the bottom of the user space memory (65023 or 0xFDFF)


*)
let initial_code:asm = (*.ORIG x3000*)
    orig 12288           ++ 
    comment "Register Initialization" ++
    ld r6 "STACK_BOTTOM_POINTER"++
    ld r5 "STACK_BOTTOM_POINTER" ++
    ld r4 "GLOBAL_DATA_POINTER" ++
    brnzp "MAIN"                ++

    comment "Pointers" ++
    label "GLOBAL_DATA_POINTER" ++
    fill_label "GLOBAL_DATA"     ++
    label "STACK_BOTTOM_POINTER"++
    fill 65023


(*TODO: remove this *)
let final_code =
    comment "Stack Manipulation"                       ++
    join_asm_lines 
        [stack_pull_subroutine; stack_push_subroutine] ++

    comment "Global Data Start"                        ++
    label "GLOBAL_DATA"                                 ++                                                  
    endd

(*
    How to deal with this recursive mess (☉_☉)?
    it would be better if these functions were declared somewhere else, but
    they must be recursive to recall the `from_ast` function recursively
 *)
let rec from_ast ast =
    match ast with
    | LList a -> (
            match (List.hd a) with
             | Symbol s -> symbol_generate s (List.tl a)
             | _ -> raise (Invalid_argument "S-Expression not starting with a symbol")
    )
    | Number a -> push_value a
    | Boolean a -> push_value (boolean_to_int a)
    | Symbol _ -> label "--NOT ADDED YET"
    | LString _ -> label "--NOT ADDED YET"


and add_func arguments=
    let pre_add = 
        List.map (from_ast) arguments
        |> join_asm_lines
    in
    pre_add     ++
    (
        Utils.list_fill (jsr "ADD_FUNC") ((List.length arguments) - 1)
        |> join_asm_lines
    )

and subtr_func arguments=
    let pre_sub = 
        List.map (from_ast) (List.tl arguments)
        |> join_asm_lines

    in
    let first_value =
        from_ast (List.hd arguments)
    in

    (join_asm_lines [pre_sub; first_value])     ++

    (
        Utils.list_fill (jsr "SUBTR_FUNC") ((List.length arguments) - 1)
        |> join_asm_lines
    )
and mull_func arguments=
    let pre_add = 
        List.map (from_ast) arguments
        |> join_asm_lines
    in
    pre_add     ++
    (
        Utils.list_fill (jsr "MUL_FUNC") ((List.length arguments) - 1)
        |> join_asm_lines
    )

and div_func arguments = 
    let pre_sub = 
        List.map (from_ast) (List.tl arguments)
        |> join_asm_lines

    in
    let first_value =
        from_ast (List.hd arguments)
    in

    (join_asm_lines [pre_sub; first_value])     ++

    (
        Utils.list_fill (jsr "DIV_FUNC") ((List.length arguments) - 1)
        |> join_asm_lines
    )

and modulo_func arguments = 
    let pre_sub = 
        List.map (from_ast) (List.tl arguments)
        |> join_asm_lines

    in
    let first_value =
        from_ast (List.hd arguments)
    in

    (join_asm_lines [pre_sub; first_value])     ++

    (
        Utils.list_fill (jsr "MODULO_FUNC") ((List.length arguments) - 1)
        |> join_asm_lines
    )


and compare_func arguments is_smaller is_equal =
    let n_to_compare = List.length arguments in
    let pre_eval = 
        List.map (from_ast) arguments
        |> join_asm_lines
    in
    pre_eval ++
    set_val r0 (to_imm is_smaller)   ++
    set_val r1 (to_imm is_equal)     ++
    set_val r3 (to_imm n_to_compare) ++
    jsr "COMPARE_START"

and symbol_generate sym arguments=
    match sym with
    | "+" -> add_func arguments
    | "-" -> subtr_func arguments 
    | "*" -> mull_func arguments
    | "/" -> div_func arguments
    | "%" -> modulo_func arguments
    | ">" -> compare_func (List.rev arguments) 1 0
    | ">=" -> compare_func (List.rev arguments) 1 1
    | "<" -> compare_func arguments 1 0
    | "<=" -> compare_func arguments 1 1
    | "=" -> compare_func arguments 0 1
    | "define" -> ""
    | "print" -> ""
    | _ -> ""




let generation_pipeline ast = 
    initial_code                       ++
    label "MAIN"                       ++

    from_ast ast                       ++
    halt                               ++
    BuiltinSubroutines.all_subroutines ++
    final_code
