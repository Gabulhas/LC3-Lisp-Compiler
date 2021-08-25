open Assembly
open BuiltinSubroutines
open Lexer

let initial_code:asm = (*.ORIG x3000*)
    orig 12288           ++ 
    lea r6 "STACK"       


let final_code =
    join_asm_lines [stack_pull_subroutine; stack_push_subroutine] ++
    label "STACK"                                      ++
    endd

let emit_functions =
    "\n;--FUNCTIONS--\n"




(*
    How to deal with this recursive mess (☉_☉)?


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

and symbol_generate sym arguments=
    match sym with
    | "+" -> add_func arguments
    | "-" -> subtr_func arguments 
    | "*" -> mull_func  arguments
    | "/" -> ""
    | "%" -> ""
    | "define" -> ""
    | "print" -> ""
    | _ -> ""

    


let generation_pipeline ast = 
    initial_code                       ++
    emit_functions                     ++

    from_ast ast                       ++
    halt                               ++
    BuiltinSubroutines.all_subroutines ++
    final_code
