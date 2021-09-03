open Lexer

let mul a b = a * b


let number_to_int value = 
    match value with
    | Number a -> a
    | _ -> raise (Invalid_argument "Arithmetic Function not starting with number")

let rec optimize ast =
    match ast with
    | LList a -> (
            match (List.hd a) with
            | Symbol symbol -> get_single_number_if_possible symbol a
            | _ -> raise (Invalid_argument "S-Expression not starting with a symbol during optimization.")

    ) 
    | _ -> ast

and optimize_symbol symbol ast =
    let arguments = List.tl ast in
    match symbol with
    | "+" -> arithm_compact arguments (+) 0
    | "-" -> arithm_compact (List.tl arguments) (-) (List.hd arguments |> number_to_int)
    | "*" -> arithm_compact arguments mul 1
    | "/" -> arithm_compact (List.tl arguments) (/) (List.hd arguments |> number_to_int)
    | "%" -> arithm_compact arguments (mod) 1
    | _ -> arithm_compact arguments (mod) 0

and arithm_compact arguments func init_accum =
    let overevaluated = List.map optimize arguments in
    let rec calculate accum current_list result_list = 
        match current_list with
        | [] -> (Number accum):: result_list
        | Number a::tl -> calculate (func accum a) tl result_list
        | hd::tl -> calculate accum tl (hd::result_list)

    in 
    calculate init_accum overevaluated []

and get_single_number_if_possible symbol ast =
    let result = optimize_symbol symbol ast in
    match result with
    | Number a :: [] -> Number a
    | _ -> LList ((List.hd ast) :: result) 



