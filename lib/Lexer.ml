type sexpression = Number of int | Symbol of string | LString of string | Boolean of bool | LList of sexpression list

let rec print_sexpression = function 
                    | Number a -> Printf.printf "[NUMBER:%d]" a
                    | Symbol a -> Printf.printf "[SYMBOL:%s]" a
                    | LString a -> Printf.printf "[STRING:%s]" a
                    | Boolean a -> Printf.printf "[BOOL:%s]" (if a then "T" else "F")
                    | LList a -> Printf.printf "\n(";List.iter print_sexpression a; print_char ')'

let atomize token = 
    match token with
        | "T" -> Boolean true
        | "F" -> Boolean false
        | token when token.[0] = '"' && token.[(String.length token ) - 1] = '"' -> LString token
        | _ -> let optionValue = int_of_string_opt token in
               match optionValue with
               | None -> Symbol token
               | Some number -> Number number


(* Hello? Is this from the garbage department?
   Fix this omg


   Just in case i forget:
       tokens contains the leftover tokens to read
       result is the result tree
       lpared_look is the ammount of parentheses to look for, 
            if it reaches the end of the token list and is still looking for a rparen, then it's Unexpected EOF
            for example: `(Print`
            if it finds a rparen while not looking for it, then it's Unexpected EOF
            for example: `Print)`
 *)

let read_from_tokens t =
    let rec aux tokens result lpared_look= 
        match tokens with
        | [] -> if  lpared_look = 0 then
            ([] ,(match result with
                | [] -> LList []
                | a::[] -> a
                | _ -> LList result), 0)
        else raise (Invalid_argument "Unexpected EOF")
        | ")"::tl -> if lpared_look < 1 then raise (Invalid_argument "Unexpected )")
                                    else (tl, LList (List.rev result), lpared_look - 1) 
        | "(" :: tl -> let (newtail, newresult, newparenlook) = aux tl [] (lpared_look + 1) in
                       aux newtail (newresult::result) newparenlook
        | token :: tl -> aux tl (atomize token::result) lpared_look
    in 
    let (_, result, _) = aux t [] 0 in
    result

