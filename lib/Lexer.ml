type atom = Number of int | Symbol of string | LString of string | Boolean of bool
type sexpression = Atom of atom | LList of sexpression list

let atom_to_string = function 
                    | Number a -> Printf.sprintf "[NUMBER:%d]" a
                    | Symbol a -> Printf.sprintf "[SYMBOL:%s]" a
                    | LString a -> Printf.sprintf "[STRING:%s]" a
                    | Boolean a -> Printf.sprintf "[BOOL:%s]" (if a then "T" else "F")

let rec printTree tree = 
    match tree with
    | Atom a -> Printf.printf "%s" (atom_to_string a)
    | LList a -> Printf.printf "\n(";List.iter printTree a; print_char ')'

let find_atom_type token = 
    match token with
        | "T" -> Boolean true
        | "F" -> Boolean false
        | token when token.[0] = '"' && token.[(String.length token ) - 1] = '"' -> LString token
        | _ -> let optionValue = int_of_string_opt token in
               match optionValue with
               | None -> Symbol token
               | Some number -> Number number

let atomize token  = 
    Atom (find_atom_type token)

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

