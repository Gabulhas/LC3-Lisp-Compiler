module Lexer = 
    struct
        type atom = Number of int | Symbol of string | LString of string | Boolean of bool
        type sexpression = Atom of atom | LList of sexpression list | Expression of sexpression

        let atom_to_string = function 
                            | Number a -> Printf.sprintf "[NUMBER:%d]" a
                            | Symbol a -> Printf.sprintf "[SYMBOL:%s]" a
                            | LString a -> Printf.sprintf "[STRING:%s]" a
                            | Boolean a -> Printf.sprintf "[BOOL:%s]" (if a then "T" else "F")

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


        let rec printTree tree = 
            match tree with
            | Atom a -> Printf.printf "%s" (atom_to_string a)
            | LList a -> Printf.printf "\n(";List.iter printTree a; print_char ')'
            | Expression a -> printTree a

        let read_from_tokens t result =
            let rec aux tokens result = 
                if tokens != [] then Printf.printf "[%s]\n" (List.hd tokens) else print_char '\n';
                Printf.printf "->%d\n" (List.length result);
                match tokens with
                | [] -> ([] ,(match result with
                        | [] -> LList []
                        | a::[] -> Expression (a)
                        | _ -> LList result))
                | ")"::tl -> if result = [] then raise (Invalid_argument "Unexpected EOF")
                                            else (tl, LList (List.rev result)) 
                | "(" :: tl -> let (newtail, newresult) = aux tl [] in
                               aux newtail (newresult::result)
                | token :: tl -> aux tl (atomize token::result)
            in aux t []

    end


module Parser =
    struct
        (* Not my code,
           check https://github.com/kanaka/mal/blob/master/impls/ocaml/reader.ml
         *)
        let token_regex = Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n{}('\"`,;)]*"
        let tokenize text =
          List.map (function | Str.Delim x -> x | Str.Text x -> "impossible!")
            (List.filter (function | Str.Delim x -> (if x = "" then false else true) | Str.Text x -> false)
              (Str.full_split token_regex text));;
        let rec printTokens = function | [] -> () | x::xs -> Printf.printf "[%s] " x; printTokens xs;

    end


let () = 
    let lispExample = {|
        (
            (print (concat "hey" "yo man"))
            (define my_list (list 1 2 3 4 5))
            (print (+ 1 2 3 4 5))
            (+ 4 4)
            (if (> 5 4)
                5
                4
            )
        )
    |}

    in
    let parsed = Parser.tokenize lispExample
    in
    Parser.printTokens parsed

    
