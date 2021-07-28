module Lexer = 
    struct
        type atom = Number of int | Symbol of string | LString of string
        type sexpression = Atom of atom | LList of sexpression list
        type exp = SExpression of sexpression | Atom of atom
            
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

    
