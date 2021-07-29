(* Not my code,
   check https://github.com/kanaka/mal/blob/master/impls/ocaml/reader.ml
   well, it's OSS :)
 *)
let token_regex = Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n{}('\"`,;)]*"
let tokenize text =
  List.map (function | Str.Delim x -> (String.trim x) | Str.Text _ -> "impossible!")
    (List.filter (function | Str.Delim x -> (if x = "" then false else true) | Str.Text _ -> false)
      (Str.full_split token_regex text));;
let rec printTokens = function | [] -> () | x::xs -> Printf.printf "[%s] " x; printTokens xs;

