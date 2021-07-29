(*TODO remove modules from module files*)
open Lib
module CodeGeneration =
    struct

    end


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)

let () = 
    if Array.length Sys.argv == 1 then raise (Invalid_argument "Please Input the Name of the file to compile") else
    begin
        let loadedFile = load_file (Sys.argv.(1))
        in
        let parsed = Lib.Parser.tokenize loadedFile in
        let lexed = Lib.Lexer.read_from_tokens parsed in
        Lexer.printTree lexed

    end

    
