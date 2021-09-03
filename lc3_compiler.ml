open Lib

let compile_from_file filename = 
        let loadedFile = Utils.load_file filename in 
        Pipeline.pipeline loadedFile

let () = 
    if Array.length Sys.argv == 1 then raise (Invalid_argument "Please Input the Name of the file to compile") else
        compile_from_file (Sys.argv.(1))

    
