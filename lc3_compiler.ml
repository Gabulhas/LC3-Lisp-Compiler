open Lib

let () = 
    if Array.length Sys.argv == 1 then raise (Invalid_argument "Please Input the Name of the file to compile") else
    begin
        let loadedFile = Utils.load_file (Sys.argv.(1)) in 
        Pipeline.pipeline loadedFile
    end

    
