let debug = false


let pipeline text = 

    let parsed = Parser.tokenize text in
    let lexed = Lexer.read_from_tokens parsed in
    let resultLines = CodeGeneration.generation_pipeline lexed in

    if debug then 
        begin
            print_string "---------- TREE --------";
            Lexer.print_sexpression lexed;
            print_string "\n---------- END TREE -------- \n";
            print_string "---------- ASM --------\n";
            print_string resultLines;
            print_string "\n---------- END ASM -------- \n";
        end 
    else
        print_string resultLines;
