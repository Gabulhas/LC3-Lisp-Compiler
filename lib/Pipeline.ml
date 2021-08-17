let pipeline text = 
    let parsed = Parser.tokenize text in
    let lexed = Lexer.read_from_tokens parsed in
    print_string "---------- TREE --------";
    Lexer.print_sexpression lexed;
    print_string "\n---------- END TREE -------- \n";
    let resultLines = CodeGeneration.generation_pipeline lexed in
    print_string "---------- ASM --------\n";
    print_string resultLines;
    print_string "\n---------- END ASM -------- \n";
