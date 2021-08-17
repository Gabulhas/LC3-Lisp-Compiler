open Assembly

let initial_code:asm = 
    (*.ORIG x3000*)
    orig 12288 ++ 
    lea r7 "STACK"

(*CHANGE THIS TO IT'S CORRECT REGISTERS*)
let final_code =
    label "PUSH"                ++ 
    addi r1 r1 (to_imm 1)       ++
    str r0 r1 (to_imm 1)        ++
    ret                         ++

    label "PULL"                ++ 
    ldr r0 r1 (to_imm (-1))     ++
    addi r1 r1 (to_imm (-1))    ++ 
    ret


let generation_pipeline _ = 
    initial_code        ++
    (*from_ast ast   ++*)
    final_code
    

    (*
let rec generate_from_sexpression llist accum =
    match llist with 
    | hd::tl -> let generator = get_generator hd
    | _ -> raise (Invalid_argument "Empty s-expression")

let from_ast ast =
    let from_ast_walk cast accum =
        match ast with
        | LList(llist) -> generate_from_sexpression llist accum

        (*Should it always be a *)
        | _ -> raise (Invalid_argument "CHANGE THIS")
    in
    from_ast_walk ast []

    *)
