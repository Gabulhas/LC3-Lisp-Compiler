open Assembly



let initial_code:asm = 
    (*.ORIG x3000*)
    orig 12288 ++ 

    
    lea r7 "STACK"              ++




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
    

let generate_code _:asm =

    ret

let generation_pipeline ast = 
    initial_code        ++
    generate_code ast   ++
    final_code





