open Assembly


(*Helpers*)

let stack_push_subroutine = 
    label "STACK_PUSH"          ++ 
    addi r6 r6 (to_imm (-1))       ++
    str r0 r6 (to_imm 0)        ++
    ret                          

let stack_pull_subroutine = 
    label "STACK_PULL"          ++ 
    ldr r0 r6 (to_imm 0)        ++
    addi r6 r6 (to_imm 1)    ++ 
    ret                         


let stack_push = 
    jsr "STACK_PUSH"

let stack_pull = 
    jsr "STACK_PULL"

let push_value value =
    load_value value r0       ++
    stack_push                  


let save_ret =
    copy r7 r5

let restore_ret = 
    copy r5 r7
(*

REMEMBER!!!!

If two nested routines use ret, R7 value must be saved


TODO:
    Maybe, just maybe, create an Fake Instructions that initializes
    "Funtions"/subroutines and also finished them

    idk, just a thought


    label "ADD_FUNC"            ++
    save_ret                    ++
    (-----------)
    (contenthere)
    (-----------)
    restore_ret                 ++
    ret

*)



(*
    add_func
    (+ a b c ....) 
        =
    (((0 + a) + b) + c) + ...)
    pulls from stack to r0
    moves value from r0 to r1
    pulls from stack to r0
    adds r0 and r1
    result is in the stack
*)
let add_routine =
    comment "--ADD_FUNC_START--"++
    label "ADD_FUNC"            ++
    save_ret                    ++
    stack_pull                  ++
    copy r0 r1                  ++
    stack_pull                  ++
    addr r0 r1 r0               ++
    stack_push                  ++
    restore_ret                 ++
    ret                         ++
    comment "--ADD_FUNC_END--"  

let subtr_routine =
    comment "--SUBTR_FUNC_START--"  ++
    label "SUBTR_FUNC"          ++
    save_ret                    ++
    stack_pull                  ++
    copy r0 r1                  ++
    stack_pull                  ++
    negativate r0               ++ 
    addr r0 r1 r0               ++
    stack_push                  ++
    restore_ret                 ++
    ret                         ++
    comment "--SUBTR_FUNC_END--" 


let multiply_routine =
    comment "--MUL_FUNC_START--"  ++
    label "MUL_FUNC"            ++
    save_ret                    ++
    stack_pull                  ++
    copy r0 r1                  ++
    stack_pull                  ++
    copy r0 r2                  ++
    zero r0                     ++

    label "MUL_FUNC_LOOP"       ++
        addr r0 r0 r2           ++
        addi r1 r1 (to_imm (-1))++
        brp "MUL_FUNC_LOOP"     ++


    stack_push                  ++

    restore_ret                 ++
    ret                         ++
    comment "--MUL_FUNC_END--"  


let divide_routine =
    comment "--DIV_FUNC_START--"++
    label "DIV_FUNC"            ++
    save_ret                    ++

    stack_pull                  ++
    copy r0 r1                  ++
    stack_pull                  ++
    copy r0 r2                  ++
    zero r0                     ++
    negativate r2               ++

    label "DIV_FUNC_LOOP"       ++
        addr r1 r1 r2           ++
        brn "DIV_FUNC_LOOP_END" ++ 
        addi r0 r0 (to_imm 1)   ++
        brp "DIV_FUNC_LOOP"     ++

    label "DIV_FUNC_LOOP_END"   ++
    stack_push                  ++
    restore_ret                 ++
    ret                         ++
    comment "--DIV_FUNC_END--"  

let modulo_routine =
    comment "--MODULO_FUNC_START--"++
    label "MODULO_FUNC"         ++
    save_ret                    ++

    stack_pull                  ++
    copy r0 r1                  ++
    stack_pull                  ++
    negativate r0               ++

    label "MODULO_FUNC_LOOP"    ++
        addr r1 r1 r0           ++
        brp "MODULO_FUNC_LOOP"  ++

    negativate r0               ++
    addr r1 r1 r0               ++
    copy r1 r0                  ++
    stack_push                  ++
    restore_ret                 ++
    ret                         ++ 
    comment "--MODULO_FUNC_END--"  


let all_subroutines =
    multiply_routine            ++ 
    add_routine                 ++
    subtr_routine               ++
    divide_routine              ++
    modulo_routine


