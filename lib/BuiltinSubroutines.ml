open Assembly


(*Helpers*)

let stack_push_from r =
    addi r6 r6 (to_imm (-1))      ++
    str r r6 (to_imm 0)        

let stack_pull_into r =
    ldr r r6 (to_imm 0)        ++
    addi r6 r6 (to_imm 1)    

let stack_push_subroutine = 
    label "STACK_PUSH"          ++ 
    stack_push_from r0               ++
    ret                          

let stack_pull_subroutine = 
    label "STACK_PULL"          ++ 
    stack_pull_into r0               ++
    ret                         


let inc_stack_pointer =
    addi r6 r6 (to_imm 1)

let dec_stack_pointer =
    addi r6 r6 (to_imm (-1))


let stack_push_call = 
    jsr "STACK_PUSH"

let stack_pull_call = 
    jsr "STACK_PULL"

let push_value value =
    load_value value r0       ++
    stack_push_call                  



(*THIS IS WRONG!!!*)

let save_ret =
    addi r6 r6 (to_imm (-1))    ++
    str r7 r6 (to_imm 0)       

let return_val =
    ldr r7 r6 (to_imm 0)        ++
    str r0 r6 (to_imm 0)        ++
    ret
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
  (a + b) or (a - b)
  a -> R1   
  b -> R0
  R0 <- R1 +/- R0

*)

let add_routine =
    comment "--ADD_FUNC_START--"        ++
    label "ADD_FUNC"                    ++
    stack_pull_into r1                  ++
    stack_pull_into r0                  ++
    addr r0 r0 r1                       ++
    stack_push_from r0                  ++
    ret                                 ++
    comment "--ADD_FUNC_END--"  

let subtr_routine =
    comment "--SUBTR_FUNC_START--"      ++
    label "SUBTR_FUNC"                  ++
    stack_pull_into r1                  ++
    stack_pull_into r0                  ++
    negate r1                           ++
    addr r0 r0 r1                       ++
    stack_push_from r0                  ++
    ret                                 ++
    comment "--SUBTR_FUNC_END--" 



(*
    (a * b)
    r0 <- 0 accumulator
    r1 <- b counter
    r2 <- a 

*)

let multiply_routine =
    comment "--MUL_FUNC_START--"        ++
    label "MUL_FUNC"                    ++
    save_ret                            ++
    stack_pull_into r1                  ++
    stack_pull_into r2                  ++
    zero r0                             ++

    label "MUL_FUNC_LOOP"               ++
        addr r0 r0 r2                   ++
        addi r1 r1 (to_imm (-1))        ++
        brp "MUL_FUNC_LOOP"             ++

    stack_push_from r0                  ++
    ret                                 ++
    comment "--MUL_FUNC_END--"  





let divide_routine =
    comment "--DIV_FUNC_START--"++
    label "DIV_FUNC"            ++
    stack_pull_into r1          ++
    stack_pull_into r2          ++
    zero r0                     ++
    negate r2                   ++

    label "DIV_FUNC_LOOP"       ++
        addr r1 r1 r2           ++
        brn "DIV_FUNC_LOOP_END" ++ 
        addi r0 r0 (to_imm 1)   ++
        brp "DIV_FUNC_LOOP"     ++

    label "DIV_FUNC_LOOP_END"   ++
    stack_push_from r0          ++
    ret                         ++
    comment "--DIV_FUNC_END--"  



let modulo_routine =
    comment "--MODULO_FUNC_START--"     ++
    label "MODULO_FUNC"                 ++
    stack_pull_into r1                  ++
    stack_pull_into r0                  ++
    negate r0                           ++

    label "MODULO_FUNC_LOOP"            ++
        addr r1 r1 r0                   ++
        brp "MODULO_FUNC_LOOP"          ++

    negate r0                           ++
    addr r1 r1 r0                       ++
    copy r1 r0                          ++
    stack_push_from r0                  ++
    ret                                 ++
    comment "--MODULO_FUNC_END--"  



(* 
   Since 
   (< 7 8 1) is equivalent to (> 1 8 7) then we only need a greater than function

    let smaller = 
*)


let all_subroutines =
    multiply_routine            ++ 
    add_routine                 ++
    subtr_routine               ++
    divide_routine              ++
    modulo_routine

