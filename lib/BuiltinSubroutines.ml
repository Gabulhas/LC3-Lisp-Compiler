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
    BINOMIAL FUNCTIONS
    Takes 2 values from the stack and performs a calculation
    Results are stored back into the top of the stack

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
    Changing the BR flag during runtime, to avoid creating multiple functions that do the same
    so, a > can be transformed into >= or a = 

    LD R1 N
    LD R0 TO_CHANGE
    AND R0 R0 R1
    ST R0 TO_CHANGE
    BR START
    -- Inverse masks, they remove that specific bit --
    N .FILL #63487
    Z .FILL #64511
    P .FILL #65023

    START 
    TO_CHANGE BR END


*)
(*
    Arguments:
    R0 - > flag
    R1 - = flag
    R3 - Values to Compare


    During Execution:
    R0 - Calculation Register
    R1 - Calculation Register
    R2 - Current Condition Result
    R3 - Values to Compare 

    Result:
    R2 is pushed into the stack


    Note, this uses some funky stuff to change the flag during runtime
    for example, is possible to change the flag to < and later to = during the execution

*)


(*Not working for = *)
let compare_routine = 
    
    let storage = 
        label  "R7_SAVE"   ++ fill 0 ++
        label  "FLAG_SAVE" ++ fill 0 ++
        label  "N_MASK"    ++ fill 63487 ++
        label  "Z_MASK"    ++ fill 64511 

    in
    let compare_start = 
        label "COMPARE_START"++
        st r7 "R7_SAVE" ++
        jsr "CHANGE_FLAG" ++
        jsr "ALL_TO_ZERO" ++
        label "COMPARE_LOOP" ++
            jsr "COMPARE_EXECUTE" ++
            addi r2 r2 (to_imm 0) ++
            brz "COMPARE_END_PERMATURE" ++
            addi r3 r3 (to_imm (-1)) ++
        brp "COMPARE_LOOP" ++

        jsr "COMPARE_FINISH"

    in
    let compare_end_permature =
        label "COMPARE_END_PERMATURE" ++
        (*This might be wrong*)
        negate r3 ++
        addr r6 r6 r3 ++
        zero r2 ++
        jsr "COMPARE_FINISH"

    in
    let change_flag = 
        label "CHANGE_FLAG"     ++
        ld r0 "TO_CHANGE"             ++
        st r0 "FLAG_SAVE"               ++
        addi r0 r0 (to_imm 0)           ++
        brnz "COMPARE_REMOVE_SMALLER"   ++
        label "COMPARE_CHECK_EQUAL"     ++
        addi r1 r1 (to_imm 0)           ++
        brnz "COMPARE_REMOVE_EQUAL"     ++
        label "COMPARE_END_CHANGE"      ++
        ret                             ++
        label "COMPARE_REMOVE_SMALLER"  ++
        ld r1 "N_MASK"                  ++
        ld r0 "TO_CHANGE"               ++
        andr r0 r0 r1                   ++
        st r0 "TO_CHANGE"               ++
        brnzp "COMPARE_REMOVE_EQUAL"    ++



        label "COMPARE_REMOVE_EQUAL"    ++
        ld r1 "Z_MASK"                  ++
        ld r0 "TO_CHANGE"               ++
        andr r0 r0 r1                   ++
        st r0 "TO_CHANGE"               ++
        brnzp "COMPARE_END_CHANGE"      

    in
    let all_to_zero =
        label "ALL_TO_ZERO" ++
        zero r0 ++
        zero r1 ++
        zero r2 ++
        zero r3 ++
        ret

    in
    let compare_execute =
        label "COMPARE_EXECUTE" ++
        stack_pull_into r1      ++
        stack_pull_into r0      ++
        negate r1               ++
        addr r0 r0 r1           ++
        label "TO_CHANGE"       ++
        brnzp "FALSE_RES"       ++
        brnzp "TRUE_RES"        ++

        label "FALSE_RES"       ++
        zero r2                 ++
        ret                     ++

        label "TRUE_RES"        ++
        set_val r2 (to_imm 1)   ++
        ret                     



    in
    let compare_finish = 
        label "COMPARE_FINISH"  ++
        ld r1 "FLAG_SAVE"       ++
        st r1 "TO_CHANGE"     ++
        stack_push_from r2      ++
        ld r7 "R7_SAVE"         ++
        ret
    in

    storage ++ compare_start ++ compare_end_permature ++ change_flag ++
    all_to_zero ++ compare_execute ++ compare_finish


(* 
   Since 
   (< 7 8 1) is equivalent to (> 1 8 7) then we only need a smaller than function
   (= 7 8 1) is similar to (< 7 8 1), only changes the "Is False" flag

*)


let all_subroutines =
    multiply_routine            ++ 
    add_routine                 ++
    subtr_routine               ++
    divide_routine              ++
    modulo_routine              ++
    compare_routine

