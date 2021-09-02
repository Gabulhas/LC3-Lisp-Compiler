type asm = string
type imm = string

let to_imm (value:int): imm= 
    "#"^(string_of_int value)



let imm_to_asm (a:imm) :asm = a
let join_asm = String.concat " "
let join_asm_lines = String.concat "\n"

let (++) a b =
    a ^ "\n" ^ b

type reg = string

(*

R4 - Global (Data) Pointer
R5 - Frame Pointer (Scope Pointer)
R6 - Stack Pointer
R7 - Return Pointer

When a C function is called under this model, the function's parameters are pushed onto the stack right to left. Space is then made on the stack for the return value of the function being called, the address of the instruction in the caller to return to, and the caller's value of R5. Local variables in the function being called are pushed onto the stack in the order that they are declared. Note that the LC-3 does not have native PUSH and POP instructions, so addition and memory storage instructions must be used separately to implement the stack.


*)
let r0:reg = "R0" 
let r1:reg = "R1"
let r2:reg = "R2"
let r3:reg = "R3"
let r4:reg = "R4"
let r5:reg = "R5"
let r6:reg = "R6"
let r7:reg = "R7"



(*opcodes*)
let addr (dr:reg) (sr1:reg) (sr2:reg): asm =
    join_asm ["ADD"; dr; sr1; sr2]

let addi (dr:reg) (sr1:reg) (imm5:imm): asm =
    join_asm ["ADD"; dr; sr1; imm5]

let andr (dr:reg) (sr1:reg) (sr2:reg): asm =
    join_asm ["AND"; dr; sr1; sr2]

let andi (dr:reg) (sr1:reg) (imm5:imm): asm =
    join_asm ["AND"; dr; sr1; imm5]


let br c_flag offset_or_label :asm =
    let (n,z,p) = c_flag in
    let n_r = if n then "N" else "" in
    let z_r = if z then "Z" else "" in
    let p_r = if p then "P" else "" in
    let operand = "BR" ^n_r^z_r^p_r in
    join_asm [operand; offset_or_label]

let brn offset_or_label :asm =
    br (true, false, false) offset_or_label
let brz offset_or_label :asm =
    br (false, true, false) offset_or_label
let brp offset_or_label :asm =
    br (false, false, true) offset_or_label
let brnz offset_or_label :asm =
    br (true, true, false) offset_or_label
let brnp offset_or_label :asm =
    br (true, false, true) offset_or_label
let brzp offset_or_label :asm =
    br (false, true, true) offset_or_label
let brnzp offset_or_label :asm =
    br (true, true, true) offset_or_label

let jsr offset_or_label : asm =
    join_asm ["JSR"; offset_or_label]

let jmp (br:reg) : imm = 
    join_asm ["JMP"; br]

let ld (dr:reg) offset_or_label : asm =
    join_asm ["LD"; dr; offset_or_label]

let ldi (dr:reg) offset_or_label : asm =
    join_asm ["LDI"; dr; offset_or_label]

let ldr (dr:reg) (br:reg) offset_or_label : asm =
    join_asm ["LDR"; dr; br ;offset_or_label]

let lea (dr:reg) offset_or_label : asm =
    join_asm ["LEA"; dr; offset_or_label]

let not (dr:reg) (sr:reg) : asm =
    join_asm ["NOT"; dr; sr]

let ret : asm= "RET"

let rti : asm = "RTI"

let st (sr:reg) offset_or_label : asm =
    join_asm ["ST"; sr; offset_or_label]

let sti (sr:reg) offset_or_label : asm =
    join_asm ["STI"; sr; offset_or_label]

let str (sr:reg) (br:reg) offset_or_label : asm =
    join_asm ["STR"; sr; br;offset_or_label]

let trap vect : asm =
    join_asm ["TRAP"; vect]

let getc =   trap "x20"
let out =    trap "x21"
let puts =   trap "x22"
let in_trap =trap "x23"
let putsp =  trap "x24"
let halt =   trap "x25"

(*Directives*)
let label a :asm = a

let orig a :asm = join_asm [".ORIG"; to_imm a]
let endd :asm = ".END"
let fill a :asm = join_asm [".FILL"; to_imm a]
let fill_label label :asm = join_asm [".FILL"; label]
let blkw a :asm = join_asm [".BLKW"; to_imm a]
let stringz a :asm = join_asm [".STRINGZ"; "\"" ^ a ^ "\""]

(*Extras*)
let comment comm =
    Printf.sprintf ";--%s--" comm


let copy source destination =
    addi destination source (to_imm 0)

let zero register =
    andi register register (to_imm 0)

let negativate register =
    "\n"                                ++
    not register register               ++ 
    addi register register (to_imm 1)


let load_big_value value register = 
    let value_label = String.concat "" ["VALUE_"; string_of_int value] in
    label value_label           ++ 
    fill value                  ++
    ld register value_label

let load_small_value value register =
    zero register ++
    addi register register (to_imm value) 



let load_value value register=
    if value > 15 || value < (-16) then
        load_big_value value register
    else
        load_small_value value register
