type asm = string
type imm = string

let to_imm (value:int): imm= 
    "#"^(string_of_int value)

let join_asm = String.concat " "

let (++) a b =
    a ^ "\n" ^ b

type reg = string

(*

R4 is used as a base register for loading and storing global data
R5 is used to point to the current function's area on the call stack
R6 is used as a stack pointer.
R7 is usually reserved for storage of return addresses from function calls; the JSR, JSRR, and TRAP instructions automatically store return addresses in this register during their execution.

When a C function is called under this model, the function's parameters are pushed onto the stack right to left. Space is then made on the stack for the return value of the function being called, the address of the instruction in the caller to return to, and the caller's value of R5. Local variables in the function being called are pushed onto the stack in the order that they are declared. Note that the LC-3 does not have native PUSH and POP instructions, so addition and memory storage instructions must be used separately to implement the stack.


*)





let r0:reg = "R0" 
let r1:reg = "R1"
let r2:reg = "R2"
let r3:reg = "R3"
let r4:reg = "R4" (*Base register for loading and storing global data*)
let r5:reg = "R5" (*Points to current function call memory*)
let r6:reg = "R6" (*Stack Pointer register*)
let r7:reg = "R7" (*Return Value register*)



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
let blkw a :asm = join_asm [".BLKW"; to_imm a]
let stringz a :asm = join_asm [".STRINGZ"; "\"" ^ a ^ "\""]
