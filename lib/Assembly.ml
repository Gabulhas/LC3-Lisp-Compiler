type asm = string
type imm = string


let to_imm (value:int): imm= 
    "#"^(string_of_int value)


let (++) a b =
    a ^ "\n" ^ b

type reg = string
let r0:reg = "R0"
let r1:reg = "R1"
let r2:reg = "R2"
let r3:reg = "R3"
let r4:reg = "R4"
let r5:reg = "R5"
let r6:reg = "R6"
let r7:reg = "R7"




let join_asm = String.concat " "


let add_regs (dr:reg) (sr1:reg) (sr2:reg): asm =
    join_asm ["ADD"; dr; sr1; sr2]

let add_imm (dr:reg) (sr1:reg) (imm5:imm): asm =
    join_asm ["ADD"; dr; sr1; imm5]

let and_regs (dr:reg) (sr1:reg) (sr2:reg): asm =
    join_asm ["AND"; dr; sr1; sr2]

let and_imm (dr:reg) (sr1:reg) (imm5:imm): asm =
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
    join_asm ["LDI"; dr; br ;offset_or_label]

let lea (dr:reg) offset_or_label : asm =
    join_asm ["LDI"; dr; offset_or_label]

let not (dr:reg) (sr:reg) : asm =
    join_asm ["NOT"; dr; sr]

let ret = "RET"

let rti = "RTI"

let st (sr:reg) offset_or_label : asm =
    join_asm ["ST"; sr; offset_or_label]

let sti (sr:reg) offset_or_label : asm =
    join_asm ["STI"; sr; offset_or_label]

let str (sr:reg) (br:reg) offset_or_label : asm =
    join_asm ["STI"; sr; br;offset_or_label]

let trap vect : asm =
    join_asm ["TRAP"; vect]

let getc =   trap "x20"
let out =    trap "x21"
let puts =   trap "x22"
let in_trap =trap "x23"
let putsp =  trap "x24"
let halt =   trap "x25"


