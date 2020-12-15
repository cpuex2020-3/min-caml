type hex_string = string

type t =
  | Li of Id.t * int
  | La of Id.t * Id.l
  | Mv of Id.t * Id.t
  | Addi of Id.t * Id.t * int
  | Add of Id.t * Id.t * Id.t
  | Sub of Id.t * Id.t * Id.t
  | Slli of Id.t * Id.t * int
  | Srli of Id.t * Id.t * int
  | Lw of Id.t * int * Id.t
  | Sw of Id.t * int * Id.t
  | Fadd of Id.t * Id.t * Id.t
  | Fsub of Id.t * Id.t * Id.t
  | Fmul of Id.t * Id.t * Id.t
  | Fdiv of Id.t * Id.t * Id.t
  | Flw of Id.t * int * Id.t
  | Fsw of Id.t * int * Id.t
  | Fsgnj of Id.t * Id.t * Id.t
  | Fsgnjn of Id.t * Id.t * Id.t
  | Ret
  | Jalr of Id.t * Id.t * int
  | J of Id.l
  | Jal of Id.l
  | Label of Id.l
  | Beq of Id.t * Id.t * Id.l
  | Bne of Id.t * Id.t * Id.l
  | Blt of Id.t * Id.t * Id.l
  | Feq of Id.t * Id.t * Id.t
  | Fle of Id.t * Id.t * Id.t
  | Word of hex_string

type fundef = { label : Id.l; args : Id.t list; fargs : Id.t list; body : t list; ret : Type.t }
type prog = { data : t list; fundefs : fundef list; prog : t list }

val regs : Id.t array
val fregs : Id.t array
val allregs : Id.t list
val allfregs : Id.t list
val reg_cl : Id.t
val reg_ra : Id.t
val reg_buf : Id.t
val reg_sw : Id.t
val reg_fsw : Id.t
val reg_sp : Id.t
val reg_hp : Id.t
val reg_zero : Id.t
val is_reg : Id.t -> bool
val is_core : bool ref
