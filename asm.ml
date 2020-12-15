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

let regs = [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"; "t3"; "t4"; "t5"; "t6" |]
let fregs = [| "fa0"; "fa1"; "fa2"; "fa3"; "fa4"; "fa5"; "fa6"; "fa7"; "fs0"; "fs1"; "fs2"; "fs3"; "fs4"; "fs5"; "fs6"; "fs7"; "fs8"; "fs9"; "fs10"; "fs11"; "ft0"; "ft1"; "ft2"; "ft3"; "ft4"; "ft5"; "ft6"; "ft7"; "ft8"; "ft9"; "ft10"; "ft11" |]
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
let reg_buf = "t2" (* register which is usable in emit.ml. TODO: better fix. *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
let reg_sp = "s0" (* stack pointer *)
let reg_ra = "ra" (* return address *)
let reg_hp = "t0" (* TODO: consider changing this to `min_caml_hp` and store dynamically.(See Notability for more info) *)
let reg_zero = "zero"
let is_reg x = List.mem x allregs || List.mem x allfregs || List.mem x [reg_sp; reg_ra; reg_hp; reg_zero]
let is_core = ref false
