type id_or_imm = V of Id.t | C of int
type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Nop
  | Seti of int
  | SetFi of Id.l
  | SetL of Id.l
  | Mov of Id.t
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * Id.t
  | Mul of Id.t * int
  | Div of Id.t * int
  (* TODO: handle with id instead of id_or_imm. handling in emit can cause error. e.g. register dependencies *)
  | Ld of Id.t * id_or_imm
  | St of Id.t * Id.t * id_or_imm
  | FMov of Id.t
  | FNeg of Id.t
  | FAdd of  Id.t * Id.t
  | FSub of  Id.t * Id.t
  | FMul of  Id.t * Id.t
  | FDiv of  Id.t * Id.t
  | LdF of Id.t * id_or_imm
  | StF of Id.t * Id.t * id_or_imm
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
type prog = Prog of (Id.l * float) list * fundef list * t

let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11" |]
let fregs = [| "fa0"; "fa1"; "fa2"; "fa3"; "fa4"; "fa5"; "fa6"; "fa7"; "fs0"; "fs1"; "fs2"; "fs3"; "fs4"; "fs5"; "fs6"; "fs7"; "fs8"; "fs9"; "fs10"; "fs11" |]
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
let reg_buf = "t2" (* register which is usable in emit.ml. TODO: better fix. *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
let reg_sp = "s0" (* stack pointer *)
let reg_ra = "ra" (* return address *)
let reg_hp = "t0" (* TODO: consider changing this to `min_caml_hp` and store dynamically.(See Notability for more info) *)
let is_reg x = List.mem x allregs || List.mem x allfregs || x = reg_sp || x = reg_ra || x = reg_hp

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Seti (_) | SetFi (_) | Restore(_) | SetL(_) -> []
  (*| Comment(_) -> []*)
  | Neg(x) | Mov(x) -> [x]
  | FMov(x) | FNeg(x) | Save(x, _) | Mul(x, _) | Div(x, _) -> [x]
  | Add(x, y') | Ld(x, y') | LdF(x, y') -> x :: fv_id_or_imm y'
  | Sub(x, y) -> x :: [y]
  | St(x, y, z') | StF(x, y, z') -> x :: y :: fv_id_or_imm z'
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) -> x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
    fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)
