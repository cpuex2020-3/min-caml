open Asm

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
  | Itof of Id.t
  | FMov of Id.t
  | FNeg of Id.t
  | FSqr of Id.t
  | Sqrt of Id.t
  | FAbs of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FSgnj of Id.t * Id.t
  | LdF of Id.t * id_or_imm
  | StF of Id.t * Id.t * id_or_imm
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
type prog = Prog of (Id.l * float) list * (Id.t * ConstExp.t) list * fundef list * t

let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Seti (_) | SetFi (_) | Restore(_) | SetL(_) -> []
  | Neg(x) | Mov(x) | Itof(x) | FSqr(x) | Sqrt(x) | FAbs(x) -> [x]
  | FMov(x) | FNeg(x) | Save(x, _) | Mul(x, _) | Div(x, _) -> [x]
  | Add(x, y') | Ld(x, y') | LdF(x, y') -> x :: fv_id_or_imm y'
  | St(x, y, z') | StF(x, y, z') -> x :: y :: fv_id_or_imm z'
  | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | FSgnj(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) | IfGE(x, y', e1, e2) ->
    x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) ->
    x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) -> fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let replace_id_or_imm from_reg to_reg = function
  | V(x) when x = from_reg -> V(to_reg)
  | e -> e

let rec replace from_reg to_reg = function
  | Ans(exp) -> Ans(replace' from_reg to_reg exp)
  | Let(xt, exp, e) -> Let(xt, replace' from_reg to_reg exp, replace from_reg to_reg e)
and replace' from_reg to_reg = function
  | Nop -> Nop
  | Seti(_) as e -> e
  | SetFi(_) as e -> e
  | SetL(_) as e -> e
  | Mov(x) as e -> if x = from_reg then Mov(to_reg) else e
  | Neg(x) as e ->
    if x = from_reg then
      if to_reg = reg_zero then Mov(reg_zero) else Neg(to_reg)
    else e
  | Add(x, V(y)) as e ->
    if x = from_reg && y = from_reg then
      if to_reg = reg_zero then Mov(reg_zero) else Mul(to_reg, 2)
    else if x = from_reg then
      if to_reg = reg_zero then Mov(y) else Add(to_reg, V(y))
    else if y = from_reg then
      if to_reg = reg_zero then Mov(x) else Add(x, V(to_reg))
    else e
  | Add(x, C(i)) as e ->
    if x = from_reg then
      if to_reg = reg_zero then Seti(i) else Add(to_reg, C(i))
    else e
  | Sub(x, y) as e ->
    if x = from_reg && y = from_reg then
      Mov(reg_zero)
    else if x = from_reg then
      if to_reg = reg_zero then Neg(y) else Sub(to_reg, y)
    else if y = from_reg then
      if to_reg = reg_zero then Mov(x) else Sub(x, to_reg)
    else e
  | Mul(x, y) as e ->
    if x = from_reg then
      if to_reg = reg_zero then Mov(reg_zero) else Mul(to_reg, y)
    else e
  | Div(x, y) as e ->
    if x = from_reg then
      if to_reg = reg_zero then Mov(reg_zero) else Div(to_reg, y)
    else e
  | Ld(x, y') ->
    if x = from_reg then
      Ld(to_reg, replace_id_or_imm from_reg to_reg y')
    else
      Ld(x, replace_id_or_imm from_reg to_reg y')
  | St(x, y, z') ->
    if x = from_reg && y = from_reg then
      St(to_reg, to_reg, replace_id_or_imm from_reg to_reg z')
    else if x = from_reg then
      St(to_reg, y, replace_id_or_imm from_reg to_reg z')
    else if y = from_reg then
      St(x, to_reg, replace_id_or_imm from_reg to_reg z')
    else
      St(x, y, replace_id_or_imm from_reg to_reg z')
  | Itof(x) as e ->
    if x = from_reg then
      if to_reg = reg_zero then Mov(reg_fzero) else Itof(to_reg)
    else e
  | FMov(x) as e -> if x = from_reg then FMov(to_reg) else e
  | FNeg(x) as e -> if x = from_reg then FNeg(to_reg) else e
  (* TODO: if fzero *)
  | FSqr(x) as e -> if x = from_reg then FSqr(to_reg) else e
  | Sqrt(x) as e -> if x = from_reg then Sqrt(to_reg) else e
  | FAbs(x) as e -> if x = from_reg then FAbs(to_reg) else e
  | FAdd(x, y) as e ->
    if x = from_reg && y = from_reg then
      if to_reg = reg_fzero then FMov(reg_fzero) else FAdd(to_reg, to_reg)
    else if x = from_reg then
      if to_reg = reg_fzero then FMov(reg_fzero) else FAdd(to_reg, y)
    else if y = from_reg then
      if to_reg = reg_fzero then FMov(reg_fzero) else FAdd(x, to_reg)
    else e
  | FSub(x, y) as e ->
    if x = from_reg && y = from_reg then
      if to_reg = reg_fzero then FMov(reg_fzero) else FSub(to_reg, to_reg)
    else if x = from_reg then
      if to_reg = reg_fzero then FNeg(y) else FSub(to_reg, y)
    else if y = from_reg then
      if to_reg = reg_fzero then FMov(x) else FSub(x, to_reg)
    else e
  | FMul(x, y) as e ->
    if to_reg = reg_fzero && (x = from_reg || y = from_reg) then
      FMov(reg_fzero)
    else
      (if x = from_reg && y = from_reg then FMul(to_reg, to_reg)
       else if x = from_reg then FMul(to_reg, y)
       else if y = from_reg then FMul(x, to_reg)
       else e)
  | FDiv(x, y) as e ->
    if x = from_reg && y = from_reg then FDiv(to_reg, to_reg)
    else if x = from_reg then FDiv(to_reg, y)
    else if y = from_reg then FDiv(x, to_reg)
    else e
  | FSgnj(x, y) as e ->
    if x = from_reg && y = from_reg then FSgnj(to_reg, to_reg)
    else if x = from_reg then FSgnj(to_reg, y)
    else if y = from_reg then FSgnj(x, to_reg)
    else e
  | LdF(x, y') ->
    if x = from_reg then
      LdF(to_reg, replace_id_or_imm from_reg to_reg y')
    else
      LdF(x, replace_id_or_imm from_reg to_reg y')
  | StF(x, y, z') ->
    if x = from_reg && y = from_reg then
      StF(to_reg, to_reg, replace_id_or_imm from_reg to_reg z')
    else if x = from_reg then
      StF(to_reg, y, replace_id_or_imm from_reg to_reg z')
    else if y = from_reg then
      StF(x, to_reg, replace_id_or_imm from_reg to_reg z')
    else
      StF(x, y, replace_id_or_imm from_reg to_reg z')
  | IfEq(x, y', e1, e2) ->
    if x = from_reg then
      IfEq(to_reg, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
    else
      IfEq(x, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
  | IfLE(x, y', e1, e2) ->
    if x = from_reg then
      IfLE(to_reg, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
    else
      IfLE(x, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
  | IfGE(x, y', e1, e2) ->
    if x = from_reg then
      IfGE(to_reg, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
    else
      IfGE(x, replace_id_or_imm from_reg to_reg y', replace from_reg to_reg e1, replace from_reg to_reg e2)
  | IfFEq(x, y, e1, e2) ->
    if x = from_reg && y = from_reg then
      (* TODO *)
      IfFEq(to_reg, to_reg, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else if x = from_reg then
      IfFEq(to_reg, y, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else if y = from_reg then
      IfFEq(x, to_reg, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else
      IfFEq(x, y, replace from_reg to_reg e1, replace from_reg to_reg e2)
  | IfFLE(x, y, e1, e2) ->
    if x = from_reg && y = from_reg then
      IfFLE(to_reg, to_reg, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else if x = from_reg then
      IfFLE(to_reg, y, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else if y = from_reg then
      IfFLE(x, to_reg, replace from_reg to_reg e1, replace from_reg to_reg e2)
    else
      IfFLE(x, y, replace from_reg to_reg e1, replace from_reg to_reg e2)
  | CallCls(_) -> raise (Failure "unhandled")
  | CallDir(l, iargs, fargs) ->
    let rep x = if x = from_reg then to_reg else x in
    CallDir(l, List.map rep iargs, List.map rep fargs)
  | Save(x, y) as e ->
    if x = from_reg && y = from_reg then
      Save(to_reg, to_reg)
    else if x = from_reg then
      Save(to_reg, y)
    else if y = from_reg then
      Save(x, to_reg)
    else
      e
  | Restore(x) as e -> if x = from_reg then Restore(to_reg) else e

let rec effect = function
  | Ans(exp) -> effect' exp
  | Let(_, exp, e) -> effect' exp || effect e
and effect' = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfGE(_, _, e1, e2) | IfFEq(_, _, e1, e2) | IfFLE(_, _, e1, e2) ->
    effect e1 || effect e2
  | St _ | StF _ | CallCls _ | CallDir _ | Save _ -> true
  | _ -> false
