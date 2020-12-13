open Asm
open ConstExp
open Ir

let pre_count_stack_set = ref S.empty
let stackset = ref S.empty
let stackmap = ref []
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
       if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
     stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x =
  match locate x with
  | hd :: tl -> 4 * hd
  | [] -> raise (Failure (Printf.sprintf "No such variable %s in stackset." x))
let stacksize () = (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> string_of_int i

let rec shuffle sw xys =
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] ->
    (y, sw) :: (x, y) :: shuffle sw
      (List.map
         (function
           | (y', z) when y = y' -> (sw, z)
           | yz -> yz)
         xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t

let rec g = function
  | dest, Ans(exp) -> g' (dest, exp)
  | dest, Let((x, t), exp, e) ->
    let elem = g' (NonTail(x), exp) in
    let rest = g (dest, e) in
    elem @ rest (* care about the evaluation order. *)
and g' = function
  | NonTail(_), Nop -> []
  | NonTail(x), Seti(i) -> [Li(x, i)]
  | NonTail(x), SetFi(l) -> [La(reg_buf, l); Flw(x, 0, reg_buf)]
  | NonTail(x), SetL(l) -> [La(x, l)]
  | NonTail(x), Mov(y) -> if x <> y then [Mv(x, y)] else []
  | NonTail(x), Neg(y) -> if x <> y then [Mv(x, y); Sub(x, reg_zero, x)] else [Sub(x, reg_zero, x)]
  | NonTail(x), Add(y, V(z)) -> [Add(x, y, z)]
  | NonTail(x), Add(y, C(i)) ->
    if i > 2047 then
      [Li(reg_buf, i); Add(x, y, reg_buf)]
    else
      [Addi(x, y, i)]
  | NonTail(x), Sub(y, z) -> [Sub(x, y, z)]
  | NonTail(x), Mul(y, i) ->
    let o = if i = 2 then 1 else if i = 4 then 2 else raise (Failure "Unhandled multiplier") in
    [Slli(x, y, o)]
  | NonTail(x), Div(y, i) ->
    let o = if i = 2 then 1 else if i = 4 then 2 else raise (Failure "Unhandled divider") in
    [Srli(x, y, o)]
  | NonTail(x), Ld(y, C(i)) -> [Lw(x, i, y)]
  | NonTail(x), Ld(y, V(z)) -> [Add(reg_buf, y, z); Lw(x, 0, reg_buf)]
  | NonTail(_), St(x, y, C(i)) -> [Sw(x, i, y)]
  | NonTail(_), St(x, y, V(z)) -> [Add(reg_buf, y, z); Sw(x, 0, reg_buf)]
  | NonTail(x), FMov(y) -> if x <> y then (assert (List.mem x allfregs); [Fsgnj(x, y, y)]) else []
  | NonTail(x), FNeg(y) -> [Fsgnjn(x, y, y)]
  | NonTail(x), FAdd(y, z) -> [Fadd(x, y, z)]
  | NonTail(x), FSub(y, z) -> [Fsub(x, y, z)]
  | NonTail(x), FMul(y, z) -> [Fmul(x, y, z)]
  | NonTail(x), FDiv(y, z) -> [Fdiv(x, y, z)]
  | NonTail(x), LdF(y, V(z)) -> [Add(reg_buf, y, z); Flw(x, 0, reg_buf)]
  | NonTail(x), LdF(y, C(i)) -> [Flw(x, i, y)]
  | NonTail(_), StF(x, y, V(z)) -> [Add(reg_buf, y, z); Fsw(x, 0, reg_buf)]
  | NonTail(_), StF(x, y, C(i)) -> [Fsw(x, i, y)]
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    [Sw(x, offset y, reg_sp)]
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    [Fsw(x, offset y, reg_sp)]
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); []
  | NonTail(x), Restore(y) when List.mem x allregs ->
    if M.mem y !Virtual.globenv then
      [La(x, Id.L(y))]
    else if y = reg_zero then
      [Mv(x, reg_zero)]
    else
      [Lw(x, offset y, reg_sp)]
  | NonTail(x), Restore(y) -> assert (List.mem x allfregs); [Flw(x, offset y, reg_sp)]
  | Tail, (Nop | St _ | StF _ | Save _ as exp) ->
    g' (NonTail(Id.gentmp Type.Unit), exp) @ [Ret]
  | Tail, (Seti _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ | Mul _ | Div _ as exp) ->
    g' (NonTail(regs.(0)), exp) @ [Ret]
  | Tail, (SetFi _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | LdF _  as exp) ->
    g' (NonTail(fregs.(0)), exp) @ [Ret]
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> g' (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> g' (NonTail(fregs.(0)), exp)
     | _ -> assert false)
    @ [Ret]
  | Tail, IfEq(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if x y e1 e2 "bne"
     | C(i) -> Li(reg_buf, i) :: g'_tail_if x reg_buf e1 e2 "bne")
  | Tail, IfLE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if y x e1 e2 "blt"
     | C(i) -> Li(reg_buf, i) :: g'_tail_if reg_buf x e1 e2 "blt")
  | Tail, IfGE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if x y e1 e2 "blt"
     | C(i) -> Li(reg_buf, i) :: g'_tail_if x reg_buf e1 e2 "blt")
  | Tail, IfFEq(x, y, e1, e2) -> g'_tail_float_if x y e1 e2 "feq.s"
  | Tail, IfFLE(x, y, e1, e2) -> g'_tail_float_if x y e1 e2 "fle.s"
  | NonTail(z), IfEq(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if (NonTail(z)) x y e1 e2 "bne"
     | C(i) -> Li(reg_buf, i) :: g'_non_tail_if (NonTail(z)) x reg_buf e1 e2 "bne")
  | NonTail(z), IfLE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if (NonTail(z)) y x e1 e2 "blt"
     | C(i) -> Li(reg_buf, i) :: g'_non_tail_if (NonTail(z)) reg_buf x e1 e2 "blt")
  | NonTail(z), IfGE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if (NonTail(z)) x y e1 e2 "blt"
     | C(i) -> Li(reg_buf, i) :: g'_non_tail_if (NonTail(z)) x reg_buf e1 e2 "blt")
  | NonTail(z), IfFEq(x, y, e1, e2) -> g'_non_tail_float_if (NonTail(z)) x y e1 e2 "feq.s"
  | NonTail(z), IfFLE(x, y, e1, e2) -> g'_non_tail_float_if (NonTail(z)) x y e1 e2 "fle.s"
  | Tail, CallCls(x, ys, zs) ->
    (g'_args [(x, reg_cl)] ys zs) @ [Lw(reg_buf, 0, reg_cl); Jalr(reg_zero, reg_buf, 0)]
  | Tail, CallDir(Id.L(x), ys, zs) -> (g'_args [] ys zs) @ [J(Id.L(x))]
  | NonTail(a), CallCls(x, ys, zs) ->
    let args = g'_args [(x, reg_cl)] ys zs in
    let ss = stacksize () in
    let jalr = [Sw(reg_ra, ss, reg_sp); Lw(reg_buf, 0, reg_cl); Addi(reg_sp, reg_sp, ss + 4);
                Jalr(reg_ra, reg_buf, 0); Addi(reg_sp, reg_sp, -(ss + 4)); Lw(reg_ra, ss, reg_sp)] in
    let epilog = if List.mem a allregs && a <> regs.(0) then
        [Mv(a, regs.(0))]
      else if List.mem a allfregs && a <> fregs.(0) then
        [Fsgnj(a, fregs.(0), fregs.(0))]
      else
        [] in
    args @ jalr @ epilog
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    let args = g'_args [] ys zs in
    let ss = stacksize () in
    let jal = [Sw(reg_ra, ss, reg_sp);
               Addi(reg_sp, reg_sp, ss + 4);
               Jal(Id.L(x));
               Addi(reg_sp, reg_sp, -(ss + 4));
               Lw(reg_ra, ss, reg_sp)] in
    let epilog = if List.mem a allregs && a <> regs.(0) then
        [Mv(a, regs.(0))]
      else if List.mem a allfregs && a <> fregs.(0) then
        [Fsgnj(a, fregs.(0), fregs.(0))]
      else
        [] in
    args @ jal @ epilog
and g'_tail_if x y e1 e2 bn =
  let b_else = Id.genid (bn ^ "_stands") in
  let stackset_back = !stackset in
  let stands = (if bn = "bne" then
                  Bne(x, y, Id.L(b_else))
                else if bn = "blt" then
                  Blt(x, y, Id.L(b_else))
                else
                  raise (Failure "unhandled in g'_tail_if")) ::
               g (Tail, e1) in
  stackset := stackset_back;
  let els = Label(Id.L(b_else)) :: g (Tail, e2) in
  stands @ els
and g'_tail_float_if x y e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  let stackset_back = !stackset in
  let stands = (if b = "feq.s" then
                  Feq(reg_buf, x, y)
                else if b = "fle.s" then
                  Fle(reg_buf, x, y)
                else
                  raise (Failure "unhandled in g'_tail_float_if")) ::
               Beq(reg_buf, reg_zero, Id.L(b_else)) ::
               g (Tail, e1) in
  stackset := stackset_back;
  let els = Label(Id.L(b_else)) :: g (Tail, e2) in
  stands @ els
and g'_non_tail_if dest x y e1 e2 bn =
  let b_else = Id.genid (bn ^ "_stands") in
  let b_cont = Id.genid (bn ^ "_cont") in
  let stackset_back = !stackset in
  let stands = (if bn = "bne" then
                  Bne(x, y, Id.L(b_else))
                else if bn = "blt" then
                  Blt(x, y, Id.L(b_else))
                else
                  raise (Failure "unhandled in g'_non_tail_if")) ::
               g (dest, e1) @
               [J(Id.L(b_cont))] in
  let stackset1 = !stackset in
  stackset := stackset_back;
  let els = Label(Id.L(b_else)) :: g (dest, e2) in
  let cont = [Label(Id.L(b_cont))] in
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2;
  stands @ els @ cont
and g'_non_tail_float_if dest x y e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  let stackset_back = !stackset in
  let stands = (if b = "feq.s" then
                  Feq(reg_buf, x, y)
                else if b = "fle.s" then
                  Fle(reg_buf, x, y)
                else
                  raise (Failure "unhandled in g'_non_tail_float_if")) ::
               Beq(reg_buf, reg_zero, Id.L(b_else)) ::
               g (dest, e1) @ [J(Id.L(b_cont))] in
  let stackset1 = !stackset in
  stackset := stackset_back;
  let els = Label(Id.L(b_else)) :: g (dest, e2) in
  let cont = [Label(Id.L(b_cont))] in
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2;
  stands @ els @ cont
and g'_args x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  let int_args = List.map
      (fun (y, r) -> Mv(r, y))
      (shuffle reg_sw yrs) in
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  let float_args = List.map
      (fun (z, fr) -> Fsgnj(fr, z, z))
      (shuffle reg_fsw zfrs) in
  int_args @ float_args

let h { name = Id.L(x); args = args; fargs = fargs; body = e; ret = ret } =
  stackset := S.empty;
  stackmap := [];
  { label = Id.L(x); args = args; fargs = fargs; body = g (Tail, e); ret = ret }

let f (Prog(float_data, array_data, fundefs, e)) =
  let fundefs = List.map (fun fundef -> h fundef) fundefs in
  stackset := S.empty;
  stackmap := [];
  let programs = g (NonTail(regs.(0)), e) in
  { floats = float_data; globals = array_data; fundefs = fundefs; programs = programs }
