open Asm
open Ir

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

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Seti(i), e) ->
    if i = 0 then
      g env (replace x reg_zero e)
    else
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then
        Let((x, t), Seti(i), e')
      else
        e'
  | Let(xt, Mul(y, i), e) when M.mem y env -> (* for array access *)
    if i = 2 then
      g env (Let(xt, Seti((M.find y env) lsl 1), e))
    else if i = 4 then
      g env (Let(xt, Seti((M.find y env) lsl 2), e))
    else
      raise (Failure "unhandled mul.")
  | Let(xt, Div(y, i), e) when M.mem y env -> (* for array access *)
    if i = 2 then
      g env (Let(xt, Seti((M.find y env) lsr 1), e))
    else if i = 4 then
      g env (Let(xt, Seti((M.find y env) lsr 2), e))
    else
      raise (Failure "unhandled div.")
  | Let((x, t), Mov(y), e) when List.mem y const_regs -> g env (replace x y e)
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function
  (* TODO: Itof *)
  | Seti(i) when i = 0 -> Mov(reg_zero)
  | Add(x, V(y)) when M.mem y env -> Add(x, C(M.find y env))
  | Add(x, V(y)) when M.mem x env -> Add(y, C(M.find x env))
  | Add(x, V(y)) when y = reg_zero -> Nop
  | Add(x, C(i)) when i = 0 -> Nop
  | Ld(x, V(y)) when M.mem y env -> Ld(x, C(M.find y env))
  | St(x, y, V(z)) when M.mem z env -> St(x, y, C(M.find z env))
  | LdF(x, V(y)) when M.mem y env -> LdF(x, C(M.find y env))
  | StF(x, y, V(z)) when M.mem z env -> StF(x, y, C(M.find z env))
  | IfEq(x, V(y), e1, e2) when M.mem y env -> IfEq(x, C(M.find y env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem y env -> IfLE(x, C(M.find y env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem y env -> IfGE(x, C(M.find y env), g env e1, g env e2)
  | IfEq(x, V(y), e1, e2) when M.mem x env -> IfEq(y, C(M.find x env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem x env -> IfGE(y, C(M.find x env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem x env -> IfLE(y, C(M.find x env), g env e1, g env e2)
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env e1, g env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env e1, g env e2)
  | IfGE(x, y', e1, e2) -> IfGE(x, y', g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, g M.empty e)
