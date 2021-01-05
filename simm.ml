open Asm
open Ir

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
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
  | Let((x, t), Mov(y), e) when (not (is_reg x)) && (not (is_reg y)) -> g env (replace x y e)
  | Let((x, t), Seti(i), e) ->
    if i = 0 then
      g env (replace x reg_zero e)
    else
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then
        Let((x, t), Seti(i), e')
      else
        e'
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function
  | Add(x, V(y)) when M.mem y env -> Add(x, C(M.find y env))
  | Add(x, V(y)) when M.mem x env -> Add(y, C(M.find x env))
  | Add(x, V(y)) when x = reg_zero -> Mov(y)
  | Add(x, V(y)) when y = reg_zero -> Mov(x)
  | Add(x, C(i)) when M.mem x env -> Seti(i + M.find x env)
  | Add(x, C(i)) when x = reg_zero -> Seti(i)
  | Sub(x, y) when M.mem y env ->
    let y' = M.find y env in
    if -2047 <= y' && y' <= 2047 then
      Add(x, C(-y'))
    else
      Sub(x, y)
  | Ld(x, V(y)) when M.mem y env -> Ld(x, C(M.find y env))
  | Ld(x, V(y)) when M.mem x env -> Ld(y, C(M.find x env))
  | Ld(x, C(i)) when M.mem x env && M.find x env + i < 2048 -> Ld(reg_zero, C(M.find x env + i))
  | St(x, y, V(z)) when M.mem z env -> St(x, y, C(M.find z env))
  | St(x, y, V(z)) when M.mem y env -> St(x, z, C(M.find y env))
  | St(x, y, C(i)) when M.mem y env && M.find y env + i < 2048 -> St(x, reg_zero, C(M.find y env + i))
  | LdF(x, V(y)) when M.mem y env -> LdF(x, C(M.find y env))
  | LdF(x, V(y)) when M.mem x env -> LdF(y, C(M.find x env))
  | LdF(x, C(i)) when M.mem x env && M.find x env + i < 2048 -> LdF(reg_zero, C(M.find x env + i))
  | StF(x, y, V(z)) when M.mem z env -> StF(x, y, C(M.find z env))
  | StF(x, y, V(z)) when M.mem y env -> StF(x, z, C(M.find y env))
  | StF(x, y, C(i)) when M.mem y env && M.find y env + i < 2048 -> StF(x, reg_zero, C(M.find y env + i))
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

let rec iter n e =
  if n = 1000 then e
  else
    let e' = g M.empty e in
    if e = e' then
      (if n > 0 then Format.eprintf "iterated in simm.ml@%d\n" n;
       e)
    else iter (n+1) e'

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = iter 0 e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, iter 0 e)
