open Asm
open Ir

let rec flatten_let xt cont = function
  | Ans(exp) -> Let(xt, exp, cont)
  | Let(xt', e1', e2') -> Let(xt', e1', flatten_let xt cont e2')

let rec g env const_env = function
  | Ans(exp) -> Ans(g' env const_env exp)
  | Let((x, t), Seti(i), e) ->
    (* Format.eprintf "found simm %s = %d@." x i; *)
    let (env, const_env) = if i = 0 then
        (env, M.add x reg_zero const_env)
      else
        (M.add x i env, const_env) in
    let e' = g env const_env e in
    if List.mem x (fv e') then
      Let((x, t), Seti(i), e')
    else
      ((* Format.eprintf "erased redundant Set to %s@." x; *)
        e')
  | Let((x, t), Mov(y), e) when M.mem y env ->
    let yi = M.find y env in
    let e' = g (M.add x yi env) const_env e in
    if List.mem x (fv e') then
      Let((x, t), Seti(yi), e')
    else
      e'
  | Let(xt, Mul(y, i), e) when M.mem y env -> (* for array access *)
    (* Format.eprintf "erased redundant Slw on %s@." x; *)
    (match i with
     | 2 -> g env const_env (Let(xt, Seti((M.find y env) lsl 1), e))
     | 4 -> g env const_env (Let(xt, Seti((M.find y env) lsl 2), e))
     | _ -> raise (Failure "unhandled mul."))
  | Let(xt, Div(y, i), e) when M.mem y env -> (* for array access *)
    (* Format.eprintf "erased redundant Slw on %s@." x; *)
    (match i with
     | 2 -> g env const_env (Let(xt, Seti((M.find y env) lsr 1), e))
     | 4 -> g env const_env (Let(xt, Seti((M.find y env) lsr 2), e))
     | _ -> raise (Failure "unhandled div."))
  | Let(xt, IfEq(y, C(i), e1, e2), e) when M.mem y env ->
    let e' = if M.find y env = i then e1 else e2 in
    g env const_env (flatten_let xt e e')
  | Let(xt, IfLE(y, C(i), e1, e2), e) when M.mem y env ->
    let e' = if M.find y env <= i then e1 else e2 in
    g env const_env (flatten_let xt e e')
  | Let(xt, IfGE(y, C(i), e1, e2), e) when M.mem y env ->
    let e' = if M.find y env >= i then e1 else e2 in
    g env const_env (flatten_let xt e e')
  | Let(xt, exp, e) -> Let(xt, g' env const_env exp, g env const_env e)
and g' env const_env = function
  | Neg(x) when M.mem x env ->
    let xi = M.find x env in
    if xi >= (2 lsl 12) then
      Neg(x)
    else
      Seti(-xi)
  | Add(x, V(y)) when M.mem y env -> Add(x, C(M.find y env))
  | Add(x, V(y)) when M.mem y const_env ->
    if M.find y const_env = reg_zero then
      Mov(x)
    else
      raise (Failure "invalid constant register.")
  | Add(x, V(y)) when M.mem x env -> Add(y, C(M.find x env))
  | Add(x, V(y)) when M.mem x const_env ->
    if M.find x const_env = reg_zero then
      Mov(y)
    else
      raise (Failure "invalid constant register.")
  | Add(x, C(i)) when M.mem x env -> Seti((M.find x env) + i)
  | Sub(x, y) when M.mem x const_env ->
    if M.find x const_env = reg_zero then
      Neg(y)
    else
      raise (Failure "invalid constant register.")
  | Sub(x, y) when M.mem y const_env ->
    if M.find y const_env = reg_zero then
      Mov(x)
    else
      raise (Failure "invalid constant register.")
  | Mul(x, i) when M.mem x env -> Seti((M.find x env) * i)
  | Div(x, i) when M.mem x env -> Seti((M.find x env) / i)
  | Ld(x, V(y)) when M.mem y env -> Ld(x, C(M.find y env))
  | Ld(x, V(y)) when M.mem y const_env ->
    if M.find y const_env = reg_zero then
      Ld(x, C(0))
    else
      raise (Failure "invalid constant register.")
  | St(x, y, V(z)) when M.mem z env -> St(x, y, C(M.find z env))
  | St(x, y, V(z)) when M.mem z const_env ->
    if M.find z const_env = reg_zero then
      St(x, y, C(0))
    else
      raise (Failure "invalid constant register.")
  | LdF(x, V(y)) when M.mem y env -> LdF(x, C(M.find y env))
  | LdF(x, V(y)) when M.mem y const_env ->
    if M.find y const_env = reg_zero then
      LdF(x, C(0))
    else
      raise (Failure "invalid constant register.")
  | StF(x, y, V(z)) when M.mem z env -> StF(x, y, C(M.find z env))
  | StF(x, y, V(z)) when M.mem z const_env ->
    if M.find z const_env = reg_zero then
      StF(x, y, C(0))
    else
      raise (Failure "invalid constant register.")
  | IfEq(x, V(y), e1, e2) when M.mem y env -> IfEq(x, C(M.find y env), g env const_env e1, g env const_env e2)
  | IfLE(x, V(y), e1, e2) when M.mem y env -> IfLE(x, C(M.find y env), g env const_env e1, g env const_env e2)
  | IfGE(x, V(y), e1, e2) when M.mem y env -> IfGE(x, C(M.find y env), g env const_env e1, g env const_env e2)
  | IfEq(x, V(y), e1, e2) when M.mem y const_env -> IfEq(x, V(M.find y const_env), g env const_env e1, g env const_env e2)
  | IfLE(x, V(y), e1, e2) when M.mem y const_env -> IfLE(x, V(M.find y const_env), g env const_env e1, g env const_env e2)
  | IfGE(x, V(y), e1, e2) when M.mem y const_env -> IfGE(x, V(M.find y const_env), g env const_env e1, g env const_env e2)
  | IfEq(x, V(y), e1, e2) when M.mem x env -> IfEq(y, C(M.find x env), g env const_env e1, g env const_env e2)
  | IfLE(x, V(y), e1, e2) when M.mem x env -> IfGE(y, C(M.find x env), g env const_env e1, g env const_env e2)
  | IfGE(x, V(y), e1, e2) when M.mem x env -> IfLE(y, C(M.find x env), g env const_env e1, g env const_env e2)
  | IfEq(x, V(y), e1, e2) when M.mem x const_env -> IfEq(y, V(M.find x const_env), g env const_env e1, g env const_env e2)
  | IfLE(x, V(y), e1, e2) when M.mem x const_env -> IfGE(y, V(M.find x const_env), g env const_env e1, g env const_env e2)
  | IfGE(x, V(y), e1, e2) when M.mem x const_env -> IfLE(y, V(M.find x const_env), g env const_env e1, g env const_env e2)
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env const_env e1, g env const_env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env const_env e1, g env const_env e2)
  | IfGE(x, y', e1, e2) -> IfGE(x, y', g env const_env e1, g env const_env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env const_env e1, g env const_env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env const_env e1, g env const_env e2)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  let rec h' n e =
    if n = 0 then e
    else
      let nxt = g M.empty M.empty e in
      if e = nxt then e else h' (n-1) nxt in
  { name = l; args = xs; fargs = ys; body = h' 1000 e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, g M.empty M.empty e)
