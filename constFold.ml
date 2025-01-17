open KNormal

let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float(_) -> true | _ -> false)
  with Not_found -> false
let memt x env =
  try (match M.find x env with Tuple(_) -> true | _ -> false)
  with Not_found -> false

let findi x env = (match M.find x env with Int(i) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d) -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple(ys) -> ys | _ -> raise Not_found)

let rec g env = function
  | Var(x) when memi x env -> Int(findi x env)
  | Var(x) when memf x env -> Float(findf x env)
  | Var(x) when memt x env -> Tuple(findt x env)
  | Neg(x) when memi x env -> Int(-(findi x env))
  | Add(x, y) when memi x env && memi y env -> Int(findi x env + findi y env)
  | Sub(x, y) when memi x env && memi y env -> Int(findi x env - findi y env)
  | FNeg(x) when memf x env -> Float(-.(findf x env))
  | FSqr(x) when memf x env -> Float((findf x env) *. (findf x env))
  | Sqrt(x) when memf x env -> Float(sqrt(findf x env))
  | FAbs(x) when memf x env -> Float(Float.abs (findf x env))
  | FAdd(x, y) when memf x env && memf y env -> Float(findf x env +. findf y env)
  | FSub(x, y) when memf x env && memf y env -> Float(findf x env -. findf y env)
  | FMul(x, y) when memf x env && memf y env -> Float(findf x env *. findf y env)
  | FDiv(x, y) when memf x env && memf y env -> Float(findf x env /. findf y env)
  | FSgnj(x, y) when memf x env && memf y env ->
    Float(if findf y env >= 0. then Float.abs (findf x env) else -. (Float.abs (findf x env)))
  | IfEq(x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
  | IfEq(x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) when memi x env && memi y env -> if findi x env <= findi y env then g env e1 else g env e2
  | IfLE(x, y, e1, e2) when memf x env && memf y env -> if findf x env <= findf y env then g env e1 else g env e2
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfFIsZero(x, e1, e2) when memf x env -> if (findf x env) = 0.0 then g env e1 else g env e2
  | IfFIsZero(x, e1, e2) -> IfFIsZero(x, g env e1, g env e2)
  | IfFIsPos(x, e1, e2) when memf x env -> if (findf x env) > 0.0 then g env e1 else g env e2
  | IfFIsPos(x, e1, e2) -> IfFIsPos(x, g env e1, g env e2)
  | Let((x, t), e1, e2) ->
    let e1' = g env e1 in
    let e2' = g (M.add x e1' env) e2 in
    Let((x, t), e1', e2')
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
    LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple(xts, y, e) when memt y env ->
    List.fold_left2
      (fun e' xt z -> Let(xt, Var(z), e'))
      (g env e)
      xts
      (findt y env)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | Itof(x) when memi x env -> Float(float_of_int(findi x env))
  | e -> e

let f = g M.empty
