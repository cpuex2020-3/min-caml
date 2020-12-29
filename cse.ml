open KNormal

let is_equal = function
  | Add(x1, y1), Add(x2, y2) | Sub(x1, y1), Sub(x2, y2)
  | FAdd(x1, y1), FAdd(x2, y2) | FSub(x1, y1), FSub(x2, y2)
  | FMul(x1, y1), FMul(x2, y2) | FDiv(x1, y1), FDiv(x2, y2)
    -> x1 = x2 && y1 = y2
  | Mul(x1, y1), Mul(x2, y2) | Div(x1, y1), Div(x2, y2)
    -> x1 = x2 && y1 = y2
  | Neg(x1), Neg(x2) | FNeg(x1), FNeg(x2) | Itof(x1), Itof(x2)
  | FSqr(x1), FSqr(x2) | Sqrt(x1), Sqrt(x2) | FAbs(x1), FAbs(x2) -> x1 = x2
  | _ -> false

let rec search env key =
  match env with
  | hd :: tl ->
    let (k, v) = hd in
    if is_equal (k, key) then Var(v) else search tl key
  | [] -> key

let rec g env = function
  | Add _ | Sub _ | Mul _ | Div _ | FAdd _ | FSub _ | FMul _ | FDiv _
  | Neg _ | FNeg _ | Itof _ | FSqr _ | Sqrt _ | FAbs _ | FSgnj _
  as ex ->
    search env ex
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env (search env e1), g env (search env e2))
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env (search env e1), g env (search env e2))
  | IfFIsPos(x, e1, e2) -> IfFIsPos(x, g env (search env e1), g env (search env e2))
  | IfFIsZero(x, e1, e2) -> IfFIsZero(x, g env (search env e1), g env (search env e2))
  | Let((x, t), e1, e2) ->
    Let((x, t), g env (search env e1), g ((e1, x) :: env) (search env e2))
  | e -> e

let f = g []
