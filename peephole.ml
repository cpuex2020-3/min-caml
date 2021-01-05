open Asm

let rec replace = function
  | [] -> []
  | cur :: rest ->
    let default _ = cur :: replace rest in
    try
      match cur with
      | Mv(x, y) ->
        (match List.hd rest with
         | Mv(x', y') ->
           if x = y' then
             Mv(x', y) :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Fsgnj(x, y, z) ->
        (match List.hd rest with
         | Fsgnj(x', y', z') ->
           if y = z && y' = z' && x = y' then
             Fsgnj(x', y, y) :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Addi(x, y, i) ->
        (match List.hd rest with
         | Addi(x', y', i') ->
           if x = y && x' = y' && x = x' && y = y' && i = -i' then
             replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Lw(lx, li, ly) ->
        (match List.hd rest with
         | Sw(sx, si, sy) ->
           if lx = sx && li = si && ly = sy then
             cur :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Flw(lx, li, ly) ->
        (match List.hd rest with
         | Fsw(sx, si, sy) ->
           if lx = sx && li = si && ly = sy then
             cur :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Sw(lx, li, ly) ->
        (match List.hd rest with
         | Lw(sx, si, sy) ->
           if lx = sx && li = si && ly = sy then
             cur :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | Fsw(lx, li, ly) ->
        (match List.hd rest with
         | Flw(sx, si, sy) ->
           if lx = sx && li = si && ly = sy then
             cur :: replace (List.tl rest)
           else
             default()
         | _ -> default())
      | e -> default()
    with _ -> default()

let rec iter n e =
  if n = 1000 then e
  else
    let e' = replace e in
    if e = e' then e else iter (n+1) e'

let h { label = l; args = xs; fargs = ys; body = e; ret = t } =
  { label = l; args = xs; fargs = ys; body = iter 0 e; ret = t }

let f { data = data; fundefs = fundefs; prog = e } =
  { data = data; fundefs = List.map h fundefs; prog = iter 0 e }
