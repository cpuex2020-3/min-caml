(* translation into assembly with infinite number of virtual registers *)

open Asm
open ConstExp
open Ir

let float_data = ref [] (* Table for floating points. *)
let array_data = ref [] (* Table for global variables. *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
       match t with
       | Type.Unit -> acc
       | Type.Float -> addf acc x
       | _ -> addi acc x t)
    ini
    xts

let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x -> (offset + !inc, addf x offset acc))
    (fun (offset, acc) x t -> (offset + !inc, addi x t offset acc))

let globenv = ref M.empty
let data_top = ref !data_top_default
let label_to_address = ref M.empty

let find_var x env =
  if M.mem x env then
    M.find x env
  else if M.mem x !globenv then
    M.find x !globenv
  else
    raise (Failure (Printf.sprintf "%s is not found." x))

let rec cal_position = function
  | ConstArray(len, init) ->
    (match init with
     | ConstInt(_) | ConstFloat(_) ->
       let ret = !data_top in
       data_top := !inc * len + !data_top;
       ret
     | ConstBool(_) -> raise (Failure "Bool array??")
     | ConstTuple(_) | ConstArray(_) ->
       let _ = cal_position init in
       let ret = !data_top in
       data_top := !inc * len + !data_top;
       ret)
  | ConstInt(i) -> data_top := !data_top + !inc; -1
  | ConstBool(b) -> data_top := !data_top + !inc; -1
  | ConstFloat(f) -> data_top := !data_top + !inc; -1
  | ConstTuple(cs) ->
    let arrays = List.filter (function ConstArray(_) | ConstTuple(_) -> true | _ -> false) cs in
    List.iter (fun arr -> let _ = cal_position arr in ()) arrays;
    let ret = !data_top in
    data_top := !data_top + List.length cs * !inc;
    ret

let rec g env = function
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Seti(i))
  | Closure.Float(d) ->
    if d = 0.0 then
      Ans(FMov(reg_fzero))
    else
      let l =
        try
          let (l, _) = List.find (fun (_, d') -> d = d') !float_data in
          l
        with Not_found ->
          let l = Id.L(Id.genid "l") in
          float_data := (l, d) :: !float_data;
          l in
      Ans(SetFi(l))
  | Closure.Neg(x) -> Ans(Neg(x))
  | Closure.Add(x, y) -> Ans(Add(x, V(y)))
  | Closure.Sub(x, y) -> Ans(Sub(x, y))
  | Closure.Mul(x, y) -> Ans(Mul(x, y))
  | Closure.Div(x, y) -> Ans(Div(x, y))
  | Closure.FNeg(x) -> Ans(FNeg(x))
  | Closure.FSqr(x) -> Ans(FSqr(x))
  | Closure.Sqrt(x) -> Ans(Sqrt(x))
  | Closure.FAbs(x) -> Ans(FAbs(x))
  | Closure.FAdd(x, y) -> Ans(FAdd(x, y))
  | Closure.FSub(x, y) -> Ans(FSub(x, y))
  | Closure.FMul(x, y) -> Ans(FMul(x, y))
  | Closure.FDiv(x, y) -> Ans(FDiv(x, y))
  | Closure.FSgnj(x, y) -> Ans(FSgnj(x, y))
  | Closure.IfEq(x, y, e1, e2) ->
    (match M.find x env with
     | Type.Bool | Type.Int -> Ans(IfEq(x, V(y), g env e1, g env e2))
     | Type.Float -> Ans(IfFEq(x, y, g env e1, g env e2))
     | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
    (match M.find x env with
     | Type.Bool | Type.Int -> Ans(IfLE(x, V(y), g env e1, g env e2))
     | Type.Float -> Ans(IfFLE(x, y, g env e1, g env e2))
     | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.IfFIsZero(x, e1, e2) -> (Ans(IfFEq(x, reg_fzero, g env e1, g env e2)))
  | Closure.IfFIsPos(x, e1, e2) -> (Ans(IfFLE(x, reg_fzero, g env e2, g env e1)))
  | Closure.Let((x, t1), e1, e2) ->
    let e1' = g env e1 in
    let e2' = g (M.add x t1 env) e2 in
    concat e1' (x, t1) e2'
  | Closure.GlobalLet((x, t1), const, e) ->
    label_to_address := M.add x (cal_position const) !label_to_address;
    array_data := (x, const) :: !array_data;
    globenv := M.add x t1 !globenv;
    g env e
  | Closure.Var(x) ->
    (match M.find x env with
     | Type.Unit -> Ans(Nop)
     | Type.Float -> Ans(FMov(x))
     | _ -> Ans(Mov(x)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) ->
    let e2' = g (M.add x t env) e2 in
    let (offset, store_fv) =
      expand
        (List.map (fun y -> (y, find_var y env)) ys)
        (!inc, e2')
        (fun y offset store_fv -> seq(StF(y, x, C(offset)), store_fv))
        (fun y _ offset store_fv -> seq(St(y, x, C(offset)), store_fv)) in
    Let((x, t), Mov(reg_hp),
        Let((reg_hp, Type.Int), Add(reg_hp, C(offset)),
            let z = Id.genid "l" in
            Let((z, Type.Int), SetL(l),
                seq(St(z, x, C(0)),
                    store_fv))))
  | Closure.AppCls(x, ys) ->
    let (int, float) = separate (List.map (fun y -> (y, find_var y env)) ys) in
    Ans(CallCls(x, int, float))
  | Closure.AppDir(Id.L(x), ys) ->
    let (int, float) = separate (List.map (fun y -> (y, find_var y env)) ys) in
    Ans(CallDir(Id.L(x), int, float))
  | Closure.Tuple(xs) ->
    let y = Id.genid "t" in
    let (offset, store) =
      expand
        (List.map (fun x -> (x, find_var x env)) xs)
        (0, Ans(Mov(y)))
        (fun x offset store -> seq(StF(x, y, C(offset)), store))
        (fun x _ offset store -> seq(St(x, y, C(offset)), store)) in
    Let((y, Type.Tuple(List.map (fun x -> find_var x env) xs)), Mov(reg_hp),
        Let((reg_hp, Type.Int), Add(reg_hp, C(offset)),
            store))
  | Closure.LetTuple(xts, y, e2) ->
    let s = Closure.fv e2 in
    let (offset, load) =
      expand
        xts
        (0, g (M.add_list xts env) e2)
        (fun x offset load ->
           if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
             fletd(x, LdF(y, C(offset)), load))
        (fun x t offset load ->
           if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
             Let((x, t), Ld(y, C(offset)), load)) in
    load
  | Closure.Get(x, y) ->
    if M.mem x env then
      let offset = Id.genid "o" in
      match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
        if !is_word_addressing then
          Ans(LdF(x, V(y)))
        else
          Let((offset, Type.Int), Mul(y, !inc),
              Ans(LdF(x, V(offset))))
      | Type.Array(_) ->
        if !is_word_addressing then
          Ans(Ld(x, V(y)))
        else
          Let((offset, Type.Int), Mul(y, !inc),
              Ans(Ld(x, V(offset))))
      | _ -> assert false
    else if M.mem x !globenv then
      let addr = Id.genid "l" in
      let offset = Id.genid "o" in
      match M.find x !globenv with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
        Let((addr, Type.Int), Seti(M.find x !label_to_address),
            if !is_word_addressing then
              Ans(LdF(addr, V(y)))
            else
              Let((offset, Type.Int), Mul(y, !inc),
                  Ans(LdF(addr, V(offset)))))
      | Type.Array(_) ->
        Let((addr, Type.Int), Seti(M.find x !label_to_address),
            if !is_word_addressing then
              Ans(Ld(addr, V(y)))
            else
              Let((offset, Type.Int), Mul(y, !inc),
                  Ans(Ld(addr, V(offset)))))
      | _ -> raise (Failure "invalid type for global array.")
    else
      raise (Failure (Printf.sprintf "variable %s is not found anywhere in Get." x))
  | Closure.Put(x, y, z) ->
    if M.mem x env then
      let offset = Id.genid "o" in
      match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
        if !is_word_addressing then
          Ans(StF(z, x, V(y)))
        else
          Let((offset, Type.Int), Mul(y, !inc),
              Ans(StF(z, x, V(offset))))
      | Type.Array(_) ->
        if !is_word_addressing then
          Ans(St(z, x, V(y)))
        else
          Let((offset, Type.Int), Mul(y, !inc),
              Ans(St(z, x, V(offset))))
      | _ -> assert false
    else if M.mem x !globenv then
      let addr = Id.genid "l" in
      let offset = Id.genid "o" in
      match M.find x !globenv with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
        Let((addr, Type.Int), Seti(M.find x !label_to_address),
            if !is_word_addressing then
              Ans(StF(z, addr, V(y)))
            else
              Let((offset, Type.Int), Mul(y, !inc),
                  Ans(StF(z, addr, V(offset)))))
      | Type.Array(_) ->
        Let((addr, Type.Int), Seti(M.find x !label_to_address),
            if !is_word_addressing then
              Ans(St(z, addr, V(y)))
            else
              Let((offset, Type.Int), Mul(y, !inc),
                  Ans(St(z, addr, V(offset)))))
      | _ -> assert false
    else
      raise (Failure (Printf.sprintf "variable %s is not found anywhere in Put." x))
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))
  | Closure.Itof(x) -> Ans(Itof(x))

let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (int, float) = separate yts in
  let (offset, load) =
    expand
      zts
      (!inc, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd(z, LdF(x, C(offset)), load))
      (fun z t offset load -> Let((z, t), Ld(x, C(offset)), load)) in
  match t with
  | Type.Fun(_, t2) -> { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
  | _ -> assert false

let f (Closure.Prog(fundefs, e)) =
  data_top := !data_top_default;
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog(!float_data, List.rev !array_data, fundefs, e)
