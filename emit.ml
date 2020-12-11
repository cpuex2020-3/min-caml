open Asm

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
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
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

let rec g oc = function
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
    g' oc (NonTail(x), exp);
    g oc (dest, e)
and g' oc = function
  | NonTail(_), Nop -> ()
  | NonTail(x), Seti(i) -> Printf.fprintf oc "\tli\t%s, %d\n" x i
  | NonTail(x), SetFi(Id.L(l)) ->
    Printf.fprintf oc "\tla\t%s, %s\n" reg_buf l;
    Printf.fprintf oc "\tflw\t%s, 0(%s)\n" x reg_buf
  | NonTail(x), SetL(Id.L(l)) -> Printf.fprintf oc "\tla\t%s, %s\n" x l
  | NonTail(x), Mov(y) ->
    if x <> y then
      Printf.fprintf oc "\tmv\t%s, %s\n" x y
  | NonTail(x), Neg(y) ->
    if x <> y then Printf.fprintf oc "\tmv\t%s, %s\n" x y;
    Printf.fprintf oc "\tsub\t%s, zero, %s\n" x x
  | NonTail(x), Add(y, V(z)) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
  | NonTail(x), Add(y, C(i)) ->
    if i > 2047 then
      (Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y reg_buf)
    else
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" x y i
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | NonTail(x), Mul(y, i) ->
    if i = 2 then
      Printf.fprintf oc "\tslli\t%s, %s, 1\n" x y
    else if i = 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" x y
    else
      raise (Failure "Unhandled multiplier")
  | NonTail(x), Div(y, i) ->
    if i = 2 then
      Printf.fprintf oc "\tsrli\t%s, %s, 1\n" x y
    else if i = 4 then
      Printf.fprintf oc "\tsrli\t%s, %s, 2\n" x y
    else
      raise (Failure "Unhandled divider")
  | NonTail(x), Ld(y, C(j)) -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x j y
  | NonTail(x), Ld(y, V(z)) ->
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" reg_buf y z;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" x reg_buf
  | NonTail(_), St(x, y, C(j)) -> Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x j y
  | NonTail(_), St(x, y, V(z)) ->
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" reg_buf y z;
    Printf.fprintf oc "\tsw\t%s, 0(%s)\n" x reg_buf
  | NonTail(x), FMov(y) ->
    if x <> y then
      (assert (List.mem x allfregs);
       Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" x y y)
  | NonTail(x), FNeg(y) -> Printf.fprintf oc "\tfsgnjn.s\t%s, %s, %s\n" x y y;
  | NonTail(x), FAdd(y, z) -> Printf.fprintf oc "\tfadd.s\t%s, %s, %s\n" x y z
  | NonTail(x), FSub(y, z) -> Printf.fprintf oc "\tfsub.s\t%s, %s, %s\n" x y z
  | NonTail(x), FMul(y, z) -> Printf.fprintf oc "\tfmul.s\t%s, %s, %s\n" x y z
  | NonTail(x), FDiv(y, z) -> Printf.fprintf oc "\tfdiv.s\t%s, %s, %s\n" x y z
  | NonTail(x), LdF(y, V(z)) ->
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" reg_buf y z;
    Printf.fprintf oc "\tflw\t%s, 0(%s)\n" x reg_buf
  | NonTail(x), LdF(y, C(j)) -> Printf.fprintf oc "\tflw\t%s, %d(%s)\n" x j y
  | NonTail(_), StF(x, y, V(z)) ->
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" reg_buf y z;
    Printf.fprintf oc "\tfsw\t%s, 0(%s)\n" x reg_buf
  | NonTail(_), StF(x, y, C(j)) -> Printf.fprintf oc "\tfsw\t%s, %d(%s)\n" x j y
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    Printf.fprintf oc "\tfsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  | NonTail(x), Restore(y) when List.mem x allregs ->
    if M.mem y !Virtual.globenv then
      Printf.fprintf oc "\tla\t%s, %s\n" x y
    else
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.fprintf oc "\tflw\t%s, %d(%s)\n" x (offset y) reg_sp
  | Tail, (Nop | St _ | StF _ | Save _ as exp) ->
    g' oc (NonTail(Id.gentmp Type.Unit), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (Seti _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ | Mul _ | Div _ as exp) ->
    g' oc (NonTail(regs.(0)), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (SetFi _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | LdF _  as exp) ->
    g' oc (NonTail(fregs.(0)), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> g' oc (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
     | _ -> assert false);
    Printf.fprintf oc "\tret\n"
  | Tail, IfEq(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if oc x y e1 e2 "bne"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_tail_if oc x reg_buf e1 e2 "bne")
  | Tail, IfLE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if oc y x e1 e2 "blt"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_tail_if oc reg_buf x e1 e2 "blt")
  | Tail, IfGE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_tail_if oc x y e1 e2 "blt"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_tail_if oc x reg_buf e1 e2 "blt")
  | Tail, IfFEq(x, y, e1, e2) ->
    g'_tail_float_if oc x y e1 e2 "feq.s"
  | Tail, IfFLE(x, y, e1, e2) ->
    g'_tail_float_if oc x y e1 e2 "fle.s"
  | NonTail(z), IfEq(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if oc (NonTail(z)) x y e1 e2 "bne"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_non_tail_if oc (NonTail(z)) x reg_buf e1 e2 "bne")
  | NonTail(z), IfLE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if oc (NonTail(z)) y x e1 e2 "blt"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_non_tail_if oc (NonTail(z)) reg_buf x e1 e2 "blt")
  | NonTail(z), IfGE(x, y', e1, e2) ->
    (match y' with
     | V(y) -> g'_non_tail_if oc (NonTail(z)) x y e1 e2 "blt"
     | C(i) ->
       Printf.fprintf oc "\tli\t%s, %d\n" reg_buf i;
       g'_non_tail_if oc (NonTail(z)) x reg_buf e1 e2 "blt")
  | NonTail(z), IfFEq(x, y, e1, e2) ->
    g'_non_tail_float_if oc (NonTail(z)) x y e1 e2 "feq.s"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
    g'_non_tail_float_if oc (NonTail(z)) x y e1 e2 "fle.s"
  | Tail, CallCls(x, ys, zs) ->
    g'_args oc [(x, reg_cl)] ys zs;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" reg_buf reg_cl;
    Printf.fprintf oc "\tjalr\tzero, %s, 0\n" reg_buf;
  | Tail, CallDir(Id.L(x), ys, zs) ->
    g'_args oc [] ys zs;
    Printf.fprintf oc "\tj\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
    g'_args oc [(x, reg_cl)] ys zs;
    let ss = stacksize () in
    Printf.fprintf oc "\tsw\t%s, %d(%s)\n" reg_ra ss reg_sp;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" reg_buf reg_cl;
    Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tjalr\t%s\n" reg_buf;
    Printf.fprintf oc "\taddi\t%s, %s, -%d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tlw\t%s, %d(%s)\n" reg_ra ss reg_sp;
    if List.mem a allregs && a <> regs.(0) then
      Printf.fprintf oc "\tmv\t%s, %s\n" a regs.(0)
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" a fregs.(0) fregs.(0)
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    g'_args oc [] ys zs;
    let ss = stacksize () in
    Printf.fprintf oc "\tsw\tra, %d(%s)\n" ss reg_sp;
    Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tjal\t%s\n" x;
    Printf.fprintf oc "\taddi\t%s, %s, -%d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tlw\tra, %d(%s)\n" ss reg_sp;
    if List.mem a allregs && a <> regs.(0) then
      Printf.fprintf oc "\tmv\t%s, %s\n" a regs.(0)
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" a fregs.(0) fregs.(0)
and g'_tail_if oc x y e1 e2 bn =
  let b_else = Id.genid (bn ^ "_stands") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_tail_float_if oc x y e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" b reg_buf x y;
  Printf.fprintf oc "\tbeq\t%s, zero, %s\n" reg_buf b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_non_tail_if oc dest x y e1 e2 bn =
  let b_else = Id.genid (bn ^ "_stands") in
  let b_cont = Id.genid (bn ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_non_tail_float_if oc dest x y e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" b reg_buf x y;
  Printf.fprintf oc "\tbeq\t%s, zero, %s\n" reg_buf b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmv\t%s, %s\n" r y)
    (shuffle reg_sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" fr z z)
    (shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let data_top = ref 100 (* .data starts from memory address 100. *)

let rec select_const_array_and_tuple = function (* TODO: use filter *)
  | hd :: tl ->
    (match hd with
     | KNormal.ConstArray(_) | KNormal.ConstTuple(_) -> hd :: select_const_array_and_tuple tl
     | _ -> select_const_array_and_tuple tl)
  | [] -> []

let rec gen_global oc x = function
  | KNormal.ConstArray(len, init) ->
    (match init with
     | ConstInt(_) | ConstFloat(_) ->
       Printf.fprintf oc "%s:\n" x;
       for cnt = 1 to len do
         gen_global oc x init
       done
     | ConstBool(_) -> raise (Failure "Bool array??")
     | ConstTuple(_) | ConstArray(_) ->
       let head_addr = !data_top in
       gen_global oc (Printf.sprintf "%s_init" x) init;
       Printf.fprintf oc "%s:\n" x;
       for cnt = 1 to len do
         Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int head_addr); data_top := !data_top + 4
       done)
  | KNormal.ConstInt(i) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int i);
    data_top := !data_top + 4
  | KNormal.ConstBool(b) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (if b then 1 else 0));
    data_top := !data_top + 4
  | KNormal.ConstFloat(f) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.bits_of_float f);
    data_top := !data_top + 4
  | KNormal.ConstTuple(cs) ->
    let arrays = select_const_array_and_tuple cs in
    let head_addresses = ref [] in
    List.iteri
      (fun i arr ->
         (let head = !data_top in
          head_addresses := head :: !head_addresses;
          gen_global oc (Printf.sprintf "%s.arr_or_tpl.%d" x i) arr))
      arrays;
    head_addresses := List.rev !head_addresses;
    Printf.fprintf oc "%s:\n" x;
    let counter = ref 0 in
    List.iter
      (function
        | KNormal.ConstInt(_) | KNormal.ConstFloat(_) as c ->
          gen_global oc x c
        | KNormal.ConstBool(b) -> Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (if b then 1 else 0))
        | KNormal.ConstArray(_) ->
          Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (List.nth !head_addresses !counter));
          counter := !counter + 1;
        | KNormal.ConstTuple(_) ->
          Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (List.nth !head_addresses !counter));
          counter := !counter + 1)
      cs

let f oc (Prog(float_data, array_data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\t.data\n";
  List.iter (fun (x, const_exp) -> gen_global oc x const_exp) array_data;
  List.iter
    (fun (Id.L(x), d) ->
       Printf.fprintf oc "%s:\t# %f\n" x d;
       Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.bits_of_float d))
    float_data;
  Printf.fprintf oc "\t.text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "\t.globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc "\taddi\tsp, sp, -52\n";
  stackset := S.empty;
  stackmap := [];
  Printf.fprintf oc "\taddi\t%s, sp, 56\n" reg_sp;
  Printf.fprintf oc "\taddi\t%s, sp, 60\n" regs.(0);
  g oc (NonTail(regs.(0)), e);
  Printf.fprintf oc "\taddi\tsp, sp, 52\n";
  Printf.fprintf oc "\tret\n";
