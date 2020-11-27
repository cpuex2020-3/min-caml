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
let offset x = 4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> string_of_int i

let rec shuffle sw xys =
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] ->
    (y, sw) :: (x, y) :: shuffle sw (
      List.map (
        function
        | (y', z) when y = y' -> (sw, z)
        | yz -> yz
      )
        xys)
  | xys, acyc -> acyc @ shuffle sw xys

let rec omit_paren s =
  try
    let lparen = String.index s '(' in
    let rparen = String.index s ')' in
    String.sub s (lparen + 1) (rparen - lparen - 1)
  with Not_found -> raise (Failure "no parenthesis.")

type dest = Tail | NonTail of Id.t

let rec g oc = function
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
    g' oc (NonTail(x), exp);
    g oc (dest, e)
and g' oc = function
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tli\t%s, %d\n" x i (* for visibility, use li *)
  | NonTail(x), SetL(Id.L(l)) -> Printf.fprintf oc "\tla\t%s, %s\n" x l
  | NonTail(x), Mov(y) ->
    if x <> y then
      Printf.fprintf oc "\tmv\t%s, %s\n" x y
  | NonTail(x), Neg(y) ->
    if x <> y then Printf.fprintf oc "\tmv\t%s, %s\n" x y;
    Printf.fprintf oc "\tsub\t%s, zero, %s\n" x x
  | NonTail(x), Add(y, z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
  | NonTail(x), AddI(y, i) ->
    if i > 2047 then
      (Printf.fprintf oc "\tli\t%s, %d\n" reg_addi_buf i;
       Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y reg_addi_buf)
    else
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" x y i
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | NonTail(x), Mul(y, i) ->
    if i == 2 then
      Printf.fprintf oc "\tslli\t%s, %s, 1\n" x y
    else if i == 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" x y
    else
      raise (Failure "Unhandled multiplier")
  | NonTail(x), Div(y, i) ->
    if i == 2 then
      Printf.fprintf oc "\tsrli\t%s, %s, 1\n" x y
    else if i == 4 then
      Printf.fprintf oc "\tsrli\t%s, %s, 2\n" x y
    else
      raise (Failure "Unhandled divider")
  | NonTail(x), Ld(y, C(j), i) -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(x), Ld(y, V(z), i) ->
    if i = 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" z z
    else if i = 8 then
      Printf.fprintf oc "\tslli\t%s, %s, 3\n" z z
    else
      raise (Failure "Unhandled size in Ld; emit.ml");
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" y y z;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" x y
  | NonTail(_), St(x, y, C(j), i) -> Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), St(x, y, V(z), i) ->
    if i = 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" z z
    else if i = 8 then
      Printf.fprintf oc "\tslli\t%s, %s, 3\n" z z
    else
      raise (Failure "Unhandled size in Ld; emit.ml");
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" y y z;
    Printf.fprintf oc "\tsw\t%s, 0(%s)\n" x y
  | NonTail(x), FMovD(y) ->
    if x <> y then
      if List.mem x allfregs then
        Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" x y y
      else
        Printf.fprintf oc "\tfcvt.s.w\t%s, %s\n" x y
  | NonTail(x), FNegD(y) ->
    Printf.fprintf oc "\tfsgnjn.s\t%s, %s, %s\n" x y y;
  | NonTail(x), FAddD(y, z) -> Printf.fprintf oc "\tfadd.s\t%s, %s, %s\n" x y z
  | NonTail(x), FSubD(y, z) -> Printf.fprintf oc "\tfsub.s\t%s, %s, %s\n" x y z
  | NonTail(x), FMulD(y, z) -> Printf.fprintf oc "\tfmul.s\t%s, %s, %s\n" x y z
  | NonTail(x), FDivD(y, z) -> Printf.fprintf oc "\tfdiv.s\t%s, %s, %s\n" x y z
  | NonTail(x), LdDF(y, V(z), i) ->
    if i = 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" z z
    else if i = 8 then
      Printf.fprintf oc "\tslli\t%s, %s, 3\n" z z
    else
      raise (Failure "Unhandled size in LdDF; emit.ml");
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" y y z;
    Printf.fprintf oc "\tflw\t%s, 0(%s)\n" x y
  | NonTail(x), LdDF(y, C(j), i) -> Printf.fprintf oc "\tflw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), StDF(x, y, V(z), i) ->
    if i = 4 then
      Printf.fprintf oc "\tslli\t%s, %s, 2\n" z z
    else if i = 8 then
      Printf.fprintf oc "\tslli\t%s, %s, 3\n" z z
    else
      raise (Failure "Unhandled size in StDF; emit.ml");
    Printf.fprintf oc "\tadd\t%s, %s, %s\n" y y z;
    Printf.fprintf oc "\tfsw\t%s, 0(%s)\n" x y
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.fprintf oc "\tfsw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    Printf.fprintf oc "\tfsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  | NonTail(x), Restore(y) when List.mem x allregs ->
    Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.fprintf oc "\tflw\t%s, %d(%s)\n" x (offset y) reg_sp
  | Tail, (Nop | St _ | StDF _ | Save _ as exp) ->
    g' oc (NonTail(Id.gentmp Type.Unit), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | AddI _ | Sub _ | Ld _ | Mul _ | Div _ as exp) ->
    g' oc (NonTail(regs.(0)), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
    g' oc (NonTail(fregs.(0)), exp);
    Printf.fprintf oc "\tret\n";
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> g' oc (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
     | _ -> assert false);
    Printf.fprintf oc "\tret\n"
  | Tail, IfEq(x, y, e1, e2) ->
    g'_tail_if oc x y e1 e2 "be" "bne"
  | Tail, IfLE(x, y, e1, e2) ->
    g'_tail_if oc y x e1 e2 "bge" "blt"
  | Tail, IfFEq(x, y, cmp, e1, e2) ->
    g'_tail_float_if oc x y cmp e1 e2 "feq.s"
  | Tail, IfFLE(x, y, cmp, e1, e2) ->
    g'_tail_float_if oc x y cmp e1 e2 "fle.s"
  | NonTail(z), IfEq(x, y, e1, e2) ->
    g'_non_tail_if oc (NonTail(z)) x y e1 e2 "be" "bne"
  | NonTail(z), IfLE(x, y, e1, e2) ->
    g'_non_tail_if oc (NonTail(z)) y x e1 e2 "bge" "blt"
  | NonTail(z), IfFEq(x, y, cmp, e1, e2) ->
    g'_non_tail_float_if oc (NonTail(z)) x y cmp e1 e2 "feq.s"
  | NonTail(z), IfFLE(x, y, cmp, e1, e2) ->
    g'_non_tail_float_if oc (NonTail(z)) x y cmp e1 e2 "fle.s"
  | Tail, CallCls(x, ys, zs, reg_cl_buf) ->
    g'_args oc [(x, reg_cl)] ys zs;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" reg_cl_buf reg_cl;
    Printf.fprintf oc "\tjalr\tzero, %s, 0\n" reg_cl_buf;
  | Tail, CallDir(Id.L(x), ys, zs) ->
    g'_args oc [] ys zs;
    Printf.fprintf oc "\tj\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs, reg_cl_buf) ->
    g'_args oc [(x, reg_cl)] ys zs;
    let ss = stacksize () in
    Printf.fprintf oc "\tsw\t%s, %d(%s)\n" reg_ra ss reg_sp;
    Printf.fprintf oc "\tlw\t%s, 0(%s)\n" reg_cl_buf reg_cl;
    Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tjalr\t%s\n" reg_cl_buf;
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
and g'_tail_if oc x y e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_tail_float_if oc x y cmp e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" b cmp x y;
  Printf.fprintf oc "\tbeq\t%s, zero, %s\n" cmp b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_non_tail_if oc dest x y e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
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
and g'_non_tail_float_if oc dest x y cmp e1 e2 b =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" b cmp x y;
  Printf.fprintf oc "\tbeq\t%s, zero, %s\n" cmp b_else;
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
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> (
         (* TODO: understand how it works *)
         if String.contains r '(' then Printf.fprintf oc "\tsw\t%s, %s\n" y r
         else if String.contains y '(' then Printf.fprintf oc "\tlw\t%s, %s\n" r y
         else Printf.fprintf oc "\tmv\t%s, %s\n" r y
       ))
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (* TODO: maybe wrong *)
    (fun (z, fr) ->
       if List.mem fr allfregs then
         (if String.contains z ')' then
            (Printf.fprintf oc "\tlw\t%s, %s\n" (omit_paren z) z;
             Printf.fprintf oc "\tfcvt.s.w\t%s, %s\n" fr (omit_paren z))
          else
            Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" fr z z
         )
       else
         (if String.contains fr ')' then
            (Printf.fprintf oc "\tfcvt.w.s\t%s, %s\n" (omit_paren fr) z;
             Printf.fprintf oc "\tsw\t%s, %s\n" (omit_paren fr) fr)
          else
            Printf.fprintf oc "\tfcvt.w.s\t%s, %s\n" fr z))
    (shuffle sw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  let callee_saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
  let callee_saved_regs_count = List.length callee_saved_regs in
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\t.data\n";
  List.iter
    (fun (Id.L(x), d) ->
       Printf.fprintf oc "%s:\t# %f\n" x d;
       Printf.fprintf oc "\t.word\t0x%lx\n" (Int32.bits_of_float d))
    data;
  Printf.fprintf oc "\t.text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "\t.globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc "\taddi\tsp, sp, -52\n";
  List.iteri (
    fun i r -> Printf.fprintf oc "\tsw\t%s, %d(sp)\n" r ((callee_saved_regs_count - i) * 4);
  ) callee_saved_regs;
  stackset := S.empty;
  stackmap := [];
  Printf.fprintf oc "\taddi\t%s, sp, 56\n" reg_sp;
  Printf.fprintf oc "\taddi\t%s, sp, 60\n" regs.(0);
  (*Printf.fprintf oc "\tla\t%s, min_caml_hp\n" reg_hp;*)
  g oc (NonTail(regs.(0)), e);
  List.iteri (
    fun i r -> Printf.fprintf oc "\tlw\t%s, %d(sp)\n" r ((i + 1) * 4);
  ) (List.rev callee_saved_regs);
  Printf.fprintf oc "\taddi\tsp, sp, 52\n";
  Printf.fprintf oc "\tret\n";
