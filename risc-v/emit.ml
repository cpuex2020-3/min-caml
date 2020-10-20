open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

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

type dest = Tail | NonTail of Id.t
let rec count_stack_size = function
  | dest, Ans(exp) -> count_stack_size' (dest, exp)
  | dest, Let((x, t), exp, e) ->
    count_stack_size' (NonTail(x), exp) + count_stack_size (dest, e)
and count_stack_size' = function
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) -> 1
  | Tail, (Nop | St _ | StDF _ | Save _ as exp) ->
    count_stack_size' (NonTail(Id.gentmp Type.Unit), exp)
  | Tail, (Set _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    count_stack_size' (NonTail(regs.(0)), exp)
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> count_stack_size' (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> count_stack_size' (NonTail(fregs.(0)), exp)
     | _ -> assert false)
  | Tail, IfEq(x, y, e1, e2) -> count_stack_size'_tail_if x y e1 e2 "be" "bne"
  | Tail, IfLE(x, y, e1, e2) -> count_stack_size'_tail_if y x e1 e2 "bge" "blt"
  | NonTail(z), IfEq(x, y, e1, e2) -> count_stack_size'_non_tail_if (NonTail(z)) x y e1 e2 "be" "bne"
  | NonTail(z), IfLE(x, y, e1, e2) -> count_stack_size'_non_tail_if (NonTail(z)) y x e1 e2 "bge" "blt"
  | Tail, CallCls(x, ys, zs) -> 0
  | Tail, CallDir(Id.L(x), ys, zs) -> 0
  | NonTail(a), CallCls(x, ys, zs) -> 0
  | NonTail(a), CallDir(Id.L(x), ys, zs) -> 0
  | _ -> 0
and count_stack_size'_tail_if x y e1 e2 b bn =
  let stackset_back = !stackset in
  let first_half = count_stack_size (Tail, e1) in
  stackset := stackset_back;
  first_half + count_stack_size (Tail, e2)
and count_stack_size'_non_tail_if dest x y e1 e2 b bn =
  let stackset_back = !stackset in
  let cur = count_stack_size (dest, e1) in
  let stackset1 = !stackset in
  stackset := stackset_back;
  let cur = count_stack_size (dest, e2) + cur in
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2;
  cur

let cur_stack_size = ref 0
let epilog oc () =
  if !cur_stack_size > 0 then Printf.fprintf oc "\taddi\tsp, sp, %d\n" !cur_stack_size;
  Printf.fprintf oc "\tret\n"

let rec g oc = function
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
    g' oc (NonTail(x), exp);
    g oc (dest, e)
and g' oc = function
  | NonTail(_), Nop -> ()
  (*| NonTail(x), Set(i) -> Printf.fprintf oc "\taddi\t%s, zero, %d\n" x i TODO: handle large numbers*)
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tli\t%s, %d\n" x i
  | NonTail(x), Mov(y) -> if x <> y then Printf.fprintf oc "\tmv\t%s, %s\n" x y
  | NonTail(x), Neg(y) ->
    if x <> y then Printf.fprintf oc "\tmv\t%s, %s\n" x y;
    Printf.fprintf oc "\tsub\t%s, zero, %s\n" x x
  | NonTail(x), Add(_, y, z') ->
    if V(x) = z' then Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y x
    else (
      match z' with
      | V(z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
      | C(z) -> Printf.fprintf oc "\taddi\t%s, %s, %s\n" x y (string_of_int z)
    )
  | NonTail(x), Sub(_, y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | NonTail(x), Ld(y, C(j), i) -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), St(x, y, C(j), i) -> Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  | NonTail(x), Restore(y) when List.mem x allregs ->
    Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  | Tail, (Nop | St _ | StDF _ | Save _ as exp) ->
    g' oc (NonTail(Id.gentmp Type.Unit), exp);
    epilog oc ()
  | Tail, (Set _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    g' oc (NonTail(regs.(0)), exp);
    epilog oc ()
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> g' oc (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
     | _ -> assert false);
    epilog oc ()
  | Tail, IfEq(x, y, e1, e2) ->
    g'_tail_if oc x y e1 e2 "be" "bne"
  | Tail, IfLE(x, y, e1, e2) ->
    g'_tail_if oc y x e1 e2 "bge" "blt"
  | NonTail(z), IfEq(x, y, e1, e2) ->
    g'_non_tail_if oc (NonTail(z)) x y e1 e2 "be" "bne"
  | NonTail(z), IfLE(x, y, e1, e2) ->
    g'_non_tail_if oc (NonTail(z)) y x e1 e2 "bge" "blt"
  | Tail, CallCls(x, ys, zs) ->
    g'_args oc [(x, reg_cl)] ys zs;
    if !cur_stack_size > 0 then Printf.fprintf oc "\taddi\tsp, sp, %d\n" !cur_stack_size;
    Printf.fprintf oc "\tj\t*(%s)\n" reg_cl;
  | Tail, CallDir(Id.L(x), ys, zs) ->
    g'_args oc [] ys zs;
    if !cur_stack_size > 0 then Printf.fprintf oc "\taddi\tsp, sp, %d\n" !cur_stack_size;
    Printf.fprintf oc "\tj\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
    g'_args oc [(x, reg_cl)] ys zs;
    let ss = stacksize () in
    if ss > 0 then Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp (ss + 4);
    Printf.fprintf oc "\tTODO:call\t*(%s)\n" reg_cl;
    if ss > 0 then Printf.fprintf oc "\taddi\t%s, %s, -%d\n" reg_sp reg_sp (ss + 4);
    if List.mem a allregs && a <> regs.(0) then
      Printf.fprintf oc "\tmv\t%s, %s\n" a regs.(0)
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    g'_args oc [] ys zs;
    let ss = stacksize () in
    Printf.fprintf oc "\tsw\tra, %d(%s)\n" ss reg_sp;
    Printf.fprintf oc "\tjal\t%s\n" x;
    Printf.fprintf oc "\tlw\tra, %d(%s)\n" ss reg_sp;
    if List.mem a allregs && a <> regs.(0) then
      Printf.fprintf oc "\tmv\t%s, %s\n" a regs.(0)
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
  | _ -> raise (Failure "Unhandled in emit!")
and g'_tail_if oc x y e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y b_else;
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
    (fun (z, fr) -> Printf.fprintf oc "\tmovsd\t%s, %s\n" z fr)
    (shuffle sw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  (* stack usage should be an open section, and the area for `ra` may be needed, so +2 *)
  cur_stack_size := 4 * (count_stack_size (Tail, e) + 2);
  if !cur_stack_size > 0 then Printf.fprintf oc "\taddi\tsp, sp, -%d\n" !cur_stack_size;
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  let callee_saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
  let callee_saved_regs_count = List.length callee_saved_regs in
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc ".data\n";
  Printf.fprintf oc ".balign\t8\n";
  List.iter
    (fun (Id.L(x), d) ->
       Printf.fprintf oc "%s:\t# %f\n" x d;
       Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
       Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc ".globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  stackset := S.empty;
  stackmap := [];
  Printf.fprintf oc "\taddi\t%s, %s, -%d\n" reg_sp reg_sp (4 * callee_saved_regs_count);
  List.iteri (
    fun i r -> Printf.fprintf oc "\tsw\t%s, %d(%s)\n" r ((callee_saved_regs_count - i) * 4) reg_sp;
  ) callee_saved_regs;
  g oc (NonTail(regs.(0)), e);
  List.iteri (
    fun i r -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" r ((i + 1) * 4) reg_sp;
  ) (List.rev callee_saved_regs);
  Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp (4 * List.length callee_saved_regs);
  Printf.fprintf oc "\tret\n";
