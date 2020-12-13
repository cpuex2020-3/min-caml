open Asm
open ConstExp

let gen_elem oc = function
  | Li(x, i) -> Printf.fprintf oc "\tli\t%s, %d\n" x i
  | La(x, Id.L(l)) -> Printf.fprintf oc "\tla\t%s, %s\n" x l
  | Mv(x, y) -> Printf.fprintf oc "\tmv\t%s, %s\n" x y
  | Addi(x, y, i) -> Printf.fprintf oc "\taddi\t%s, %s, %d\n" x y i
  | Add(x, y, z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
  | Sub(x, y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | Slli(x, y, i) -> Printf.fprintf oc "\tslli\t%s, %s, %d\n" x y i
  | Srli(x, y, i) -> Printf.fprintf oc "\tsrli\t%s, %s, %d\n" x y i
  | Lw(x, i, y) -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x i y
  | Sw(x, i, y) -> Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x i y
  | Fadd(x, y, z) -> Printf.fprintf oc "\tfadd.s\t%s, %s, %s\n" x y z
  | Fsub(x, y, z) -> Printf.fprintf oc "\tfsub.s\t%s, %s, %s\n" x y z
  | Fmul(x, y, z) -> Printf.fprintf oc "\tfmul.s\t%s, %s, %s\n" x y z
  | Fdiv(x, y, z) -> Printf.fprintf oc "\tfdiv.s\t%s, %s, %s\n" x y z
  | Flw(x, i, y) -> Printf.fprintf oc "\tflw\t%s, %d(%s)\n" x i y
  | Fsw(x, i, y) -> Printf.fprintf oc "\tfsw\t%s, %d(%s)\n" x i y
  | Fsgnj(x, y, z) -> Printf.fprintf oc "\tfsgnj.s\t%s, %s, %s\n" x y z
  | Fsgnjn(x, y, z) -> Printf.fprintf oc "\tfsgnjn.s\t%s, %s, %s\n" x y z
  | Ret -> Printf.fprintf oc "\tret\n"
  | Jalr(_) -> raise (Failure "why closure?")
  | J(Id.L(l)) -> Printf.fprintf oc "\tj\t%s\n" l
  | Jal(Id.L(l)) -> Printf.fprintf oc "\tjal\t%s\n" l
  | Label(Id.L(l)) -> Printf.fprintf oc "%s:\n" l
  | Beq(x, y, Id.L(l)) -> Printf.fprintf oc "\tbeq\t%s, %s, %s\n" x y l
  | Bne(x, y, Id.L(l)) -> Printf.fprintf oc "\tbne\t%s, %s, %s\n" x y l
  | Blt(x, y, Id.L(l)) -> Printf.fprintf oc "\tblt\t%s, %s, %s\n" x y l
  | Feq(x, y, z) -> Printf.fprintf oc "\tfeq.s\t%s, %s, %s\n" x y z
  | Fle(x, y, z) -> Printf.fprintf oc "\tfle.s\t%s, %s, %s\n" x y z

let data_top = ref 100 (* .data starts from memory address 100. *)

let rec gen_global oc x = function
  | ConstArray(len, init) ->
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
  | ConstInt(i) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int i);
    data_top := !data_top + 4
  | ConstBool(b) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (if b then 1 else 0));
    data_top := !data_top + 4
  | ConstFloat(f) ->
    Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.bits_of_float f);
    data_top := !data_top + 4
  | ConstTuple(cs) ->
    let arrays = List.filter (function ConstArray(_) | ConstTuple(_) -> true | _ -> false) cs in
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
        | ConstInt(_) | ConstFloat(_) as c ->
          gen_global oc x c
        | ConstBool(b) -> Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (if b then 1 else 0))
        | ConstArray(_) ->
          Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (List.nth !head_addresses !counter));
          counter := !counter + 1;
        | ConstTuple(_) ->
          Printf.fprintf oc "\t.word\t0x%08lx\n" (Int32.of_int (List.nth !head_addresses !counter));
          counter := !counter + 1)
      cs

let h oc { label = Id.L(x); args = _; fargs = _; body = body; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  List.iter (fun e -> gen_elem oc e) body

let f oc { floats = float_data; globals = array_data; fundefs = fundefs; prog = prog } =
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
  Printf.fprintf oc "\taddi\t%s, sp, 56\n" reg_sp;
  Printf.fprintf oc "\taddi\t%s, sp, 60\n" regs.(0);
  List.iter (fun elem -> gen_elem oc elem) prog;
  Printf.fprintf oc "\taddi\tsp, sp, 52\n";
  Printf.fprintf oc "\tret\n";
