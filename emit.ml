open Asm
open ConstExp

let emit oc = function
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
  | Word(hex) -> Printf.fprintf oc "\t.word\t%s\n" hex

let h oc { label = Id.L(x); args = _; fargs = _; body = body; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  List.iter (fun e -> emit oc e) body

let f oc { data = data; fundefs = fundefs; prog = prog } =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\t.data\n";
  List.iter (fun word -> emit oc word) data;
  Printf.fprintf oc "\t.text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "\t.globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  if !is_core then
    (Printf.fprintf oc "\tli\t%s, 170\n" reg_buf;
     Printf.fprintf oc "\ttxbu\t%s\n" reg_buf);
  Printf.fprintf oc "\taddi\tsp, sp, -52\n";
  Printf.fprintf oc "\taddi\t%s, sp, 56\n" reg_sp;
  Printf.fprintf oc "\taddi\t%s, sp, 60\n" regs.(0);
  List.iter (fun elem -> emit oc elem) prog;
  Printf.fprintf oc "\taddi\tsp, sp, 52\n";
  Printf.fprintf oc "\tret\n";
