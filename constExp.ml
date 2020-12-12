type t =
  | ConstInt of int
  | ConstBool of bool
  | ConstFloat of float
  | ConstTuple of t list
  | ConstArray of int * t (* length * init *)
