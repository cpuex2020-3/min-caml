let rec min_caml_sin v =
  let twopi = 2.0 *. 3.141592 in
  let q = float_of_int(int_of_float(v/.twopi)) in
  let v = v -. twopi *. q in
  v
  -.  (v *. v *. v) /. (3.0 *. 2.0)
  +.  (v *. v *. v *. v *. v) /. (5.0 *. 4.0 *. 3.0 *. 2.0)
  -.  (v *. v *. v *. v *. v *. v *. v) /. (7.0 *. 6.0 *. 5.0 *. 4.0 *. 3.0 *. 2.0) in

let rec min_caml_cos v =
  let twopi = 2.0 *. 3.141592 in
  let q = float_of_int(int_of_float(v/.twopi)) in
  let v = v -. twopi *. q in
  1.0
  -.  (v *. v) /. 2.0
  +.  (v *. v *. v *. v) /. (4.0 *. 3.0 *. 2.0)
  -.  (v *. v *. v *. v *. v *. v) /. (6.0 *. 5.0 *. 4.0 *. 3.0 *. 2.0) in

let tmp = min_caml_sin 3.0 in
print_int(int_of_float tmp);
let tmp = min_caml_cos 3.0 in
print_int(int_of_float tmp);
