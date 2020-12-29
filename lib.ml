let rec kernel_sin x flag =
  let x2 = x *. x in
  let x3 = x *. x2 in
  let x5 = x3 *. x2 in
  let x7 = x5 *. x2 in
  let x13 = x -. 0.16666668 *. x3 in
  let x57 = 0.008332824 *. x5 -. 0.00019587841 *. x7 in
  fsgnj (x13 +. x57) flag
in


let rec kernel_cos x flag =
  let x2 = x *. x in
  let x4 = x2 *. x2 in
  let x6 = x4 *. x2 in
  let x02 = 1.0 -. 0.5 *. x2 in
  let x46 = 0.04166368 *. x4 -. 0.0013695068 *. x6 in
  fsgnj (x02 +. x46) flag
in

let rec reduction_2pi x p is_p_updating =
  if is_p_updating then
    if x >= p then
      reduction_2pi x (p *. 2.) true
    else if x >= 3.14159265 *. 2. then
      if x >= p then
        reduction_2pi (x -. p) (p /. 2.) false
      else
        reduction_2pi x (p /. 2.) false
    else
      x
  else
  if x >= 3.14159265 *. 2. then
    if x >= p then
      reduction_2pi (x -. p) (p /. 2.) false
    else
      reduction_2pi x (p /. 2.) false
  else
    x
in


let rec sin x_orig =
  let x = reduction_2pi (fabs x_orig) (2. *. 3.14159265) true in
  if x >= 3.14159265 then
    (if x >= 3.14159265 /. 2. then
       (if x >= 3.14159265 /. 4. *. 7. then
          kernel_sin (3.14159265 *. 2. -. x) (-.x_orig)
        else
          kernel_cos (x -. 3.14159265 *. 3. /. 2.) (-.x_orig))
     else
       (if x >= 3.14159265 /. 4. *. 5. then
          kernel_sin (x -. 3.14159265) (-. x_orig)
        else
          kernel_cos (3.14159265 *. 3. /. 2. -. x) (-. x_orig)))
  else
    (if x >= 3.14159265 /. 2. then
       (if x >= 3.14159265 /. 4. *. 3. then
          kernel_sin (3.14159265 -. x) x_orig
        else
          kernel_cos (x -. 3.14159265 /. 2.) x_orig)
     else
       (if x >= 3.14159265 /. 4. then
          kernel_sin x x_orig
        else kernel_cos (3.14159265 /. 2. -. x) x_orig))
in

let rec cos x =
  let x = reduction_2pi (fabs x) (2. *. 3.14159265) true in
  if x >= 3.14159265 /. 2. then
    (if x >= 3.14159265 /. 2. *. 3. then
       (if x >= 3.14159265 *. 7. /. 4. then
          kernel_cos (2. *. 3.14159265 -. x) (-.1.0)
        else
          kernel_sin (x -. 3.14159265 *. 7. /. 4.) (-.1.0))
     else
       (if x <= 3.14159265 /. 5. *. 4. then
          kernel_cos (x -. 3.14159265) 1.0
        else
          kernel_sin (3.14159265 *. 3. /. 2. -. x) 1.0))
  else
    (if x >= 3.14159265 /. 2. then
       (if x >= 3.14159265 *. 3. /. 4. then
          kernel_cos (3.14159265 -. x) (-.1.0)
        else
          kernel_sin(x -. 3.14159265 /. 2.) (-.1.0))
     else
       (if x <= 3.14159265 /. 4. then
          kernel_cos x 1.0
        else
          kernel_sin (3.14159265 /. 2. -. x) 1.0))
in

let rec kernel_atan x app flag =
  let x2 = x *. x in
  let x3 = x *. x2 in
  let x5 = x3 *. x2 in
  let x7 = x5 *. x2 in
  let x9 = x7 *. x2 in
  let x11 = x9 *. x2 in
  let x13 = x11 *. x2 in
  let x_13 = x -. 0.3333333 *. x3 in
  let x57 = 0.2 *. x5 -. 0.142857142 *. x7 in
  let x913 = 0.111111104 *. x9 -. 0.08976446 *. x11 +. 0.060035485 *. x13 in
  fsgnj (x_13 +. x57 +. x913) flag
in

let rec atan x =
  let a = fabs x in
  if a < 0.4375 then
    kernel_atan x 0.0 x
  else if a < 2.4375 then
    kernel_atan ((a -. 1.0) /. (a +. 1.0)) (3.14159265 /. 4.) x
  else
    kernel_atan (1.0 /. a) (-.3.14159265 /. 2.) x
in
