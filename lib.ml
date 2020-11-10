let rec min_caml_sin x =
  let rec sub_sin c cur last is_neg =
    if c >= 9 then
      cur
    else
      let last = last *. x *. x /. float_of_int (c + 2) /. float_of_int (c + 1) in
      if is_neg then
        sub_sin (c + 2) (cur -. last) last false
      else
        sub_sin (c + 2) (cur +. last) last true
  in
  sub_sin 1 x x true
in
print_int (int_of_float (min_caml_sin 1.57))
