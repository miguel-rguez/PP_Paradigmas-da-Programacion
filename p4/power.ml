let rec power x y =
  if y > 0 then x * power x (y-1)
  else 1;;

let rec power' x y =
if y > 0 then if y mod 2 = 0
  then power' (x*x) (y/2)
  else x * power (x*x) (y/2)
else 1;;

let rec powerf x y =
  if y > 0 then x *. powerf x (y-1)
  else 1.;;
