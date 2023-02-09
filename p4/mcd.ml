let rec mcd (x , y) =
  if x > y then
    if  y > 0 then
      if (x == y) then x
      else mcd(x mod y, y)
    else x
  else mcd (y , x);;
