(*  Autor : Miguel Rodriguez Novoa 
 *  login:  miguel.rodriguez.novoa@udc.es *)

let f n = 
  if  n mod 2 = 0 then n / 2
  else 3 * n + 1
;;

let rec orbit n = 
  if n = 1 
  then "1"
  else (string_of_int n) ^ ", " ^ orbit (f n)
;;

let rec length n =
  if n = 1 then 0
  else length (f n) + 1
;;  

let rec top n =
  if n > 1 then max (top(f n)) n
  else n
;;

let rec length'n'top n =
  if n > 1 then let (l,t) = length'n'top(f n) in (l+1, (max t n ))
  else (0,1)
;;

let rec longest_in a b = 
  if a < b then let (m , lm)  = longest_in (a+1) b 
  in let la = length a in (if la < lm then ( m, lm) else (a, la))
  else (b , length b)
;;

let rec highest_in a b = 
  if a < b then let (m , tm)  = highest_in (a+1) b 
  in let ta = top a in (if ta < tm then ( m, tm) else (a, ta))
  else (b , top b)
;;
