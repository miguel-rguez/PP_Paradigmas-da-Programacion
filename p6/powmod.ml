(*  Autor : Miguel Rodriguez Novoa 
 *  login:  miguel.rodriguez.novoa@udc.es *)

let powmod m b e = 
  let rec aux x n e =
    if e = 0 then n
    else aux x (n * x mod m ) (e-1)
  in aux (b mod m) 1 e
;;