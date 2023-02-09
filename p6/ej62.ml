(*  Autor : Miguel Rodriguez Novoa 
 *  login:  miguel.rodriguez.novoa@udc.es *)

let curry f x y = 
  f (x, y)
;;

let uncurry f (x, y) = 
  f x y
;;

uncurry (+);; (* - : int * int -> int = <fun> *)

let sum = (uncurry (+));; (* val sum : int * int -> int = <fun> *)

(*sum 1;; la expresion es de tipo int pero se necesita tipo int * int*)

sum (2, 1);; (* - : int = 3 *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(* g (2, 5);; error de tipo, esperado int -> int, no 'a * 'b*)
g 2 5;; (* - : int = 19 *)

let h = g 2;; (* val h : int -> int = <fun>*)

h 1, h 2, h 3;; (* int * int * int = (7, 10, 13)*)

let comp fa fb a =
  fa (fb (a))
;;

let f = let square x = x * x in comp square ((+) 1);; (*val f : int -> int = <fun> *)

f 1, f 2, f 3;; (* int * int * int = (4, 9, 16) *)

let i a =
  a
;;

let j (a, b) = 
  a
;;

let k (a, b) = 
  b
;; 

let l a = 
  [a]
;;

(* ¿Cuántas funciones se pueden escribir 
  para cada uno de esos tipos?
  
  * Se pueden escribir tantas como tipos se definan, es decir
  puedes escribir tantas funciones que realizen el mismo trabajo 
  que i como tipos de datos hay definidos en ocaml, con la diferencia 
  de que esa funcion solo podra realizer el trabajo con ese tipo 
  especifico de ocaml(o definido por el programador), en contrapartida 
  a la funcion especificada, que utiliza el polimorfismo para abtraer el tipo  

  *)