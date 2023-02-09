let rec divide l = match l with
  h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
  | _ -> l, []
;;

let rec merge = function
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
  else h2 :: merge (h1::t1, t2)
;;

let rec msort1 l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
    merge (msort1 l1, msort1 l2)
;;

(*¿Puede provocar algún problema la no terminalidad de divide o merge?
   
 * SI,  en ambos casos ya que al no emplear recursivadad terminal la pila
 de ejecucion del programa se llenara con facilidad de llamadas recursivas
 con operaciones h1 :: merge .... o h2 :: merge .... sin realizar.

*)

let l2 = List.init 400000 (function x -> Random.int 100);;

let divide' list = 
  let rec aux t1 t2 = function
    [] -> (t1, t2)
    | h::[] -> (h::t1, t2)
    | h1::h2::t -> aux (h1::t1) (h2::t2) t
  in aux [] [] (List.rev list) 
;;

let merge' ord (l1, l2) =
  let rec aux (a1, a2) mer = match a1, a2 with
    [], l | l, [] -> List.rev_append mer l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux (t1, h2::t2) (h1::mer)
      else aux (h1::t1, t2) (h2::mer)
  in aux (l1, l2) [];;

let rec msort2 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide' l
  in merge' ord (msort2 ord l1, msort2 ord l2)
;;

(*Compare el rendimiento en tiempo de ejecución de msort2 con el de msort1
    y con el de qsort2.

    Funcion utilizada para medir los tiempos
    
let crono f x = 
  let t = Sys.time () in
    f x;
    Sys.time () -. t
;;

    Tiempos obtenidos para la siguiente lista:

let l = List.init 10000 (function x -> Random.int 100000);;

crono msort1 l;;                - : float = 0.017009000000000718
    
crono (msort2 (<=)) l;;         - : float = 0.0243039999999998813

crono (qsort2 (<=)) l;;         - : float = 0.0217610000000005854
    
    *)