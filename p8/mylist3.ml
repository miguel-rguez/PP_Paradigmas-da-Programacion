let rev l1 =
  let rec loop l1 l2 = match l1 with
    | [] -> l2
    | h::t -> loop t (h::l2)
  in loop l1 []
;;


(*val remove : 'a -> 'a list -> 'a list*)
let remove e l =
  let rec aux lwe = function
  [] -> lwe
  | h::t when (h = e) -> lwe@t
  | h::t -> aux (h::lwe) t
  in aux [] l
;; 

(*val remove_all : 'a -> 'a list -> 'a list*)

let remove_all e l =
  let rec aux lwe = function
  [] -> lwe
  | h::t when (h = e) -> aux lwe t
  | h::t -> aux (h::lwe) t
  in aux [] (rev l) 
;;

let rec mem a = function
   [] -> false
  | h::t when h = a -> true
  | _::t -> mem a t
;;

(*val ldif : 'a list -> 'a list -> 'a list*)
let ldif l1 l2 = 
  let rec aux lf = function
    [] -> lf
    | h::t when (mem h l2) -> aux lf t
    | h::t -> aux (h::lf) t
  in aux [] (rev l1)
;;

(*val lprod : 'a list -> 'b list -> ('a * 'b) list*)
let lprod lx ly = 
  let rec aux l l1 l2 = match l1,l2 with
  [],_ -> (rev l)
  | h1::t1,[] -> aux l t1 ly
  | h1::t1,h2::t2 -> aux ((h1, h2)::l) l1 t2
  in aux [] lx ly
;;

let divide l = 
  let rec aux li lp n = function
  [] -> (li, lp)
  | h::t when (n mod 2 = 0) -> aux li (h::lp) (n+1) t
  | h::t -> aux (h::li) lp (n+1) t
  in aux [] [] 1 (rev l)
;;