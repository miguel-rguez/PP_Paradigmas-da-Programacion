(*  Autor : Miguel Rodriguez Novoa 
 *  login:  miguel.rodriguez.novoa@udc.es *)

let hd = function
  [] -> failwith "hd"
  | h::_ -> h
;;

let tl = function
  [] -> failwith "tl"
  | _::t -> t
;;


let length l = 
  let rec aux n = function
    [] -> n
	  | _::t-> aux (n+1) t
  in aux 0 l
;;

let rec compare_lengths l1 l2 = match (l1, l2) with
	([], []) -> 0
	| ([], _) -> -1
	| (_, []) -> 1
	| (_::t1, _::t2) -> compare_lengths t1 t2
;;

let rec nth  l p = match l,p with
   _,p when p < 0 -> raise (Invalid_argument "nth")
  | [],_ -> raise (Failure "nth")
  | h::_,0 -> h
  | _::t,p -> nth t (p-1)
;;

let rec append l1 l2 = match l1 with
   [] -> l2
  |  h::t-> h::append t l2
;;

(*Redefina las funciones siguientes del módulo *)
(*List, sin utilizar ese módulo*)

let rec find p = function
   [] -> raise (Not_found) 
  | h::_ when p h -> h
  | _::t -> find p t
;;

let for_all p l =
  let rec aux b = function
    [] -> b
    | h::t -> aux ((p h) && b) t 
  in aux true l
;; 

let exists f l = 
  let rec aux b lf = match lf,b with
   [],_ -> b
  | _,b when b -> b
  | h::t,_ -> aux (b || (f h)) t
  in aux false l 
;;

let rec mem a = function
   [] -> false
  | h::t when h = a -> true
  | _::t -> mem a t
;;

let rev l1 =
  let rec loop l1 l2 = match l1 with
    | [] -> l2
    | h::t -> loop t (h::l2)
  in loop l1 []
;;

let filter f l = 
  let rec aux l1 = function
    [] -> l1
    | h::t when (f h) -> aux (h::l1) t
    | _::t -> aux l1 t
  in aux [] (rev l)
;;

let find_all = filter;;

let partition p l =
  let rec aux (lt, lf) = function
    [] -> (lt, lf)
    | h::t when (p h) -> aux (h::lt, lf) t
    | h::t -> aux (lt, (h::lf)) t
  in aux ([], []) (rev l)
;;

let rec split = function
  [] -> ([],[])
  | (h1,h2)::t -> let t1,t2 = split t 
  in h1::t1,h2::t2
;;

let rec combine l1 l2 = match (l1,l2) with
  [], [] -> []
  | h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
  | _ -> raise (Invalid_argument "combine")
;;

let rev_append l1 l2 =
  let rec aux l = function 
    [] -> l
    | h::t -> aux (h::l) t
  in aux l2 l1
;; 

let init n f = 
  if n<0 then raise (Invalid_argument "init")
  else let rec aux acc i = 
    if i = n then rev acc
    else aux (f i::acc) (i+1)
  in aux [] 0
;;

let rec append a b = match a with
  | [] -> b
  | h :: t -> h :: append t b
;;

let rec concat = function
  [] -> []
  | [[]] -> []
  | x::l -> x@(concat l)
;;

let flatten = concat;;

let rec map f = function
   [] -> []
  | h :: t -> f h :: map f t
;;

let rev_map f l = 
  let rec aux l1 = function
  [] -> l1
  | h::t -> aux ((f h)::l1) t
  in aux [] l
;;

let rec map2 f l1 l2 = 
  if (length l1 != length l2)
    then raise (Invalid_argument "map2")
  else if (length l1 == 0) then []
  else (f (hd l1) (hd l2))::map2 f (tl l1) (tl l2)
;;

let rec fold_left f a = function
| [] -> a
| h::t -> fold_left f (f a h) t
;;

let rec fold_right f l b = match l with
  | [] -> b
  | h::t -> f h (fold_right f t b)
;;
