(*  Autor : Miguel Rodriguez Novoa 
 *  login:  miguel.rodriguez.novoa@udc.es *)

(*  Intente realizar todas las implementaciones 
 *  directamente con pattern matching*)

(*Redefine las siguientes funciones del Modulo List*)
(*sin utilizar el módulo list ni la funcion @ de Stdlib*)

let hd = function
  [] -> failwith "lista sin cabeza"
  | h::_ -> h
;;

let tl = function
  [] -> failwith "lista sin cola"
  | _::t -> t
;;


let rec length = function 
	[] -> 0
	| _::t-> 1 + length t
;;

let rec compare_lengths l1 l2 = match (l1, l2) with
	([], []) -> 0
	| ([], _) -> -1
	| (_, []) -> 1
	| (_::t1, _::t2) -> compare_lengths t1 t2
;;

let rec nth  l p = match l,p with
   _,p when p < 0 -> raise (Invalid_argument "lista_length")
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
   [] -> failwith "Not_found" 
  | h::t -> if p h then h else find p t
;;

let rec for_all p = function
   [] -> true
  | h::t -> (p h) && (for_all p t);;

let rec exists f = function 
   [] -> false
  | x::l -> (f x) || exists f l
;;

let rec mem a = function
   [] -> false
  | h::t when h = a ->true
  | _::t -> mem a t;;

let rec filter p = function
   [] -> []
  | h::t -> if (p h) then h::(filter p t)
  else (filter p t)
;;

let find_all = filter;;

let rec partition p = function
  [] -> ([],[])
  | h::t -> let (x, y) = partition p t
  in if (p h) then (h::x,y)
  else (x, h::y)
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

(*  Redefina las funciones siguientes del módulo List,
 *  sin utilizar ese módulo.*)

let rev l1 =
  let rec loop l1 l2 = match l1 with
    | [] -> l2
    | h::t -> loop t (h::l2)
  in loop l1 []
;;

let rev_append l1 l2 = append (rev l1) (l2) ;; 

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

let rec map f l = match l with
   [] -> []
  | h :: t -> f h :: map f t
;;

let rev_map f l = 
  rev (map f l)
;;

let rec map2 f l1 l2 = 
  if (length l1 != length l2)
    then raise (Invalid_argument"map2")
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
