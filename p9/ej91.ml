let to0from n =
  let rec aux i l = 
    if i > n  then l
    else aux (i+1) (i::l)
  in aux 0 []
;;

let fromto m n =
  let rec aux i l = 
    if i < m then l
    else aux (i-1) (i::l)
  in aux n []
;;

let incseg l = 
  let rec aux l1 c = function
    [] -> List.rev l1
    | h::t -> aux ((h+c)::l1) (c+h) t
  in aux [] 0 l
;;

let remove x l = 
  let rec aux l1 b l2 = match l2,b with 
  [],_ -> List.rev l1
  | h::t,b when b -> aux (h::l1) b t
  | h::t,b when ( h = x )-> aux l1 true t
  | h::t,_  -> aux (h::l1) b t
  in aux [] false l
;;

let compress l = 
  let rec aux lc = function
    [] -> lc
    | h::[] -> h::lc
    | h1::h2::t -> if h1 = h2 then aux lc (h2::t)
      else aux (h1::lc) (h2::t) 
  in aux [] (List.rev l)
;;