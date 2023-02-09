let notmem l e =
  not (List.mem e l)
;;

let is_in_board m n (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n
;;

let is_in_tree m n tree (x,y) = 
  if is_in_board m n (x,y) then let rec aux = function
    [] -> false
    | h::t -> if h = (x,y) then true
      else aux t
    in aux tree
  else false 
;;

let legal_moves x y ltrees visited n pos= 
  let all_moves = function
    (x1, y1)-> let rec aux l = function
      nf when nf = n -> (x1+nf, y1)::(x1-nf, y1)::(x1, y1+nf)::(x1, y1-nf)::l
      | n1 -> aux ((x1+n1, y1)::(x1-n1, y1)::(x1, y1+n1)::(x1, y1-n1)::l) (n1+1)
    in aux [] (-n)
  in List.filter (notmem visited) (List.filter (is_in_tree x y ltrees) (all_moves pos))
;;

let first = function
[] -> []
| ( x0, y0)::t -> [x0, y0]

let sec_element = function 
  [] | _::[] -> raise Not_found
  | _::( x0, y0)::t -> (x0, y0)
;;

let marcha_atras = function 
  [] -> raise Not_found
  | h::t -> t 
;;

let tour x y ltress n = 
 
  let rec aux solucion visitados = function
    [] -> raise Not_found
    | h::t -> if (h = ( x, y))
      then List.rev(h::solucion)
      else try aux (h::solucion) (h::visitados) (legal_moves x y ltress visitados n h)
      with Not_found -> if t != [] then aux solucion (h::visitados) t
        else aux (marcha_atras solucion) (h::visitados) (legal_moves x y ltress (h::visitados) n (sec_element solucion))
  in aux [] [] (first ltress)
;;
            
