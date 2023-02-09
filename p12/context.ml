
type 'a context =
  (string * 'a) list;;

exception No_binding of string;;

let empty_context = [];;

let get_binding ctx name =
  try List.assoc name ctx with
  Not_found -> raise (No_binding name)
;;


(* Asigna valores a nombres de variables y los añade a context*)
let rec add_binding ctx name = function
   a when List.mem_assoc name ctx -> a ; (name, a)::(List.remove_assoc name ctx)
   | a -> a ; (name, a)::ctx
;;
