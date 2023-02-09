

let shortest_tour x y ltrees n =
  let is_in_board (x, y) = x >= 1 && x <= x && y >= 1 && y <= y in
  let is_in_tree (x, y) = List.mem (x, y) ltrees in
  let legal_moves (x, y) visited =
    let all_moves (x, y) =
      let rec aux l = function
        nf when nf = n -> (x+nf, y)::(x-nf, y)::(x, y+nf)::(x, y-nf)::l
        | n1 -> aux ((x+n1, y)::(x-n1, y)::(x, y+n1)::(x, y-n1)::l) (n1+1)
      in aux [] (-n)
    in List.filter (fun pos -> is_in_board pos && is_in_tree pos && not (List.mem pos visited)) (all_moves (x, y))
  in
  let rec aux queue visited =
    match queue with
    | [] -> raise Not_found
    | (pos, path)::t ->
      if pos = (x, y) then List.rev path
      else
        let neighbors = legal_moves pos visited in
        aux (List.rev_append (List.map (fun pos -> (pos, pos::path)) neighbors) t) (pos::visited)
  in
  aux [((1, 1), [])] []
;;

let shortest_tour x y ltrees n =
  let is_in_board (x, y) = x >= 1 && x <= x && y >= 1 && y <= y in
  let is_in_tree (x, y) = List.mem (x, y) ltrees in
  let legal_moves (x, y) visited =
    let all_moves (x, y) =
      let rec aux l = function
        nf when nf = n -> (x+nf, y)::(x-nf, y)::(x, y+nf)::(x, y-nf)::l
        | n1 -> aux ((x+n1, y)::(x-n1, y)::(x, y+n1)::(x, y-n1)::l) (n1+1)
      in aux [] (-n)
    in List.filter (fun pos -> is_in_board pos && is_in_tree pos && not (List.mem pos visited)) (all_moves (x, y))
  in
  let heuristic (x, y) = (abs (x - x) + abs (y - y)) in
  let rec aux queue visited =
    match queue with
    | [] -> raise Not_found
    | (pos, path)::t ->
      if pos = (x, y) then List.rev path
      else
        let neighbors = legal_moves pos visited in
        let neighbors_with_costs = List.map (fun pos -> (pos, heuristic pos + List.length path)) neighbors in
        let sorted_neighbors = List.sort (fun (_, c1) (_, c2) -> compare c1 c2) neighbors_with_costs in
        aux (List.rev_append sorted_neighbors t) (List.rev_append neighbors visited)
in
aux [((1, 1), [(1, 1)])] []
