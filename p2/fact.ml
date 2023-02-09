
let rec fact = function
0 -> 1 | n -> n * fact (n - 1);;
  
let y = Array.length Sys.argv in
if y != 2 then Printf.printf "fact: número de argumentos invalidos\n"
else Printf.printf "%d\n" (fact (int_of_string Sys.argv.(1)));
