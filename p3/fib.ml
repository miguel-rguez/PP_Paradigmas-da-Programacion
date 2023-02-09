let rec fib n =
	let rec fib2 = function
	  0 -> (0, 1)
	| n -> let (f1,f2) = fib2 (n-1) in
		(f1+f2, f1)
	in
	   fst (fib2 n);;

let f_prin n = 
    let rec imprime i = 
        print_endline (string_of_int (fib i));
        i >= n || imprime(i+1)
    in imprime 0;;

let cfalse =
    Printf.printf "fib: número de argumentos invalidos\n";
    true;;

let y = Array.length Sys.argv in
if y == 2 
    then f_prin (int_of_string Sys.argv.(1))
    else cfalse;