(*Un valor u de tipo int a partir de una expresión que contenga, al menos, 4 operadores infijos.*)
let u = 5 + 4 *(-3)/4;;

(*Un valor v de tipo float a partir de una expresión que incluya una función predefinida.*)
let v = float_of_string "16.35";;

(*Un valor w de tipo char a partir de una expresión que incluya una sub-expresión de tipo int.*)
let w = Char.chr 126;;

(*Un valor w de tipo char a partir de una expresión que incluya una sub-expresión de tipo int.*)
let x = true && 12 = 3 * int_of_float 3.0;;

(*Un valor y de tipo string a partir de una expresión que contenga una frase if-then-else.*)
let y = if 3 = int_of_float 3.0 then "Correcto" else "Incorrecto";;
