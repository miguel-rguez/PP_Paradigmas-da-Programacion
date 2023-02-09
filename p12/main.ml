
open Parsing;;
open Lexing;;

open Context;;
open Lib;;
open Arith;;
open Command;;
open Parser;;
open Lexer;;

let rec loop ctx =
  print_string ">> ";
try
  
  let cmd = s token (from_string (read_line ())) in
  loop (run ctx cmd) with
  Lexer.Lexical_error -> print_endline "Lexical error"; loop ctx
  | Parsing.Parse_error -> print_endline "Syntax error"; loop ctx
  | Function_not_defined s -> Printf.printf "Function %s not defined\n" s; loop ctx 
  | No_binding s -> Printf.printf "Variable %s not defined\n" s; loop ctx 
  | End_of_program -> exit
;;
let _ = print_endline "Floating point calculator..." in
let _ = loop empty_context in
print_endline "...bye!!!"

(*

print_endline "... bye!!!";;

let rec loop ctx =
  print_string ">> ";
try
  
  let cmd = s token (from_string (read_line ())) in
  loop (run ctx cmd) with
  Lexer.Lexical_error -> print_endline "Lexical error"; loop ctx
  | Parsing.Parse_error -> print_endline "Syntax error"; loop ctx
  | Function_not_defined s -> Printf.printf "Function %s not defined\n" s; loop ctx 
  | No_binding s -> Printf.printf "Variable %s not defined\n" s; loop ctx 
  | End_of_program -> exit
;;
let _ = print_endline "Floating point calculator..." in
let _ = loop empty_context in
print_endline "...bye!!!"

*)