
open Context;;
open Lib;;

type arith_oper =
    Opp;;

type arith_bi_oper =
    Sum | Sub | Prod | Div | Mod | Pow;;

type arith =
    Float of float
  | Var of string
  | Arith_op of arith_oper * arith
  | Arith_bi_op of arith_bi_oper * arith * arith
  | Fun_call of string * arith;;

let rec eval ctx = function
    Float f ->
      f

  | Var name ->
      get_binding ctx name

  | Arith_op (op, e) ->
    let f = eval ctx e in
      (match op with Opp -> -.f)
  | Arith_bi_op (op, e1, e2) ->
    let f1 = eval ctx e1 and f2 = eval ctx e2 in
    (match op with
      | Sum -> f1 +. f2
      | Sub -> f1 -. f2
      | Prod -> f1 *. f2
      | Div -> f1 /. f2
      | Mod -> mod_float f1 f2
      | Pow -> f1 ** f2)
  | Fun_call (str, p) -> (get_function str) (eval ctx p)
;;

