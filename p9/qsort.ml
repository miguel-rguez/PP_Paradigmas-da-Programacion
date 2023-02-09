let rec qsort1 ord = function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
    qsort1 ord before @ h :: qsort1 ord after
;;

(*¿En qué casos no será bueno el rendimiento de esta implementación?
   Cuando el numero de elementos de la lista provoque el desbordamiento
   por la no terminalidad de @*)

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
  append' (qsort2 ord before) (h :: qsort2 ord after)
;;

(*¿Tiene qsort2 alguna ventaja sobre qsort1?
    Si 
   ¿Permite qsort2 ordenar listas que no podrı́an ordenarse con qsort1?
   Permite ordenar listas de mayor tamaño, aunque con un limite,
   puesto que en la implementacion siquen usandose recursividades no terminales*)

let l1 = List.init 350000 (function x -> Random.int 10000);;

(*¿Tiene qsort2 alguna desventaja sobre qsort1? 
   Compruebe si qsort2 es más lento que qsort1.

   Si, la implementacion de qsort2 tarda mas tiempo, 
   en todas las situaciones posibles de uso para ambas 
   funciones, ya sea con inicilizaciones ya ordenadas, 
   aleatorias y ordenadas de forma inversa. Esto a sido
   comprobado con la implementacion de una funcion cronometro
   que probaba la ejecucion midiendo el tiempo, sobre un
   vector de 10000 elementos inicilizado como ya se ha dicho
   
   Si es ası́, explique por qué y estime la penalización, 
   en porcentaje de tiempo usado, de qsort2 respecto a qsort1.
   
   Con un vector aleatorio de 10000 elementos, qsort2 es, de media,
   un 18.5% mas lento que qsort1. Esto sucede porque en la implementacion
   de qsort2, ademas de realizar la operacion de ordenar, que se realiza
   en un tiempo similar en las dos implemetaciones, tiene que al acabar
   realizar otra operacion, append' enn la que se le da la vuelta dos veces a una 
   parte de la lista l1, y la une con l2, lo que consume un valioso tiempo, a cambio
   de tener recursivada terminal en la funcion que une las dos partes del vector 
   que ordena qsort.
   *)

