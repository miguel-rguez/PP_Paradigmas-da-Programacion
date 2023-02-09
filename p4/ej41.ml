let rec sum_cifras n =
  if n /10 =  0 then n mod 10
  else n mod 10 + sum_cifras (n/10);;

let rec num_cifras n =
  if n/10 = 0 then 1
  else 1+num_cifras(n/10);;

let rec exp10 n =
  if n = 0 then 1
  else 10 * exp10(n-1);;

let rec reverse n =
  if (n / 10 != 0)
  then (n mod 10)*exp10 (num_cifras n)/10 + reverse (n/10)
  else n;;

let rec palindromo s =
  if (String.length s)>=2
  then if (s.[0] = s.[((String.length s)-1)])
    then palindromo (String.sub s 1 ((String.length s)-2))
    else false
  else true;;
