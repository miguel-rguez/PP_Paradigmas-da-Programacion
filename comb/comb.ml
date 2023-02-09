(*Autor: Miguel Rodriguez Novoa
   login: miguel.rodriguez.novoa@udc.es*)

let aux n = 
    if n > 0 then n
    else 1;;

let rec factnesimo x n = 
    if n > 0 then  x * factnesimo (x-1) (n-1) / aux n
    else 1;;        
   
let rec comb (m, n) = 
    if m < 50 || n < 20 || n > (m-20) then
        if(n < (m/2)) then factnesimo m n 
        else factnesimo m (m-n)
    else if n = 0 || n = m then 1
        else comb(m-1, n -1) + comb(m-1, n);;