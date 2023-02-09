let g1 n = (if n>= 0  then n mod 2 == 0 else n mod 2 == -1)
let g2 n = (function true -> n mod 2 == 0 | false -> n mod 2 == -1)(n >= 0)
