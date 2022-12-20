let g n = (n >= 0 && n mod 2 = 0) || n mod 2 = -1;;

let g1 n = if n >= 0 then true else if n mod 2 = 0 then true else n mod 2 = -1;;

let g2 n = (function true | false -> (function true | false -> n mod 2 = -1) (n mod 2 = 0)) (n >= 0);;
