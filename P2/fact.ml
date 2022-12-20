let m = Sys.argv.(1)

let n = int_of_string(m)

let rec fact = function 0 -> 1 | n -> n * fact (n - 1);;

if Array.length(Sys.argv) != 2 then print_string("Nº argumentos invalido") else print_int(fact(n));;

print_endline("");;



