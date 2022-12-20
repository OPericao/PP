let m = Sys.argv.(1)

let max = int_of_string(m)


let rec fib = function
	0-> 0
|	1-> 1
|	n-> fib (n-1) + fib (n-2);;


let rec fib2 n = if n<=max
	then	(print_int (fib(n)); print_endline(""); fib2 (n+1));;

fib2(0);;


		







