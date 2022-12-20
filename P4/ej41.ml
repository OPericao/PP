let rec num_cifras n = if n<10
	then 1
	else num_cifras (n/10)+1;;

let rec sum_cifras n = if num_cifras n <= 1 
	then n 
	else (n mod 10) + (sum_cifras(n/10));;


let rec exp10 n = if n <=1
	then 10
	else 10 * (exp10 (n-1));;


let rec reverse n = if num_cifras n <= 1
	then n
	else ((n mod 10)*(exp10((num_cifras n)-1)) + (reverse(n/10)));;


let rec palindromo s = if (String.length s <= 2) && (s.[0] == s.[(String.length s)-1])
	then true
	else (s.[0] == s.[(String.length s)-1]) && palindromo(String.sub s 1 (String.length (s)-1));;



