let rec power x y = if y < 2
	then x
	else x * power x (y-1);;

let rec power' x y = if y mod 2 == 0 (*SE REALIZAN LA MITAD DE ITERACIONES, Y/2*)
	then power' (x*x) (y/2)
	else x*power'(x*x) (y/2);;
	

let rec powerf k y = if y < 2
	then k
	else k*.powerf k (y-1);;
