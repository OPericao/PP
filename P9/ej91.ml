let rev l =
	let rec loop l1 = function
	[] -> l1
	|h::t -> loop (h::l1) t  
	in loop [] l;;
	
let rev_append l1 l2 = 
	let rec loop l2 = function
	[] -> l2
	|h::t -> loop (h::l2) t
	in loop l2 l1;;

let to0from n =
	let rec loop l aux =
		if aux>n then l
		else loop (aux::l) (aux+1)
	in loop [] 0;;
	
let fromto m n =
	let rec loop auxM auxN l =
		if auxM < auxN then l
		else loop (auxM-1) auxN (auxM::l)
	in loop n m [];;
	
let incseg l =
	let rec loop l1 l = match l1,l with
	_,[] -> rev l1
	|[],h::t -> loop (h::l1) t
	|h1::t1,h::t -> loop ((h1+h)::l1) t
	in loop [] l;; 
	
let remove x l = 
	let rec loop x l1 = function
		[] -> l1
		|h::t -> if x = h then loop x (rev_append l1 t) [] else loop x (h::l1) t
	in loop x [] l;;
	
let compress l = 
	let rec loop l1 = function
		[] -> rev l1
		|h1::[] -> loop (h1::l1) []
		|h1::h2::t -> if h1 = h2 then loop (h2::l1) t else loop (h1::l1) (h2::t)
	in loop [] l;;


















































