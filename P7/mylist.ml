let hd = function
	[] -> raise(Failure "hd")
     |h::_ -> h;;
     
     
let tl = function
	[] -> raise(Failure "tl")
	|h::t -> t;;
	
let rec length = function [] -> 0 | _::t -> 1 + length t;;
	
let rec compare_lengths l1 l2 = match (l1,l2) with
	([], []) -> 0
	|([], _) -> -1
	|(_,[]) -> 1
	| (_::t1,_::t2) -> compare_lengths t1 t2;;

let rec nth l n = if n < 0 then raise(Failure "Illegal argument") else match l, n with
	[], _ -> raise(Failure "nth")
	|h::_, 0 -> h
	|_::t, _ -> nth l (n-1);;
	
let rec append l1 l2 = match l1 with
	[] -> l2
	|h::t -> h :: append t l2;;
	
let rec find p l = match l with
	[] -> raise(Not_found)
	|h::t -> if p h then h else find p t;;
	
let rec for_all p = function
	[] -> true
	|h::t -> p h && for_all p t;;
	
let rec exists p = function
	[] -> true
	|h::t -> p h || for_all p t;;
	
let rec mem a set = match set with
	[] -> false
	|h::t -> a = h || mem a t;;
	
let rec filter f l = match l with (*Comprobar funcion f para comprobar*)
	[] -> []
	|h::t -> if f h then h::filter f t else filter f t;;
	
let rec find_all f l = filter f l

let rec partition f l = match l with
	[] -> ([],[])
	|h::t -> let l1,l2 = partition f t in if f h then h::l1,l2 else l1,h::l2;;
	
let rec split = function
     [] -> [],[]
     |(a,b)::t -> let l1,l2 = split t in a::l1, b::l2

let rec combine l1 l2 = match (l1,l2) with
	[],[] -> []
	|[],_|_,[] -> raise(Invalid_argument "combine")
	|h1::t1,h2::t2 -> let l = combine l1 l2 in (h1,h2)::l;;

let rec rev = function
	[] -> []
	|h::t -> (rev t) @ [h];;
	
let init n f =
    if n < 0 then raise (Invalid_argument "init")
    else let rec loop acc i =
        if i = n then rev acc
        else loop (f i::acc) (i+1)
    in loop [] 0;;
	
let rec rev_append l1 l2 = match l1 with
	[] -> l2
	|h::t -> rev_append t (h::l2);;
	
let rec concat = function
	[] -> []
	|h::t -> h @ concat t;;

let rec flatten = concat

let rec map f l = match l with
	[] -> []
	|h::t -> let l2 = map f t in (f h)::l2;; 

let rec rev_map f = function
	[] -> []
	|h::t -> (rev_map f t) @ [f h];;
	

let rec map2 f l1 l2 = match l1,l2 with
	[],[] -> []
	|[],_|_,[] -> raise(Invalid_argument "map2")
	|h1::t1,h2::t2 -> let l3 = map2 f t1 t2 in (f h1 h2)::l3;;

let rec fold_left op e = function
	[] -> e
	|h::t -> fold_left op (op e h) t;;	
	
let rec fold_right op l e = match l with
	[] -> e
	|h::t -> op h (fold_right op t e);;

	
	


















