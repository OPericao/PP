let rec divide l = match l with
	h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;

let rec merge = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
					else h2 :: merge (h1::t1, t2);;

let rec msort1 l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l in
		merge (msort1 l1, msort1 l2);;
	
(*Si que lo provocaría, si la lista que le pasamos a msort1 es muy grande, la que le llegaria a divide
 y a merge tambien lo seria, y al no ser recursivas terminales derivarían en un fallo en el stack*)

let l2 = List.init 1_000_000 abs;;

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
	
let divide' l =
	let rec loop l1 l2 = function
		[] -> rev l1, rev l2
		|h1::h2::t -> loop (h1::l1) (h2::l2) t
		|h1::[] -> loop (h1::l1) l2 [] 
	in loop [] [] l;;

let merge' p (l1, l2) =
	let rec loop l l1 l2 = match l1, l2 with 
		[], aux | aux, [] -> rev (rev_append aux l) 
		|h1::t1, h2::t2 -> if p h1 h2 then loop (h1::l) t1 (h2::t2) else loop (h2::l) (h1::t1) t2
	in loop [] l1 l2;;
	
let rec msort2 p l = match l with
		[] | _::[] -> l
		|_ -> let l1, l2 = divide' l in merge' p (msort2 p l1, msort2 p l2);;

(*Para una lista de 100_000 elementos aleatorios, tanto msort1 como msort2 tardan practicamente lo mismo, 0.147378 y 0.147943 respectivamente.
  Ambos tiempos son menores que el de qsort2, que tarda 0.176721*)
		
		
		
		
		
		
		
		
