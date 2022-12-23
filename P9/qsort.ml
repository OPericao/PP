let rec qsort1 ord = function
	[] -> []
	| h::t -> let after, before = List.partition (ord h) t in
		qsort1 ord before @ h :: qsort1 ord after;;

let rec qsort2 ord =
	let append' l1 l2 = List.rev_append (List.rev l1) l2 in
	function
		[] -> []
		| h::t -> let after, before = List.partition (ord h) t in
			append' (qsort2 ord before) (h :: qsort2 ord after);;

(*qsort2 es una funcion recursiva terminal, por lo que para listas muy grandes,
 qsort2 seguirá funcionando mientras qsrot1 dara un fallo en el stack*)
 
let crono f x =
    let t = Sys.time () in
    let _ = f x in
    Sys.time () -. t;;
 
let rlist n = List.init n (fun _ -> Random.int n);;

(*qsort1 es más rapido que qsort2. Para una lista de 100_000 aleatorios, qsort1 tarda 0.153669
mientras que qsort2 0.166095. Esto se debe a que qsort1 es una función más simple, mientras que
qsort2 esta hecha para ser recursiva terminal y es mas compleja. Qsort2 tiene una penalización
de 8.086% sobre qsort1*)

let l1 = List.init 1_000_000 abs;;


