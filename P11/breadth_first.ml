type 'a g_tree =
  Gt of 'a * 'a g_tree list;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

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

let append l1 l2 = 
	let rec loop l1 = function
		[] -> rev l1
		|h::t -> loop (h::l1) t
	in loop (rev l1) l2;;

let breadth_first_t gt = 
	let rec loop l = function
	Gt (x, []) -> rev (x::l)
	|Gt (x, (Gt (y, t2))::t1) -> loop (x::l) (Gt(y, append t1 t2))
	in loop [] gt;;

let t = Gt (2,[Gt (7, [Gt (2, []); Gt (10, []);
Gt (6, [Gt (5, []); Gt (11, [])])]);
Gt (5, [Gt (9, [Gt (4, [])])])]);;

let t2 = ... ;;

