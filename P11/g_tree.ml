type 'a g_tree =
  Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_, []) -> 1
  | Gt (r, h::t) -> size h + size (Gt (r, t));;

let rev l =
	let rec loop l1 = function
	[] -> l1
	|h::t -> loop (h::l1) t  
	in loop [] l;;

let rec map f l = match l with
	[] -> []
	|h::t -> let l2 = map f t in (f h)::l2;;

let rec concat = function
	[] -> []
	|h::t -> h @ concat t;;

let lmax = function
	[] -> raise (Invalid_argument "lmax")
	|h::t -> fold_left max h t;;

let rec preorder = function
	Gt (r,[]) -> r::[]
	|Gt (r, l) -> (r::concat (map preorder l));;
  
let rec heigth = function
	Gt (r,[]) -> 1
	|Gt (r,l) -> 1 + lmax (map heigth l);;
	
let rec leaves = function 
    Gt (r,[]) -> [r]
  | Gt (r,l) ->  concat (map leaves l);;


let rec mirror = function
    Gt (r,[]) -> Gt(r,[])
   |Gt (r,l) -> Gt(r, map mirror (rev l));;

(*let rec postorder = function
    Gt (r,[]) -> [r]
  | Gt (r, h::t) -> (postorder h) @ (postorder (Gt (r,t)));;*)

let rec postorder = function
	Gt (r,[]) -> r::[]
	|Gt (r, l) -> concat (map postorder l)@[r];;
