type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);; 

let inorder t = let fInorder x l r = l @ x::r in fold_tree fInorder [] t;;

let rec insert_tree ord n = function
	Empty -> Node(n,Empty,Empty)
	|Node(x,l,r) -> if n<=x then Node(x,insert_tree ord n l,r) else Node(x,l,insert_tree ord n r);;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l);;

