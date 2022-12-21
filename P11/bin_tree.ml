type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let t = Node (3, Node (8, Empty, Empty),
Node (2, Node (5, Empty, Empty),
Node (1, Empty, Empty)));;

let rec map_tree f = function
	Empty -> Empty
	|Node (x, l, r) -> Node (f x, map_tree f l, map_tree f r);;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);; 

let sum t = let fSum x l r = x+l+r in fold_tree fSum 0 t;;

let prod t = let fProd x l r = x*.l*.r in fold_tree fProd 1. t;;   

let size t = let fSize x l r = 1 + l + r in fold_tree fSize 0 t;;

let height t = let fHeight x l r = 1 + max l r in fold_tree fHeight 0 t;;

let inorder t = let fInorder x l r = l @ x::r in fold_tree fInorder [] t;;

let mirror t = let fMirror x l r = Node(x,r,l) in fold_tree fMirror Empty t;;

