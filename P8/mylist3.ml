let rec remove a l = match l with
	[] -> []
  | h::t -> if h=a then t
  			else h :: remove a t;;

let rec remove_all a l = match l with		
	[] -> []
  | h::t -> if h=a then remove_all a t
  			else h :: remove_all a t;;
  			
let rec ldif l1 l2 = match (l1,l2) with				
	(l1,[]) -> l1
  | ([],l2) -> []
  | (h1::t1,h2::t2) -> ldif (remove_all h2 l1) t2;;
  
let rec eprod a l = match l with
	[] -> []
  | h::t -> (a,h) :: eprod a t;;
	  
let rec lprod l1 l2 = match l1 with
	[] -> []
  | h::t -> (eprod h l2) @ lprod t l2;;	
  
let length l = 							
	let rec aux n = function
		[] -> n
	  | h::t -> aux (n+1) t
	in aux 0 l;;				

let rec divide = function						
	[] -> ([],[])
  | h::t -> let (l1,l2) = divide t in
  			if length t mod 2 == 1 then (h::l1,l2)
			else (l1,h::l2);;
