let compatibles (a,b) (c,d) n =        
    if (a=c && (abs (b-d) = n)) || (b=d && (abs (a-c) = n)) then true
    else false;;
            
let siguiente (a,b) l m = 
  let rec aux (a,b) ll n = match ll,n with
    [],0 -> (-1,-1)
  | [],n -> aux (a,b) l (n-1) (*fallo*)
  | h::t,n -> if compatibles (a,b) h n then h  
              else aux (a,b) t n
  in aux (a,b) l m;;
  
let rev_append l1 l2 =						
	let rec aux l2 = function		
		[] -> l2
  		|h::t -> aux (h::l2) t
 	 in aux l2 l1;;
 	 
let remove x l = 
    let rec aux x l1 = function
	    [] -> l1
	  | h::t -> if h = x then aux x (rev_append l1 t) [] else aux x (h::l1) t 	    
    in aux x [] l;;
    
let volver camino = match camino with
	[] -> (-1,-1)
	|h1::[] -> (-1,-1)
	|h1::h2::t -> h2;;

let rev l= 									
	let rec aux ll = function		
		[] -> ll
     	|h::t -> aux (h::ll) t
	in aux [] l;;
 
let tour m n l s =
  let rec aux camino (i,j) l =  
  let l2 = (remove (i,j) l) in   
      if (i,j) = (m,n) then rev camino
      else if (siguiente (i,j) l2 s)=(-1,-1) && (i,j) = (1,1) then raise(Not_found)
      else if (siguiente (i,j) l2 s)=(-1,-1) then aux (remove (i,j) camino) (volver camino) l2
      else aux ((siguiente (i,j) l2 s)::camino) (siguiente(i,j) l2 s) l2                                                                 
  in aux [(1,1)] (1,1) (remove (1,1) l);;
