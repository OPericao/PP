(*si entre as 2 casillas hay n ou menos casillas*)
(*let compatibles (a,b) (c,d) n =        
    if (a=c && (abs (b-d) <= n)) || (b=d && (abs (a-c) <=n)) then true
    else false;;*)
    
let compatibles (a,b) (c,d) n =        
    if (a=c && (abs (b-d) = n)) || (b=d && (abs (a-c) = n)) then true
    else false;;

(*mira a que casilla da lista pode saltar*)  (*deberia facer que empezara mirando antes os saltos de tamano n, e ir baixando a distancia si non hai ningun*)
(*poderia facelo declarando unha funcion auxliar dentro e facendo que a primeira chamada fose con n*)
(*let rec siguiente (a,b) l n = match l with
    [] -> (-1,-1)
  | h::t -> if compatibles (a,b) h n then h
            else siguiente (a,b) t n;;*)
            
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


(*chequear o do try ... with*)

(*esta funcion vai construindo o resultado (a lista cas casillas polas que hay que pasar)*)
(*aproveitar a i e a j para os indices (ACTUALIZACION : creo que non podo facelo asi)*)
(*chamar a compatibles ca casilla na que estou e a siguiente casilla da lista de sombreadas*)
(*mirar q e (i,j))*)

(*let tour m n l s =
  let rec aux camino (i,j) l = match l with     
      [] -> rev camino
      h::t -> aux ((siguiente (i,j))::camino) siguiente(i,j) (remove (siguiente(i,j)) l)                                                                    
  in aux [(1,1)] (1,1) (remove (1,1) l);;*)
 
(*let l2 = remove (i,j) l*)  
  
let tour m n l s =
  let rec aux camino (i,j) l =      
      if (i,j) = (m,n) then rev camino
      else if (siguiente (i,j) (remove (i,j) l) s)=(-1,-1) then aux (remove (i,j) camino) (volver camino) (remove (i,j) l) 
      else aux ((siguiente (i,j) (remove (i,j) l) s)::camino) (siguiente(i,j) (remove (i,j) l) s) (remove(i,j) l)                                                                  
  in aux [(1,1)] (1,1) (remove (1,1) l);;
  
let trees =
[(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
(3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
(6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
(8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
(12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
(15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
(16,16)];;
