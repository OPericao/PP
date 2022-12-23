let compatibles (i,j) l n = 
  let rec aux pos ll =
    if (pos = List.length l) then ll 
    else let (a,b) = (List.nth l pos) in 
      if (a,b)<>(i,j) && (((b=j && abs(a-i) <= n) || (a=i && abs(b-j) <= n)))             
      then aux (pos+1) ((a,b)::ll) 
      else aux (pos+1) ll 
  in aux 0 [];;
	
let filtrar visitados l = 
  let rec aux ll l2 = match ll with 
    [] -> l2
  | h::t-> if List.mem h visitados then aux t l2
           else aux t (h::l2)
  in aux l [];;
	
let shortest_tour m n l s = 
  let rec aux ll = 
    if ll = [] then raise (Not_found) 
    else let visitados,(i,j) = List.hd ll, List.hd(List.hd ll) in 
      if (i,j) = (m,n) then List.rev visitados
      else let posibles = filtrar visitados (compatibles(i,j) l s) in
        let l2 = (List.map(fun x -> x::visitados) posibles) @ (List.tl ll) in
      aux (List.sort(fun x y -> (List.length x) - (List.length y)) l2) in
  aux [[(1,1)]];;
