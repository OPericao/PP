(*Creación de las funciones:*)
let curry =  function f -> function a -> function b -> f (a,b);;
let uncurry =  function f -> function (a,b) -> f a b;;

(*Predicción y comprobación:*)

uncurry (+);;
(* - : int * int -> int = <fun> *)

let sum = (uncurry (+));;
(* val sum : int * int -> int = <fun> *)

(* sum 1;; *)
(* Error: expresión de tipo int pero se esperaba de tipo (int * int) *)

sum (2, 1);;
(* - : int = 3 *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(* g (2, 5);; *)
(* Error: expresión de tipo ('a * 'b) pero se esperaba de tipo int *)

let h = g 2;;
(* val h : int -> int = <fun> *)

h 1, h 2, h 3;;
(* - : int * int * int = (7, 10, 13) *)

(*COMPOSICION*)

let comp f g x = f (g x);;
(* comp: ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b) *)

let f = let square x = x * x in comp square ((+) 1);;
(* val f : int -> int = <fun> *)

f 1, f 2, f 3;;
(* - : int * int * int = (4, 9, 16) *) 

(*POLIMORFISMO*)

let i x = x;;

(*Esta es la única manera de hacerlo*)

let j (x,y) = x;;

(*Esta es la única forma de hacerlo*)

let k (x,y) = y;;

(*Esta es la única forma de hacerlo*)

let l x = [x];;

(*Este tipo se puede escribir de infinitas formas por que let j x = [x];x 
daría también el mismo tipo y podemos poner ;x de forma infinita. Esto hace 
que tengamos infinitas funciones*)
