(***********************************************************************)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)
open Bsml
(** Fonctions usuelles  *)
let replicate a =mkpar (fun _ -> a)
let parfun f v = apply (replicate f) v
let procs = 
  let rec aux i l = 
    if i<=0 then 
      0::l else 
      aux (i-1) (i::l) 
  in aux (bsp_p -1) []
  ;;

let id x  =x ;;
(*
let this = (* à compléter *)

(** Parallel implementation of parallel arrays *)

(** {2 Parallel array type }  *)
*)
type 'a parallel_array =    'a array par;;           
(* à compléter *)
(** Type of parallel arrays  *)


(* offset and length *)
let ofs_len n pid =
  if(pid >= bsp_p)then failwith "le processeur n'existe pas !"
  else
  let ofs  = if(n=0)then (-1)
	      else if(n <= bsp_p)then if(pid >=n)then (-1) else pid
		   else  if(pid >= (n mod bsp_p)) then (n/bsp_p)*pid + (n mod bsp_p)
			    else (n/bsp_p)*pid + pid
  and length  = if(n=0)then 0
		else if(n <= bsp_p) then if(pid >=n)then 0 else 1
		      else if(pid >= (n mod bsp_p)) then (n/bsp_p)
			    else ((n/bsp_p)+1)
  in (ofs , length)
;;

(** {2 Parallel array constructors} *)

let  make : int -> 'a -> 'a parallel_array =
  fun n v -> 
  match n with
  |0 -> mkpar(fun i -> [||])
  |_ -> if(n<0)then failwith "Raise Invalid argument" 
		else mkpar(fun i -> Array.make (snd (ofs_len n i)) v);;		
    (* à compéter *)

let test = (make 10 1);;
let test = (make 0 1);;
(*let test = (make (-1) 1);;*)

let rec initlocal n ofs f = 
  match n with 
  |0 -> [||]
  |_ -> Array.append [|f ofs|] (initlocal (n-1) (ofs + 1) f);;
  
let init : int -> (int -> 'a) -> 'a parallel_array =
  fun n f -> 
  match n with
  |0 -> mkpar(fun i -> [||])
  |_ -> if(n<0)then failwith "Raise Invalid argument" 
		else mkpar(fun i -> initlocal (snd (ofs_len n i)) (fst(ofs_len n i)) f );;

 
let test = (init 10 id);;
let test = (init 0 id);;

let copy : 'a parallel_array -> 'a parallel_array =
  fun ap -> mkpar(fun i -> proj ap i);;
  
copy (make 10 3);;

let of_array : 'a array -> 'a parallel_array =
  fun a ->
  if (Array.length a < bsp_p) then mkpar(fun i -> 
				if(i < (Array.length a) ) then Array.make 1 a.(i)
				else [||])
  else
  mkpar(fun i -> Array.sub a (fst(ofs_len (Array.length a) i)) (snd (ofs_len (Array.length a) i)))
  ;;
  (* à completer  *)

of_array [|1;2;3;4;5;6;6;8;9;10;11|];;
let test = of_array (Array.init 10 id);;
let test = of_array (Array.init 1 id);;

let rec for_array n ap =
 match n with 
 | 0 -> proj ap 0
 | _ -> Array.append (for_array (n-1) ap) (proj ap n);;  
 
let to_array : 'a parallel_array -> 'a array =
  fun ap -> for_array (bsp_p -1) ap;;
  
(** [to_array a] returns a fresh array containing the elements
   of [a]. 
*)

let test = to_array (init 10 id);;
let test = to_array (init 3 id);;
let test = to_array (init 0 id );;


(** {2 Basic operations on parallel arrays.} *)

let rec distribution_ ap n =
  match n with 
  | 0 -> [|(Array.length (proj ap 0))|]
  | _ -> Array.append (distribution_ ap (n-1)) [|(Array.length (proj ap n))|];;
let distribution : 'a parallel_array -> int array =
  fun ap -> distribution_ ap (bsp_p -1);;  
  
let rec for_length n ap = 
match n with 
 | 0 -> Array.length (proj ap 0)
 | _ -> (for_length (n-1) ap) + Array.length (proj ap n);; 
 
let length : 'a parallel_array -> int =
  fun ap -> for_length (bsp_p -1) ap;;
 
let test = length (make 10 "a");;

let rec my_proc ap n x=
  match n with
  | -1-> failwith "erreur dans les paramaitres proc_offs_of_position ap x"
  | _ -> if( fst( ofs_len (length ap) n )<= x) then n else  my_proc ap (n-1) x;;
let proc_offs_of_position ap x = 
  let proc = my_proc ap (bsp_p -1) x
  and indice = x - fst( ofs_len (length ap) (my_proc ap (bsp_p -1) x))
  in (proc, indice);;
proc_offs_of_position (make 10 id) 2;;
(* Suggestion de fonction intermédiare
   retourne le numéro de processeur et la position local du x ème l'élément du tableau parallèle
*)


let get : 'a parallel_array -> int -> 'a =
  fun ap pos -> (proj ap (fst (proc_offs_of_position ap pos))).((snd (proc_offs_of_position ap pos)));;

get (init 10 id) 3;;   
let test =  Array.init 10 (get (init 10 id));; 


let set : 'a parallel_array -> int -> 'a -> unit =
  fun ap pos d -> Array.set (proj ap (fst(proc_offs_of_position ap pos))) 
			    (snd(proc_offs_of_position ap pos)) d;;
let test = (init 10 id);; 
set  test 0 100;;
test;;

let fill : 'a parallel_array -> int -> int -> 'a -> unit =
  fun ap ofs len x -> for i = ofs to len do set ap i x done ;;
(**
   à compléter
   indictation : vous aurez certainement besoin de la fonction 
   [ignore : 'a -> unit] qui renvoie unit quelque soit la valeur sur laquelle elle est appliquée.
*)


(** {2 map/reduce sur les tableaux parallèles.} *)

let map : ('a -> 'b) -> 'a parallel_array -> 'b parallel_array = 
  fun f ap ->  apply (mkpar (fun i tab ->Array.map f tab)) ap;;                        
(* à compléter *)
let test= map (fun i -> i+10) (init 10 id);;

let mapi : (int -> 'a -> 'b) -> 'a parallel_array -> 'b parallel_array =
  fun f ap -> of_array (Array.mapi f (to_array ap));;    
  (* à compléter *)
let test= mapi (fun i b-> i+b) (init 10 id);;

let rec reduce_bis f e a n =
  match n with
  | 0 -> failwith "erreur tableau vide !" 
  | 1 -> failwith "erreur tableau trop petit !" 
  | 2 -> if( a.(0) = e)then a.(1) else if(a.(1) = e)then a.(0) else f  a.(0) a.(1)
  | _ -> if(a.(n-1)=e)then reduce_bis f e a (n-1) else f (reduce_bis f e a (n-1)) a.(n-1);;
let reduce : ('b -> 'b -> 'b) -> 'b -> 'b parallel_array -> 'b =
  fun f x ap -> reduce_bis f x (to_array ap) (length ap);;
let test= reduce (fun a b -> a + b) 0 (make 10 0);;
let test= reduce (fun a b -> a + b) 0 (init 10 id);;


let fold_left : ('a -> 'b -> 'a) -> 'a -> 'b parallel_array -> 'a =
  fun f x ap -> Array.fold_left f x (to_array ap);;
  
let test = fold_left (fun a b -> a + b) 10 (init 10 id);;
(*  à compléter 
    indication : pensez au type ['a option]
*)

(** {2 Tri} *)

let sort_bis cmp a = let tab = Array.fast_sort cmp a in a;;
let sort : ('a -> 'a -> int) -> 'a parallel_array -> 'a parallel_array= 
fun cmp ap -> of_array (sort_bis cmp ( to_array ap));;

let test = sort compare (init 10 (fun x -> Random.int 100));;
(* à compléter  *)

(** {2 Functions avec schéma de communication non-trivial}  *)
let exists l a = if(List.exists ((fun x y -> if(x = y)then true else false ) a) l) 
		  then true else false;;  
let rec randomProcs n l= 
match n with
| 0 -> let r1 = (Random.int bsp_p) in if(exists l r1)then randomProcs n l  else  l@[r1]
| _ -> let r2 = (Random.int bsp_p ) in if(exists l r2)then  randomProcs n l  
					    else randomProcs (n-1) (l@[r2]) ;;
let rec get_el l n =
match n with
|0 -> List.hd l
|_ -> get_el (List.tl l) (n-1);;
let balance : 'a parallel_array -> 'a parallel_array =
fun ap -> apply (mkpar(fun i j-> proj ap j)) 
		(let t =randomProcs (bsp_p -1) [] in mkpar (fun i -> get_el t i));;
let tab_test = init 10 id;;		
let test = balance tab_test;;		
 (*
 à compléter  
*)

let sub : 'a parallel_array -> int -> int -> 'a parallel_array
  = fun ap offs len -> of_array (Array.sub ( to_array ap) offs len);;
(* à compléter  *)

let rec for_blit ap2 ap1 ofs1 ofs2 len =
  match len with
  |1 -> set ap2 ofs2 (get ap1 ofs1)  
  |_ -> set ap2 ofs2 (get ap1 ofs1); for_blit ap2 ap1 (ofs1 + 1) (ofs2 + 1) (len -1);;
let blit : 'a parallel_array -> int -> 'a parallel_array -> int -> int -> unit
  = fun ap1 offs1 ap2 offs2 len -> for_blit ap2 ap1 offs1 offs2 len ;; 
(* à compléter  *)
let tab1 = init 10 id;;
let tab2 = make 10 0;;
blit tab1 0 tab2 0 3;;
tab2;;









