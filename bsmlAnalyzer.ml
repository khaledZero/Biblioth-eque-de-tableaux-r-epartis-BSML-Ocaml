(**********************************************************)
(*  BSML :  Wrapper to analyse performances		  *)
(*  Februrar 2014	          	                  *)
(*  Author: J. Tesson, Gaétan Hains			      *)
(**********************************************************)

module BsmlAnalyzer =
struct

  let bsp_p = Bsml.bsp_p
  let bsp_g = Bsml.bsp_g
  and bsp_l = Bsml.bsp_l
  let bsp_r = Bsml.bsp_r

(* Variables globales pour l'accumulation des parametres de
   la complexite BSP lors de l'appel d'operations BSML. La
   somme des parametres h et le nombre de super-etapes. *)

let array_max = Array.fold_left max 0;;
let array_sum = Array.fold_left (+) 0;;
let list_sum = List.fold_left (+) 0;;

type cost = {t: float (* secondes *); h: int (* octets *); steps: int; };;
let string_of_cost c =
 "t="^(string_of_float c.t)^"seconds\n"
^"h="^(string_of_int c.h)^" bytes\n"
     ^(string_of_int c.steps)^" supersteps\n" ;;
let bsp_print_cost c = print_string (string_of_cost c);;
let sub_costs c1 c2 = {t=c1.t-.c2.t; h= c1.h - c2.h; steps= c1.steps - c2.steps };;

let zero_cost = {t=0.0; h=0; steps=0};;
let bsp_cost = ref zero_cost;;
let bsp_cost_reset = fun () -> bsp_cost:= zero_cost;;
let bsp_get_cost= fun () ->  !bsp_cost ;;
let bsp_get_t= fun () ->     (bsp_get_cost()).t ;;
let bsp_get_h= fun () ->     (bsp_get_cost()).h ;;
let bsp_get_steps= fun () -> (bsp_get_cost()).steps ;;

let bytes_in = fun x -> String.length(Marshal.to_string x []) ;;
let bytes_in_option x = match x with
                   | None -> 0 (* BSML will not send such values in the network *)
                   | _ -> String.length(Marshal.to_string x []);;

(* enregistrer le coût d'une phase de calcul asynchrone *)
let bsp_async = fun delta_t ->
  bsp_cost:= {t= bsp_get_t()+.delta_t; h= bsp_get_h(); steps= bsp_get_steps()}
;;

(* enregistrer le coût d'une barrière de synchronisation *)
let bsp_step= fun () ->
  bsp_cost:= {t= bsp_get_t(); h= bsp_get_h(); steps= bsp_get_steps()+1}
;;

(* enregistrer le coût d'une communication globale de paramètre h=delta_h *)
let bsp_comm delta_h =
  bsp_cost:= {t= bsp_get_t(); h= bsp_get_h()+delta_h; steps= bsp_get_steps()}
;;

let processors = let rec aux pid l = if pid > 0 then aux (pid-1) (pid-1::l) else l in aux bsp_p [] ;;

let fold_timings () =
  let timings = Bsml.proj (Bsml.get_cost ()) in
  List.fold_left max 0.0 (List.map timings processors)

(* Exécuter un programme et en restituer le coût BSP. *)
let bsp_test f x =
  let start = !bsp_cost in
  try 
    ignore (f x);
    bsp_async (fold_timings ());
    bsp_print_cost(sub_costs (!bsp_cost) start)
  with ex ->
    bsp_async (fold_timings ());
    bsp_print_cost(sub_costs (!bsp_cost) start); 
    raise ex
;;
(** Fonctions nécessaires à la définition de BSML. ***********************)

let (mkpar: (int -> 'a) -> 'a Bsml.par) =
fun  f ->
  Bsml.start_timing ();
  try
    let res =Bsml.mkpar f in
    let () = Bsml.stop_timing () in
    res
  with ex -> 
    Bsml.stop_timing () ; 
    raise ex
;;

let (apply: ('a -> 'b)Bsml.par -> 'a Bsml.par -> 'b Bsml.par) =
fun ff aa ->
  Bsml.start_timing ();
  try
    let res =Bsml.apply ff aa in
    let () = Bsml.stop_timing () in
    res
  with ex -> 
    Bsml.stop_timing () ; 
    raise ex
;;

let (put_option: (int -> 'a option)Bsml.par -> (int -> 'a option) Bsml.par)=
fun (fs) ->
  let messages_sizes =
    apply (mkpar (fun _ ->
      fun f ->
        Array.of_list (List.map (fun pid -> bytes_in_option (f pid)) processors))) fs
  in
  let exchanged_sizes = Bsml.proj messages_sizes
  in
  let bytes_received pid = list_sum
    (List.map (fun j -> (exchanged_sizes j).(pid)) processors)
  in
  let h_minus_array =
    Array.of_list (List.map (fun pid -> bytes_received pid) processors)
  in
  let h_minus = array_max h_minus_array in
  let bytes_leaving = fun i ->
    array_sum (exchanged_sizes i) in
  let h_plus_array =
    Array.of_list(List.map bytes_leaving processors)
  in
  let h_plus = array_max h_plus_array in
  let h = max h_minus h_plus in
  let _ = (bsp_async (fold_timings ()) ;bsp_step(); bsp_comm h)
  in
  Bsml.put fs;;


let (put: (int -> 'a)Bsml.par -> (int -> 'a) Bsml.par)=
fun (fs) ->
  let messages_sizes =
    apply (mkpar (fun _ ->
      fun f ->
        Array.of_list (List.map (fun pid -> bytes_in (f pid)) processors))) fs
  in
  let exchanged_sizes = Bsml.proj messages_sizes
  in
  let bytes_received pid = list_sum
    (List.map (fun j -> (exchanged_sizes j).(pid)) processors)
  in
  let h_minus_array =
    Array.of_list (List.map (fun pid -> bytes_received pid) processors)
  in
  let h_minus = array_max h_minus_array in
  let bytes_leaving = fun i ->
    array_sum (exchanged_sizes i) in
  let h_plus_array =
    Array.of_list(List.map bytes_leaving processors)
  in
  let h_plus = array_max h_plus_array in
  let h = max h_minus h_plus in
  let _ = (bsp_step(); bsp_comm h)
  in
  Bsml.put fs;;

let (proj: 'a Bsml.par -> (int -> 'a)) =
  fun fs ->
  let sizes = Bsml.proj (Bsml.apply (Bsml.mkpar (fun  _ ->  bytes_in)) fs)
  in
  let tailles =
    List.map sizes processors
  in
  let h_plus =
    bsp_p * (List.fold_left max 0 tailles) in
  let h_minus = List.fold_left (+) 0 tailles in
  let h = max h_plus h_minus in
  let _ = (bsp_step(); bsp_comm h) in
  Bsml.proj fs
;;

end
