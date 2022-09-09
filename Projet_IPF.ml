(* Question 1 *)

type string_builder =
	|Mot of string * int
	|N of int * string_builder * string_builder
;;

let word s = Mot(s,String.length s);;

let w1 = word "Hello "
let w2 = word "world! ";;

let w3 = word "My "
let w4 = word "name ";;

let w5 = word "is "
let w6 = word "Alae";;

let longueur s = match s with
	|Mot(_,l) -> l
	|N(l,_,_) -> l
;;

let concat s1 s2 = N((longueur s1)+(longueur s2),s1,s2);;

let phrase = concat (concat w1 w2) (concat (concat w3 w4) (concat w5 w6));;

let l = longueur phrase;;


(* Question 2 *)

let rec char_at i s = match (i,s) with
	|(ind,Mot(c,l)) -> String.get c ind
	|(ind,N(l,g,d)) -> let lg = (longueur g) in 
								if ind < lg
									then 
										char_at ind g
									else 
										char_at (ind-lg) d
;;

char_at 0 phrase;; 
char_at 1 phrase;;
char_at 6 phrase;;
char_at 8 phrase;;
char_at 16 phrase;;
char_at 20 phrase;;
char_at 26 phrase;;
char_at 27 phrase;;

(* Question 3 *)

String.sub "Hello" 0 2;;

let rec sub_string ind long sb = match sb with
	|Mot(str,l) -> Mot((String.sub str ind long),long)
	|N(l,g,d) -> let lg = (longueur g) in 
										if ind < lg  
											then (* indice à gauche *)
												if ind+long <= lg  
													then 
														sub_string ind long g (* mot entièrement à gauche  *)
												   else 
														concat (sub_string ind (lg-ind) g)(sub_string 0 (long-(lg-ind)) d) (* mot qui s'étale sur les deux branches *)
											else 
												sub_string (ind-lg) long d (* mot entièrement à droite  *)
;;
sub_string 0 3 w1;;
sub_string 0 6 w1;;

let hw = concat w1 w2;; (* "Hello world! " *)

sub_string 0 3 hw;;
sub_string 0 6 hw;;
sub_string 0 7 hw;;
sub_string 6 6 hw;;
sub_string 0 13 hw;;
sub_string 5 1 hw;;
sub_string 5 3 hw;;

sub_string 5 15 phrase;;
sub_string 5 18 phrase;;


(* Question 4 *)



let cost sb =
	let rec aux sb profondeur = match (sb,profondeur) with
		|(Mot(_,l),p) -> p*l
		|(N(_,g,d),p) -> (aux g (p+1))+(aux d (p+1))
	in
		aux sb 0;;

cost w1;;
cost hw;;
cost phrase;;


(* Question 5 *)



let rand_chr () = (Char.chr (97 + (Random.int 26)));;


let rand_chaine () = 
	let rec aux i = if i == 0 
							then 
								""
							else 
								(Char.escaped (rand_chr ()))^(aux (i-1))
	in
		aux (1 + (Random.int 6))
;;


let rand_Mot () = word (rand_chaine());;

rand_Mot ();;


let random_string i = 
	let rec aux i p = 
		if p < i then
			let rand = Random.int 3 in 
				if rand == 0 then
					concat (aux i (p+1)) (rand_Mot ())
				else if rand == 1 then
					concat (rand_Mot ()) (aux i (p+1)) 
				else 
					concat (aux i (p+1)) (aux i (p+1))
		else
			rand_Mot ()
	in 
	aux i 0
;;

random_string 3;;


(* Question 6 *)


let rec list_of_string sb = match sb with
	|Mot(s,l) -> [s]
	|N(l,g,d) -> (list_of_string g) @ (list_of_string d)
;;

list_of_string phrase;;
list_of_string (random_string 10);;


(* Question 7 *)

let list_of_Mot sb = List.map word (list_of_string sb) ;;
list_of_Mot phrase;;

let rec concat_i_next sb_list i = match (sb_list,i) with
	|([],_) -> failwith "list.lenght < 2"
	|([sb],_) -> failwith "list.lenght < 2"
	|(x::y::q,0) -> (concat x y)::q 
	|(x::y::q,i) -> x::(concat_i_next (y::q) (i-1))
;;


let min x y =  if x < y then x else y;;
let max x y =  if x > y then x else y;;


let l = "ab"::"abc"::"abcd"::"e"::"a"::"er"::[];;
let l2 = List.map word l;;


(* Version utilisant les ref *)
(*
let indice_min_cost l = 
	let min_actuel = ref (cost (concat (List.nth l 0) (List.nth l 1))) in
	let ind_act = ref 0 in
	let ind_min_act = ref 0 in 
	let rec aux l = match l with
		|[] -> failwith "list.lenght < 2"
		|[x] -> cost x
		|x::y::q -> let test = cost (concat x y) in 
							if (test < !min_actuel) then 
							begin
								min_actuel := test;
								ind_min_act := (!ind_act);
								ind_act := (!ind_act) + 1;
								aux (y::q)
							end
							else 
							begin
								ind_act := (!ind_act) + 1;
								aux (y::q)
							end
	in ignore (aux l);
	!ind_min_act;
	;;
*) 
;;

(* Version sans les ref *)
let indice_min_cost l =
    let rec aux l min_actuel ind_act ind_min_act = match l with
        | [] -> failwith "the list is empty"
        | [x] -> ind_min_act
        | x :: y :: l -> 
            let test = cost (concat x y) in
                if test < min_actuel
                    then aux (y :: l) test (ind_act + 1) ind_act
                    else aux (y :: l) min_actuel (ind_act + 1) ind_min_act
    in aux l (cost (concat (List.nth l 0) (List.nth l 1))) 0 0 ;;

indice_min_cost l2;;


let balance sb = 
	let list_mot_sb = list_of_Mot sb in
	let rec aux l = match l with
		|[] -> failwith "list.lenght < 2"
		|[x] -> x
		|l -> let ind_min = indice_min_cost l in
					aux (concat_i_next l ind_min)
	in aux list_mot_sb
;;

let phrase2 = concat w1 (concat w2 (concat w3 (concat w4 (concat w5 w6) ) ) );; 


balance phrase2;;
let lp = list_of_Mot phrase2;;
let i1 = indice_min_cost (lp);;
let etape1 = concat_i_next lp i1;;
let i2 = indice_min_cost (etape1);;
let etape2 = concat_i_next etape1 i2;;
let i3 = indice_min_cost (etape2);;
let etape3 = concat_i_next etape2 i3;;
let i4 = indice_min_cost (etape3);;
let etape4 = concat_i_next etape3 i4;;
let i5 = indice_min_cost (etape4);;
let etape5 = concat_i_next etape4 i5;;


cost phrase;;
cost (balance phrase);;



cost phrase2;;
balance phrase2;;
cost (balance phrase2);;


(* Question 8 *)

type stats = 
	{ list_sb : string_builder list ;
	  list_cost : int list;
	  minimum : int ;
	  maximum : int ;
	  moyenne : float ;
	  mediane : float ;
	  list_sb_balanced : string_builder list ;
	  list_cost_balanced : int list;
	  minimum_balanced : int ;
	  maximum_balanced : int ;
	  moyenne_balanced : float ;
	  mediane_balanced : float ;
	  list_gain : int list;
	  gain_tot : int;
	}
;;


let rec generate_random_sb_list taille = 
	if taille == 0 then
		[]
	else if taille == 1 then
		[random_string (1 + Random.int 9)]
	else
		(random_string (1 + Random.int 9))::(generate_random_sb_list (taille-1))
;;


let rec list_of_cost sb_list = match sb_list with
	|[] -> []
	|x::q -> cost x::(list_of_cost q)
;;

let min_list l = List.fold_left (min) (List.hd l) l;;

let max_list l = List.fold_left (max) (List.hd l) l;;

let somme_list l = List.fold_left (fun x y -> x+y) 0 l;;

let moyenne_list l = (Float.of_int (somme_list l)) /. (Float.of_int (List.length l));;

let mediane_list l = let taille = List.length l in
	let l_sort = List.sort (compare) (l) in
	if (taille mod 2) == 1 then
		Float.of_int (List.nth l_sort ((taille-1)/2))
	else
		((Float.of_int (List.nth l_sort (taille/2)-1)) +. (Float.of_int (List.nth l_sort (taille/2))))  /. 2.0
;;

let rec diff_deux_list l1 l2 = match (l1,l2) with
	|([],[]) -> []
	|[],l2 -> failwith "listes de tailles différentes" (* Cas impossible dans la suite car cette fonction est utilisé que pour des listes de même taille *)
	|l1,[] -> failwith "listes de tailles différentes" (* Cas impossible dans la suite car cette fonction est utilisé que pour des listes de même taille *)
	|(x::q1,y::q2) -> (x-y)::(diff_deux_list q1 q2)
;;

let test =(1::2::3::4::3::5::[]);;

min_list test;;

max_list test;;

somme_list test;;

moyenne_list test;;

mediane_list test;;


let generate_stat () = let list_of_sb = generate_random_sb_list 300 in
	let list_costs = list_of_cost list_of_sb in
	let list_of_sb_balanced = List.map balance list_of_sb in
	let list_costs_balanced = list_of_cost list_of_sb_balanced in
	let list_des_gains = diff_deux_list (list_costs) (list_costs_balanced) in
	let stat = 
	{ list_sb = list_of_sb ;
	  list_cost = list_costs;
	  minimum = min_list list_costs;
	  maximum = max_list list_costs;
	  moyenne = moyenne_list list_costs;
	  mediane = mediane_list list_costs;
	  
	  list_sb_balanced = list_of_sb_balanced;
	  list_cost_balanced = list_costs_balanced;
	  minimum_balanced = min_list list_costs_balanced;
	  maximum_balanced = max_list list_costs_balanced;
	  moyenne_balanced = moyenne_list list_costs_balanced;
	  mediane_balanced = mediane_list list_costs_balanced;
	  list_gain = list_des_gains;
	  gain_tot = somme_list list_des_gains;
	  
	} in
	stat
;;


let test = generate_stat ();;

test.minimum;;
test.minimum_balanced;;

test.maximum;;
test.maximum_balanced;;

test.moyenne;;
test.moyenne_balanced;;

test.mediane;;
test.mediane_balanced;;

test.list_gain;;
test.gain_tot;;


