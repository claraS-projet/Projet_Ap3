#load "btreeP.cmo";;
open BtreeP;;

(*Calcul de la taille de l'arbre*)
let rec size tree =
  match tree with
  |Empty -> 0
  |Node {value ; left; right} -> 1 + size left + size right
;;

(*Calcul de la hauteur de l'arbre*)
let rec height tree =
  match tree with
  |Empty -> 0
  |Node {value ; left; right} -> 1 + max (height left) (height right)
;;

let rec to_string tree =
  match tree with
  |Empty -> "()"
  |Node {value; left; right} -> "(" ^ string_of_int(value) ^ " " ^ to_string(left) ^ " " ^ to_string(right) ^ ")"
;;

let myArbre = bt_rooting 5 (bt_rooting 2 (bt_empty()) (bt_empty())) (bt_rooting 7 (bt_empty()) (bt_empty()));;
let arbre = to_string myArbre;;
let taille = height myArbre;;
