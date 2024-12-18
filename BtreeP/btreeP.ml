(** Implémentation du module pour les arbres binaires @author clara Schobert*)

type t_a = int;;
(** Type représentant un noeud *)
type node = {
  value : t_a;
  left : t_btree;
  right : t_btree;
}
and t_btree =
  | Empty
  | Node of node;;

(** Renvoie un arbre vide *)
let bt_empty () = Empty;;

(** Vérifie si un arbre est vide *)
let bt_isempty tree =
  match tree with
  | Empty -> true
  | _ -> false
;;

(** Crée un arbre en enracinant une valeur avec deux sous-arbres *)
let bt_rooting x ag ad =
  Node { value = x; left = ag; right = ad }
;;

(** Renvoie la racine de l'arbre *) 
let bt_root tree =
  match tree with
  | Node { value; _ } -> value
  | Empty -> failwith "bt_root: L'arbre est vide"
;;

(** Renvoie le sous-arbre gauche *)
let bt_subleft tree =
  match tree with
  | Node { left; _ } -> left
  | Empty -> failwith "bt_subleft: L'arbre est vide"
;;

(** Renvoie le sous-arbre droit *)
let bt_subright tree =
  match tree with
  | Node { right; _ } -> right
  | Empty -> failwith "bt_subright: L'arbre est vide"
;;
