(** Module pour les arbres binaires tp1 @author Clara Schobert *)

(** Type pour la valeur contenue dans les noeuds de l'arbre *)
type t_a = int

(** Type abstrait représentant un arbre binaire *)
type t_btree

(** Renvoie un arbre vide *)
val bt_empty : unit -> t_btree

(** Crée un arbre en enracinant une valeur avec deux sous-arbres *)
val bt_rooting : t_a -> t_btree -> t_btree -> t_btree

(** Renvoie la racine d'un arbre *)
val bt_root : t_btree -> t_a

(** Renvoie le sous-arbre gauche *)
val bt_subleft : t_btree -> t_btree

(** Renvoie le sous-arbre droit *)
val bt_subright : t_btree -> t_btree

(** Vérifie si un arbre est vide *)
val bt_isempty : t_btree -> bool


