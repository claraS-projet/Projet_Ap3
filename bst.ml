#load "btreeP.cmo";;
open BtreeP;;
#show BtreeP

let rec bst_seek tree myval =
  match tree with
  |Empty -> false
  |Node {value; left; right} -> (value = myval) || bst_seek left myval || bst_seek right myval
;;

let rec bst_insert (tree : t_btree ) ( myval : t_a) : t_btree =
  if bt_isempty(tree)
  then bt_rooting myval (bt_empty()) (bt_empty())
  else
    if myval = bt_root(tree)
    then tree
    else
        if myval > bt_root (tree)
        then bst_insert (bt_subright(tree)) myval
        else bst_insert (bt_subleft(tree)) myval
;;

let bst_lbuild (mylist : int list) : t_btree =
  let len = List.length(mylist)
  and arbre = bt_empty in
  
  for i = 0 to len - 1
  do bst_insert (arbre) (List.hd(mylist))
       List.tl(mylist);
  done;
  arbre
;;

let rec bst_delete_aux (tree, myval, treeli : t_btree * int * int list) : int list =
  match tree with
  | Empty -> []
  | Node { myval; left; right } -> bst_delete_aux tree myval treeli
  | Node { x; left; right} -> bst_delete_aux tree myval x::treeli
;;
  
let bst_delete (tree, myval : t_btree * int) : t_btree =
  let newli : int list = bst_delete_aux tree myval [] in
  bst_lbuild newli;;
  
