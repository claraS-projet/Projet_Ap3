#load "btreeP.cmo";;
open BtreeP;;

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
