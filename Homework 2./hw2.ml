type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;                                                                                                                               \
                                                                                                                                  

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
      ;;                                                                                                                         \
                                                                                                                                  

let rec find_set rules sym = match rules with
  |[] -> []
    |(a,b)::t ->
      if a= sym then b::find_set t sym
else find_set t sym;;                                                                                                            \
                                                                                                                                  

let convert_grammar gram1 = match gram1 with
  |(a,b) -> (a, find_set b);;

 let rec loop_list f = function
      |[] -> []
    |h::t -> (f h)@loop_list f t;;

let rec parse_tree_leaves tree = match tree with
  |Leaf s -> [s]
  |Node (a,b) -> match b with
                 |[] -> []
                 |h::t -> (parse_tree_leaves h)@(loop_list parse_tree_leaves t);;

(* abstraction: if l is all terminal then just compare it with the frag and try the apt with the rest of frag if matches
                if a nonterminal is encountered, then just use matcher to deal with the alternative list and use sub_matcher
                to carry the information of the rest of l *)

let rec sub_tree1 list1 f1 f2 apt rlist rrlist sym slist frag = match list1 with
|[] -> (match rrlist with
       |[] -> (match slist with
              |[] ->  apt ([Node(sym,rlist)]) [] sym [] frag
              |sh::st -> []
           (*   apt ([Node(sym,rlist@llist)]) [] sh st frag *)
              )
       |rh::rt -> (match slist with
                  |[] -> [] (* apt (rh@[Node(sym,rlist@llist)]) rt sym [] frag*)
                  |sh::st ->
                  apt (rh@[Node(sym,rlist)]) rt sh st frag
                  )
       )
|h::t -> (
         match h with
         |T s -> (match frag with
                 |[] -> []
                 |hf::tf -> if s = hf
                            then (

                                  sub_tree1 t f1 f2 apt(rlist@[Leaf s]) rrlist sym slist tf
                                 )
                             else []
                 )
         |N s -> (
                  f2 [] (rlist::rrlist) s (sym::slist) (f1 s) f1 (sub_tree1 t f1 f2  apt) frag

                 )
          );;



let rec make_tree1 rlist rrlist sym slist l f apt frag = match l with
|[] -> []
|h::t -> (
         match (sub_tree1 h f make_tree1 apt rlist rrlist sym slist frag) with
         |[] -> make_tree1 rlist rrlist sym slist t f apt frag
         |x -> x

         );;
         
         
let make_parser g frag = match g with
|(s,f) -> (match (make_tree1 [] [] s [] (f s) f apt1 frag) with
          |[] -> None
          |h::t -> Some (h)
          ) ;;                 
