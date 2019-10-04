let rec mem a = function
| [] -> false
|h::t ->
if h = a then true
else mem a t ;;                                                                                                                   

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal ;;                                                                                                             

let rec find_set sym rules = match rules with
|[] -> []
  |(a,b)::t ->
if a = sym then b::find_set sym t
else find_set sym t;;                                                                                                             

let rec concat l = match l with
              |[] -> []
              |h::t ->
              (match h with
              |[] -> concat t
              |a::b -> a::(concat (b::t))
              );;                                                                                                                 
let rec filter_terminal l = match l with
  |[] -> []
|T s ::t -> filter_terminal t
  |N s ::t -> s::filter_terminal t;;

let rec finds g = match g with
  |(s,rules) -> filter_terminal(concat( find_set s rules));;                                                                      

 let rec cut_rules sym rules = match rules with
  |[] -> []
|(a,b)::t -> if sym = a then cut_rules sym t
  else (a,b)::cut_rules sym t;;                                                                                                   

let rec dfs sym rules set = match set with
            |[] -> []
            |h::t -> let sub_rules = (cut_rules sym rules) in
            if h = sym then dfs sym sub_rules t
            else  let sub_set = (filter_terminal(concat(find_set h sub_rules)))in
  (dfs h (cut_rules h sub_rules) sub_set)@(dfs sym (cut_rules h rules) t)@set@sub_set;;                                           

 let rec filter_sym l set = match l with
  |[] -> []
|h::t -> (match h with
  |(c,d) -> if mem c set then (c,d)::(filter_sym t set)
else filter_sym t set
);;

let rec subset a b = match a with
|[] -> true
|h::t ->
if mem h b then subset t b
else false ;;

let equal_sets a b = (subset a b) && (subset b a);;

let set_union a b = a@b;;

let rec set_intersection a = function
|[] -> []
  |h::t ->
if mem h a then h::set_intersection a t
else set_intersection a t;;

let rec set_diff a b = match a with
          |[] -> []
          |h::t ->
if mem h b then set_diff t b
else h::set_diff t b;;

let rec computed_fixed_point eq f x =
if eq x (f x) then x
else computed_fixed_point eq f (f x);;

let filter_reachable g = match g with
|(a,b) -> let set = finds g in
let l = dfs a b set in
let c = a::l in
let s = filter_sym b c in
  (a,s);;                                                                                                                         

