(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree


(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let leaf x = Node (Empty, x, Empty)
let primer = Node (leaf (3, Node(Empty, "g", leaf "t")) ,(7, leaf "a"), leaf (4, 
      Node (leaf "e", "r", Node(leaf "t", "z", Empty))))


(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)
let rec is_elemetn_binary x = function
  |Empty -> false 
  | Node  (l, curr, d) -> if curr = x then true 
                          else if x < curr then is_elemetn_binary x l 
                          else is_elemetn_binary x d 

let rec is_elemetn_lexi (x, y) = function
  |Empty -> false 
  |Node (l, (curr, xtree), d) -> if curr = x then is_elemetn_binary y xtree 
  else if x < curr then is_elemetn_lexi (x, y) l 
  else is_elemetn_lexi (x, y) d 
  


(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)
let rec insert_binary tree x = match tree with
    |Empty -> Node (Empty, x, Empty)
    |Node (l, y, d) -> if x < y then Node(insert_binary l x, y, d)
                        else if x = y then Node(l, y, d)
                        else Node(l, y, insert_binary d x)

let rec insert_lexi (x, y) = function
|Empty -> leaf (x, leaf y)
|Node (l ,(curr, xtree), d) -> if curr = x then Node (l, (curr, insert_binary xtree y), d)
    else if x < curr then insert_lexi (x, y) l 
    else insert_lexi (x, y) d 





(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
let rec seznam_binary = function
  |Empty -> [] 
  |Node (l, x, d) -> (seznam_binary l) @ ([x] @ (seznam_binary d))


let rec seznam_lexi = function
  |Empty -> [] 
  |Node (l , (curr, tree), d) -> 
    let tree_list = seznam_binary tree in 
    let curr_seznam = List.map (fun x -> (curr, x)) tree_list in 
    (seznam_lexi l) @ (curr_seznam @ (seznam_lexi d))


(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)
let rec lexi_fold f a tree = 
  let setz = seznam_lexi tree in 
  List.fold_left f a setz
  (* okejm, treba bo oddat. Zato bom bl idejno napisal ,pa ne bom pazil na sintakso* *)