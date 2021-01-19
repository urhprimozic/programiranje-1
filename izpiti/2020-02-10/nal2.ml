type ('a, 'b) tree = 
    |Empty
    |ANode of ('a, 'b) tree * 'a * ('a, 'b) tree 
    |BNode of ('a, 'b) tree  * 'b * ('a, 'b) tree 

let aleaf x = ANode (Empty, x, Empty)
let bleaf x = BNode (Empty, x, Empty)
 
let test = ANode(bleaf true, 12, ANode(aleaf 0, 5, bleaf false))
let testis = ANode(bleaf true, 12, ANode(bleaf false, 5, bleaf false))

let rec adepth = function
    |Empty -> 0
    |ANode (l, _, d) -> 1 + max (adepth l) (adepth d)
    |BNode (l, _, d) -> let left = max (adepth l) (adepth d) in
                        if left = 0 then 0
                        else 1 + left  

let rec bdepth = function
    |Empty -> 0
    |BNode (l, _, d) -> 1 + max (bdepth l) (bdepth d)
    |ANode (l, _, d) -> let left = max (bdepth l) (bdepth d) in
                        if left = 0 then 0
                        else 1 + left 

type result = {aNodes : int; bNodes : int}

let (+/) res1 res2 = {aNodes = res1.aNodes + res2.aNodes; bNodes = res1.bNodes + res2.bNodes}


let rec count = function 
    |Empty -> {aNodes = 0; bNodes = 0}
    |ANode (l, _, r) -> {aNodes = 1; bNodes = 0} +/ (count l) +/ (count r)
    |BNode (l, _, r) -> {aNodes = 0; bNodes = 1} +/ (count l) +/ (count r)

let rec is_typemirror tree1 tree2 = 
match tree1, tree2 with 
    |Empty, Empty -> true
    |Empty, _ -> false 
    |_, Empty -> false
    |ANode _, ANode _ -> false 
    |BNode _, BNode _ -> false 
    |ANode (l1, x1, d1), BNode (l2, x2, d2) ->  (x1=x2) && (is_typemirror l1 l2) && (is_typemirror d1 d2) 
    |BNode (l1, x1, d1), ANode (l2, x2, d2) ->  (x1=x2) && (is_typemirror l1 l2) && (is_typemirror d1 d2) 

let rec foldmap fa fb acc tr = match tr with 
    |Empty -> (acc, Empty)
    |ANode (l, x, r) -> let tmp = fa acc x in 
                        let acc' = fst tmp in 
                        let x' = snd tmp in 
                        (* Najprej slikam levo vejo*)
                        let tmp_left = foldmap fa fb acc' l in 
                        let acc' = fst tmp_left in 
                        let left = snd tmp_left in 
                        (* slikam desno vejo z novim acc*)
                        let tmp_right = foldmap fa fb acc' r in
                        let acc' = fst tmp_right in 
                        let right = snd tmp_right in 
                        (acc' ,ANode (left, x', right)) 
    |BNode (l, x, r) -> let tmp = fb acc x in 
                        let acc' = fst tmp in 
                        let x' = snd tmp in 
                        (* Najprej slikam levo vejo*)
                        let tmp_left = foldmap fa fb acc' l in 
                        let acc' = fst tmp_left in 
                        let left = snd tmp_left in 
                        (* slikam desno vejo z novim acc*)
                        let tmp_right = foldmap fa fb acc' r in
                        let acc' = fst tmp_right in 
                        let right = snd tmp_right in 
                        (acc' ,BNode (left, x', right))