(*Binarno drevo*)
type 'a bst = 
    |Empty
    |Parent of 'a bst * 'a * 'a bst

(*Manjši ali enako LEVO, večji DESNO *)

(*Vrne tree, ki ima notr x na pravem mestu. Če ima tree že x, vrne tree*)
let rec insert tree x = match tree with
    |Empty -> Parent (Empty, x, Empty)
    |Parent (l, y, d) -> if x < y then Parent(insert l x, y, d)
                        else if x = y then Parent(l, y, d)
                        else Parent(l, y, insert d x)

(* Vrne true, če tree vsebuje x, drugače false *)
let rec contains tree x = match tree with
    |Empty -> false
    |Parent (l, y, d) -> if y = x then true 
                        else if x < y then contains l x 
                        else contains d x 
let rec get_biggest  =  function
    |Empty -> None 
    |Parent (_, x, Empty) -> Some x
    |Parent (_, _, d) -> get_biggest d

let rec get_smallest = function
    |Empty -> None 
    |Parent (Empty, x, _) ->Some x
    |Parent (l, _, _) -> get_smallest l

(* Vrne drevo brez x*)
let rec delete tree x = match tree with 
    |Empty -> Empty (* Nasledna vrstica je sicer redundant but okj*)
    |Parent (Empty, y, Empty) -> if x = y then Empty else Parent (Empty, y, Empty)
    |Parent (l, y, d) -> if x < y then Parent (delete l x, y, d)
                    else if x > y then Parent (l, y, delete d x)
                    else (* y mormo odstrant*)
                    let a = get_smallest d in  
                    match a with 
                        |Some y -> Parent (l, y, delete d y) (* Zih znamo ker je list*)
                        |None -> 
                            let b = get_biggest l in
                            match b with  
                                |Some z -> Parent(delete l z, z, d)
                                |None -> Empty
    
let leaf x = Parent (Empty, x, Empty)

let ex1 = Empty
let ex2 = Parent(Parent(Parent(Parent(Empty, 4, leaf 5), 6, Empty), 7, leaf 9), 10, Parent(Empty,11, Parent(leaf 12, 15, leaf 16) ) )

