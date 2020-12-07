(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo = Prazno | Vozlisce of 'a drevo * 'a * 'a drevo
(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)
let leaf x = Vozlisce(Prazno, x, Prazno)
let pomarancovec = Vozlisce(Vozlisce(leaf 0, 2, Prazno), 5, Vozlisce(leaf 6, 7, leaf 11))  

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)
let  rec mirror = function
     | Prazno -> Prazno
     | Vozlisce (l, v, d) -> Vozlisce (mirror d, v,mirror l)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)
let rec height = function
      |Prazno -> 0
      | Vozlisce (l, v, d)-> 1 + (max (height l) (height d))

let rec size = function
     |Prazno -> 0
     | Vozlisce (l,v,d) -> 1 + (size l) + (size d)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)
let rec map_tree f = function 
     | Prazno -> Prazno
     | Vozlisce (l,v,d) -> Vozlisce (map_tree f l, f v, map_tree f d)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

(* _slow_as_fuck omg o(n) je tko miljon ( n^3)*)
let rec list_of_tree = function
     | Prazno -> []
     | Vozlisce  (l,v,d) -> (list_of_tree l) @ [v] @ (list_of_tree d)

let list_go_brrr = () (* TODO *)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec is_bst = function
     |Prazno -> true
     |Vozlisce (l, v, d) -> match (l ,d) with
                         |(Prazno, Prazno) -> true
                         |(Vozlisce(_, vl,_), Prazno) -> (vl < v) && (is_bst l)
                         |(Prazno, Vozlisce(_, vd,_) ) -> (v < vd ) && (is_bst d)
                         |(Vozlisce(_, vl, _), Vozlisce(_, vd, _)) -> (v < vd ) && (is_bst d) && (vl < v) && (is_bst l)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec insert a = function
     |Prazno -> leaf a
     |Vozlisce(l, v, d) -> if a = v then Vozlisce(l, a, d) 
                         else if a < v then Vozlisce(insert a l, v, d) 
                         else Vozlisce(l, v, insert a d)


(* O(n, h) = h ~ ln(n) za AVL drevesa*)
let rec member a = function
     |Prazno -> false
     |Vozlisce (l, v, d) -> if v = a then true
                       else if a < v then member a l
                       else    member a d

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)
let rec member_slow_as_fuck a = function
     |Prazno -> false
     |Vozlisce (l, v, d) -> if a = v then true
                         else member_slow_as_fuck a l || member_slow_as_fuck a d

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)
let min_iskalno drevo = 
     let urejenec = list_of_tree drevo in 
     match urejenec with
          |[] -> None 
          | x::xs -> Some x
let succ = function
     |Prazno -> None
     |Vozlisce (_, _, d) -> min_iskalno d

let reverse_list = 
     let rec repna acc = function
          |[] -> acc 
          |x::xs -> repna (x::acc) xs
          in 
     repna []

(* To je lepa koda*)
let rec max_iskalno = function
     | Prazno -> None 
     | Vozlisce(_, x, Prazno) -> Some x
     | Vozlisce (_, _, d) -> max_iskalno d

let pred = function
     |Prazno -> None
     |Vozlisce(l,_,_) -> max_iskalno l

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)
let rec delete x = function
     | Prazno -> Prazno
     | Vozlisce (l, y, d) -> if x < y then Vozlisce( delete x l, y, d)
                         else if x > y then Vozlisce ( l, y, delete x d)
                         else 
                              let naslednji = succ (Vozlisce(l, y, d)) in 
                              match naslednji with
                                   |None -> l 
                                   |Some nas -> Vozlisce(l, nas, delete nas d)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type ('key, 'value) dict = ('key * 'value) drevo

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)
let test_dict = Vozlisce (leaf ("a", 0), ("b", 1), Vozlisce(leaf ("c", -2), ("d" ,2), Prazno ) )

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)
let rec dict_get k = function
     |Prazno -> None 
     | Vozlisce (l, (k', v'), d) ->
               if k < k' then dict_get k l 
               else if k > k' then dict_get k d
               else v'
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
let rec  print_string_int_dict = function
     | Prazno -> ()
     | Vozlisce (l, (k, v), d) ->
          print_endline (k^" . " ^ (string_of_int v));
          print_string_int_dict l;
          print_string_int_dict d;


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
let rec dict_insert k v = function
     | Prazno -> leaf (k, v)
     | Vozlisce (l, (k', v'), r) ->
          if k < k' then Vozlisce (dict_insert k v l, (k' , v'), r )
          else if k' < k then Vozlisce (l, (k', v'), dict_insert k v d)
          else 
               Vozlisce (l, (k', v), d)

