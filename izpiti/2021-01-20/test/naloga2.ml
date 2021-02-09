(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
  [*============================================================================*)

type 'a vsebina_kuhinje =
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
  [*----------------------------------------------------------------------------*)
let kuhinja = [Ponev "tuna"; Lonec ("brokoli", "mango"); Omara ["sir"; "toast"; "sok"; "ragu"]]


(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
  [*----------------------------------------------------------------------------*)
(*ni repna *)
let rec prestej = function 
  |[] -> 0
  |objekt::ostalo -> 
    match objekt with
      |Ponev _-> 1 + (prestej ostalo) 
      |Lonec _ -> 2 + (prestej ostalo) 
      |Omara sez -> (List.length sez) + (prestej ostalo)


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

  [*----------------------------------------------------------------------------*)
let pretvori f = function
  |Ponev a-> Ponev (f a)
  |Lonec (x, y) -> (Lonec (f x, f y))
  |Omara sez -> Omara (List.map f sez)


(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna. 

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

  [*----------------------------------------------------------------------------*)
let pospravi  = 
  let rec repna acc = function
    |[] -> Omara acc 
    |objekt::ostalo -> 
      match objekt with 
        |Ponev a -> repna (a::acc) ostalo 
        |Lonec (x, y) -> repna (x::y::acc) ostalo 
        |Omara sez -> repna (sez@acc) ostalo 
    in 
    repna []

(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
  [*----------------------------------------------------------------------------*)

(* iz https://stackoverflow.com/questions/23654136/how-to-sum-a-list-of-float-numbers-in-ocaml
 in porihtano a dela na intih*)
let rec sum l=
  match l with
  []->0
  |h::t-> h + (sum t);;


let rec oceni k_list gelb =
  match k_list with
    |[] -> 0
    |objekt::ostalo -> 
      match objekt with 
        |Ponev a -> (gelb a) + (oceni ostalo gelb)
        |Lonec (x, y) -> 3*(gelb x) + 3*(gelb y) + (oceni ostalo gelb)
        |Omara sez -> 5*(sum (List.map gelb sez)) + (oceni ostalo gelb)
