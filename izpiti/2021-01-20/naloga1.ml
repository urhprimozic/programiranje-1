(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki vrne razliko med produktom in vsoto dveh celih števil.

    razlika_produkta_in_vsote : int -> int -> int

[*----------------------------------------------------------------------------*)
let razlika_produkta_in_vsote m n = 
  let produkt = m * n in 
  let vsota = m + n in 
  produkt - vsota



(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki združi dva para v četverico.
    
    zlimaj_para : 'a * 'b -> 'c * 'd -> 'a * 'b * 'c * 'd

[*----------------------------------------------------------------------------*)
let zlimaj_para (x, y) (a, b) = (x, y, a, b)


(* c *)
(*----------------------------------------------------------------------------*]
  Imamo podatke tipa [int option * int option * int option], ki jih želimo
  grafično predstaviti. Napišite funkcijo [trojica_graficno], ki sprejme takšno
  trojico in vrne niz, kjer so ``manjkajoči'' elementi nadomeščeni z [-].
  Primer vrnjenega niza je ["(1, 2, -)"]

    trojica_graficno : int option * int option * int option -> string

[*----------------------------------------------------------------------------*)
let option_to_string = function
      |None -> "-"
      |Some n -> string_of_int n

let trojica_graficno (a, b, c) = 
  let sez = ["(";option_to_string a;", "; option_to_string b; ", "; option_to_string c; ")"] in 
  String.concat "" sez 



(* d *)
(*----------------------------------------------------------------------------*]
  Klic funkcije [nedeljivo_do x n] preveri, da število [x] ni deljivo z nobenim
  naravnim številom od 2 do vključno [n]. Število 73859 je praštevilo, torej
  mora [nedeljivo_do 73859 73858] vrniti [true].

    nedeljivo_do : int -> int -> bool

[*----------------------------------------------------------------------------*)
(* Predvidevamo, da bo n>= 2*)
let rec nedeljivo_do x n = 
  match n with 
    | 2 -> (x mod 2) = 1
    | m -> if x mod m = 0 then false else nedeljivo_do x (m - 1)



(* e *)
(*----------------------------------------------------------------------------*]
  Seznam elementov tipa ['a option] želimo razdeliti na podsezname glede na
  pojavitve vrednosti [None].

    razcepi_pri_None : 'a option list -> 'a list list.

  Kot primer, funkcija seznam

    [Some 1; None; Some 2; Some 3; None; None; Some 4; None]

  razcepi v [[1]; [2;3]; []; [4]; []]. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)
let razcepi_pri_None o_list= 
  let rec repna acc ans = function
    |[] -> acc::ans
    |x::xs -> match x with  
              |None -> repna [] ((List.rev acc)::ans) xs 
              |Some y -> repna (y::acc) ans xs 
    in 
    List.rev (repna [] [] o_list)

