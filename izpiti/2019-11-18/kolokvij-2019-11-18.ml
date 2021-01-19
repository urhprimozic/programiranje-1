(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = x * x = y

let pack3 a b c = (a, b, c)

let sum_if_not p =
  let rec repna acc = function
    |[] -> acc 
    |x::xs -> if p x then repna acc xs else repna (acc + x) xs 
  in 
  repna 0

let map_list f_list a = 
  let rec repna acc = function
    |[] -> acc 
    |f::fs -> repna ((f a)::acc) fs 
  in 
  List.rev (repna [] f_list)

let apply f_list a_list =
  let rec repna acc = function
    |[] -> acc
    |x::xs -> repna ((map_list f_list x)::acc) xs 
  in
  List.rev (repna [] a_list)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = |Predavanje |Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje : int}

type urnik = srecanje list list

let vaje =  {predmet = "Analiza 2a" ; vrsta = Vaje; trajanje = 3}

let predavanja = {predmet = "Programiranje 1" ; vrsta = Predavanje; trajanje = 2}

let urnik_profesor = [ {predmet = "Analiza 2a" ; vrsta = Vaje; trajanje = 2}::[];[];[{predmet = "Programiranje 1" ; vrsta = Predavanje; trajanje = 1}];[];[];[{predmet = "Programiranje 1" ; vrsta = Vaje; trajanje = 1}]   ]

let ( **+ )(a, b) (c, d) = (a + c, b + d)

let rec sestej_dan = function  
    |[] -> (0, 0)
    |pouk::ostalo -> if pouk.vrsta = Vaje then (sestej_dan ostalo) **+ (0, pouk.trajanje)
                    else (sestej_dan ostalo) **+ (pouk.trajanje, 0)

let je_preobremenjen urnik = 
    let rec repna pred vaje = function
        |[] -> (pred, vaje)
        |dan::preostanek -> let tmp = sestej_dan dan in 
                            repna (pred + (fst tmp)) (vaje + (snd tmp)) preostanek
        in 
    let tmp = repna 0 0 urnik in 
    if (fst tmp) <= 4 && (snd tmp) <= 4 then true else false

let bogastvo urnik = 
    let rec repna pred vaje = function
        |[] -> (pred, vaje)
        |dan::preostanek -> let tmp = sestej_dan dan in 
                            repna (pred + (fst tmp)) (vaje + (snd tmp)) preostanek
        in 
    let tmp = repna 0 0 urnik in 
    (fst tmp * 2) + (snd tmp)
