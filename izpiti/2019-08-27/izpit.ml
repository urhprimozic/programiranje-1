let odstej_trojici (a, b, c) (d, e, f) = (a- d, b- e, c- f)

let rec ustvari_padajoc_seznam n = 
    if n < 0 then []
    else
        let rec repna acc n = 
            if n = 0 then 0::acc
            else repna (n::acc) (n-1)
            in 
        repna [] n

let rec list_max = function
    |[] -> 0
    |x::[] -> x (* Potrebujem tole, da ne pride do situacije, kjer bi bil xs enak []*)
    |x::y::[] -> max x y
    |x::xs -> max x (list_max xs) (* xs zagodovo ni []*)

let max_rezultat_do_n f n = 
    list_max (List.map f (ustvari_padajoc_seznam n))

let rec pocisti_seznam = 

    let rec repna acc = function
        |[] -> acc
        |x::xs -> match x with
                    |None -> repna acc xs
                    |Some y -> repna (y::acc) xs
    in 
    repna []

let izlusci_lihe_sode sez = 
    let rec repna lihi sodi = function
        |[] -> (lihi, sodi)
        |x::xs -> if (x mod 2) == 0 then repna lihi (x::sodi) xs 
                else repna (x::lihi) sodi xs
    in 
    let ans = repna [] [] sez in
    ((List.rev (fst ans)), (List.rev (snd ans)) )

let preveri_urejenost sez = 
    let locen = izlusci_lihe_sode sez in 
    let lihi = fst locen in 
    let sodi = snd locen in 
    if (lihi = List.sort (fun a b -> b - a) lihi) && (sodi = List.sort (fun a b -> a  - b) sodi) then true
    else false

(* 2. naloga*)
type 'a gnezdenje =
    | Element of 'a 
    | Podseznam of 'a gnezdenje list

let gnezdenje_primer = Podseznam [Element 1; Element 2; Podseznam [Element 3; Podseznam [Element 4] ; Podseznam []] ; Podseznam [Element 5]]

let najvecja_globina gsez = 
    let rec repna maxi = function
        |Element _ -> maxi 
        |Podseznam xs -> (list_max (List.map (repna maxi) xs)) + 1
        in 
    repna 0 gsez

let rec preslikaj f = function
    |[] -> []
    |x::xs -> match x with
                |Element y -> (f y)::(preslikaj f xs)
                |Podseznam ys -> (Podseznam (preslikaj f ys))::(preslikaj f xs)

let rec splosci = function
    |[] -> []
    |x::xs -> match x with 
            |Element y -> y::(splosci xs)
            |Podseznam ys -> (splosci ys)@(splosci xs)

let alternirajoci_konstruktorji gsez=
    (* zadnji= true -> zadji je biu element*)
    (* zadnji= false -> zadji je biu podseznam*)
    let rec kao_repna zadnji = function
        |[] -> true
        |(Element _)::xs -> if zadnji=true then false else kao_repna true xs 
        |(Podseznam _ )::xs -> if zadnji=false then false else kao_repna false xs
    in 
    match gsez with
        |[] -> true
        |(Element _)::xs -> kao_repna true xs 
        |_::xs -> kao_repna false xs 

let gsez_primer =  [Element 1; Element 2;
  Podseznam [Element 3; Podseznam [Element 4]; Podseznam []];
  Podseznam [Element 5]]

let rec zlozi_gnezdenje f acc g = match g with 
    | Element x -> f acc x 
    | Podseznam xs -> List.fold_left (zlozi_gnezdenje f) acc xs

let zlozi_preko_gnezdenja f acc glist =
    zlozi_gnezdenje f acc (Podseznam glist)
