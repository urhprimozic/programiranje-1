type najemnik = string

type vrt = 
    | Obdelovan of najemnik
    | Oddan of najemnik * (vrt * vrt list)
    | Prost

let vrt_primer = Oddan ("Kovalevskay", (Obdelovan "Galois", [Obdelovan "Lagragrange"; Prost]))

let obdelovalec_vrta = function
    |Obdelovan y -> Some y 
    |_ -> None 


let rec  globina_oddajanja = function
    |Oddan (_, (x, rest)) -> 
       ( match rest with
            |[] -> 1 + (globina_oddajanja x )
            |ys -> 1 + (List.fold_left max (globina_oddajanja x) (List.map globina_oddajanja ys)))
            
    |_ -> 0

let rec v_uporabi = function
    |Obdelovan _ -> true 
    |Prost -> false 
    |Oddan (_, (v, v_list)) -> (match v_list with
                                |[] -> v_uporabi v 
                                |vs -> List.fold_left (||) (v_uporabi v) (List.map v_uporabi v_list)
    )

let rec vsi_najemniki = function
    |Prost -> [] 
    |Obdelovan n -> [n]
    |Oddan (n, (v, vlist)) -> n::(v::(List.map vsi_najemniki vlist) )