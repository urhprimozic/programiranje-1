(* Današnja učna ura: večina jezikov ima 2 funkciji tangenz, tan in tan2*)
let angle_between (x1, y1) (x2, y2) = 
    (* Vrne kot v radianih .Tenx ocaml docs*)
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in  
    Float.atan2 dy dx

let list_to_triple = function
    |x::y::z::[] -> Some (x,y,z)
    |_ -> None

type counter = {lt : int; eq : int; gt: int}



let compare_with sez x =
    let rec repna acc = function
        |[] -> acc 
        |y::ys -> if y > x then  repna {lt = acc.lt; eq = acc.eq; gt = acc.gt + 1} ys
             else if y = x then repna {lt = acc.lt; eq = acc.eq +1; gt = acc.gt} ys
             else  repna {lt = acc.lt + 1; eq = acc.eq ; gt = acc.gt} ys
    in 
    repna {lt=0;eq=0;gt=0} sez 

let apply_all f_list x =
    let rec repna acc = function 
        |[] -> acc 
        |y::ys -> repna (y acc) ys 
    in 
    repna x f_list
