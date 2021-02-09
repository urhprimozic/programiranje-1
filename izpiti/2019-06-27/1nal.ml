type complex = {re : float; im : float}

let complex_add z w = {re=z.re +. w.re; im=z.im +. w.im}

let complex_conjugate z = {re=z.re; im = -. z.im}

let rec list_apply_either pred f g = function
    |[] -> []
    |x::xs -> if pred x then (f x)::(list_apply_either pred f g xs) else (g x)::(list_apply_either pred f g xs)

let pow x a = 
    let rec repna acc b =
        if b = 0 then acc 
        else repna (x*acc) (b-1)
    in 
    repna 1 a 

let eval_poly p x = 
    let rec repna acc = function
        |[] -> acc 
        |k::ostanek -> 
            let potenca = List.length ostanek in 
            repna (acc + k * (pow x potenca)) ostanek
    in
    repna 0 (List.rev p)