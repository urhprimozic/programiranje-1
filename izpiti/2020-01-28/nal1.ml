let option_sum a b = match a, b with
    |None, _ |_, None -> None 
    |Some x, Some y ->  Some (x + y ) 

let twostep_map f g h x = let ans = f x in 
                        let b = fst ans in 
                    let c = snd ans in 
                    (g b, h c)

(*O(n), vendar sploh ni rekruzivna *)
let repeat x n = let tmp = Array.init n (fun i -> x) in 
                Array.to_list tmp 

let function_repeat f = 
    let rec repna acc = function 
        |[] -> acc 
        |x::xs -> repna ( (repeat x (f x))@acc) xs 
        in
    repna []
    (* repna je res repna, ker edinkrat ko kliče sama sebe to anrdi NA KONCU*)

              
let rec interate f p x = 
    if p x then x
    else interate f p (f x)