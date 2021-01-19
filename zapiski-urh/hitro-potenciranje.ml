let rec s_pow a n = match n with
    |0 -> 1
    |n -> let k = s_pow a (n/2) in 
        if n mod 2 = 0 then k * k else a * k * k