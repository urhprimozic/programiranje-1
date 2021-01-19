let dot_prod (x1, y1, z1) (x2, y2, z2) = (x1*.x2)+.(y1*.y2)+.(z1*.z2)

let fix_second f x y = f y x

let combine_and_filter f xs ys = 
    let rec repna acc = function
        |([],_) |(_, [])-> acc
        |(x::xx, y::yy) -> let c = f x y in 
                        match c with
                             |None -> repna acc (xx, yy)
                             |Some cc-> repna (cc::acc) (xx, yy)
    in 
    List.rev (repna [] (xs, ys))

let rec conditional_print p = function
    |[] -> ()
    |str::[] -> print_string str
    |str::rep -> (if p str then (print_string str); (print_string ", ") );
                conditional_print p rep
