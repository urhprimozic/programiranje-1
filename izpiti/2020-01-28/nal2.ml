type 'a improved_list = 
    |Empty 
    |Sest of 'a array * 'a improved_list

let ( **/ ) tabela i_list = Sest (tabela, i_list)

let test = [|1;2;20|]**/[|17;19;20;30|]**/[|100|]**/Empty

let nth_table n i_list=
    let rec repna index = function
        |Empty -> None 
        |Sest (t, ts) -> if index = n then Some t else repna (index +1) ts 
    in 
    repna 0 i_list

 let nth n i_list = 
    let rec f index = function
        |Empty -> None 
        |Sest (t, ts) -> 
                 if Array.length t = 0 then f index ts 
            else if (Array.length t) + index <= n then f ((Array.length t) + index) ts
            else Some t.(n - index)
    in 
    f 0 i_list


let rec is_sorted = function
    |Empty -> true 
    |Sest (t, Empty) -> true
    |Sest (t1, Sest(t2, rest)) -> if t1 < t2 && is_sorted (Sest(t2, rest)) then true 
                                else false

let update i_list n x =
    let rec f index acc = function
        |Empty -> acc 
        |Sest (t, ts) -> 
                 if Array.length t = 0 then f index ([||]**/acc) ts 
            else if (Array.length t) + index <= n then f ((Array.length t) + index) ( (Array.copy t)**/acc) ts
            else if index > n then f ((Array.length t) + index) ( (Array.copy t)**/acc) ts
            else let t' = Array.copy t in 
                    t'.(n - index) <- x;
                    f ((Array.length t) + index) (t'**/acc) ts
    in 
    f 0 Empty i_list

