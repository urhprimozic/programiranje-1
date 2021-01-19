(* list in list conatins*)
let list_contains =  List.mem 

(*tabela array*)
let array_reverse_new tabela= 
    (*Iz zapiskov *)
    let n = Array.length tabela in
    Array.init n (fun i -> tabela.(n - i - 1))


let array_reverse_onplace tabela= 
    (*UČINEK. Vrne unit, ampak obrne tabela *)
    let switch i j =
        let tmp = tabela.(i) in 
        tabela.(i) <- tabela.(j);
        tabela.(j) <- tmp
    in
    let n = Array.length tabela in 
    for i = 0 to (n / 2) -1 do 
        switch i ((n-1) - i)
    done

(*hitro potenciranje fast pow  *)
let rec s_pow a n = match n with
    |0 -> 1
    |n -> let k = s_pow a (n/2) in 
        if n mod 2 = 0 then k * k else a * k * k

