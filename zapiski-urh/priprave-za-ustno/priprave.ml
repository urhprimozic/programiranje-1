let memo = Hashtbl.create 1024

let rec fibo_mem n=
    match Hashtbl.find_opt memo n with
        |Some m -> m 
        |None -> (match n with
                    |0 |1 -> 1 
                    |m ->( let rez = fibo_mem(m-1) + fibo_mem(m-2) in 
                          Hashtbl.add memo n rez;
                          rez  )
        )
    



let rec  fibo =function 
    |0 |1 -> 1 
    |n -> fibo(n-1) + fibo(n-2)

let odviti_f g = function
    |0 |1 -> 1
    |n -> g (n-1) + g (n)

let memoiziraj_rec odviti_f = 
    let memo = Hashtbl.create 1024 in 
    let rec mem_f n = 
        match Hashtbl.find_opt memo n with
            |Some m -> m 
            |None ->  (let rez = odviti_f mem_f n in 
                        Hashtbl.add memo n rez;
                        rez)
        in 
    mem_f

