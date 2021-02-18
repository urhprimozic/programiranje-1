let rec nerepna_vsota = function
     |[] -> 0
     |x::xs -> x + (nerepna_vsota xs)

let repna_vstoa = 
    let rec  repna acc = function
        |[] -> acc 
        |x::xs ->  repna (acc +x) xs
    in 
    repna 0

F n = f(n-1) + f(n-2) = 
f(n-1) = f(n) - f(n-2) = f(n-1) + 0*F(n-2)



f(n-1)  f(n-2)   *  [] == 
f(n-1)   0


M = 1 1 
    1 0

[F(n) , f(n-1)] =  [1* f(n-1) + 1* f(n-2), 1* f(n-1) + 0*F(n-2)] = M * [F(n-1),F(n-2) ] = M*M *[F(n-2, f(n-3))] ) --- = M^(n-1) [1 1]

[1,4,2,7,4,6,8,6]
T(n) = O(n) + T(n/2) = O(n) + O(n/2) + O(n/4) + T(n/8) ...= O(n)



[1,4], [2, 3]  ---> [1,2, 3, 4 ]
