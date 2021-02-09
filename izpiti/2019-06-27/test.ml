type test = Value of int | Parent of test 

let f = function 
    |Value y -> Some  y 
    |Parent _ -> None 

let g = function
    |Parent x -> (match x with
            |Value a -> Some a
            |_ -> None)
    |_ -> Some 1
