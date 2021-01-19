type xytree =
    | Xsplit of int * xytree * xytree
    | Ysplit of int * xytree * xytree
    | Elements of (int * int) list

let example = Xsplit (2, Ysplit(3, Elements [(0, 2); (1, 1)] ,  Elements []), Ysplit(2, Elements [(3, 1)], Xsplit(4,Elements [(4, 3)], Elements [])))

let rec num_of_elements = function
    |Elements sez -> List.length sez
    |Xsplit (_, t1, t2) -> (num_of_elements t1) + (num_of_elements t2)
    |Ysplit (_, t1, t2) -> (num_of_elements t1)  + (num_of_elements t2)

let rec insert tree (x, y) = match tree with
    |Elements sez -> Elements ((x,y)::sez)
    |Xsplit (x0, levo, desno) -> if x <= x0 then Xsplit (x0, insert levo (x, y), desno)
                                else Xsplit (x0, levo ,insert desno (x, y))
    |Ysplit (y0, spodnje, zgornje) -> if y <= y0 then Ysplit (y0, insert spodnje (x, y), zgornje)
                                    else Ysplit (y0, spodnje, insert zgornje (x,y))

let rec insert_wihout_duplicates tree (x, y) = match tree with
    |Elements sez -> if List.mem (x,y) sez then Elements sez
                    else  Elements ((x,y)::sez)
    |Xsplit (x0, levo, desno) -> if x <= x0 then Xsplit (x0, insert_wihout_duplicates levo (x, y), desno)
                                else Xsplit (x0, levo ,insert_wihout_duplicates desno (x, y))
    |Ysplit (y0, spodnje, zgornje) -> if y <= y0 then Ysplit (y0, insert_wihout_duplicates spodnje (x, y), zgornje)
                                    else Ysplit (y0, spodnje, insert_wihout_duplicates zgornje (x,y))

let rec alternates = function
    |Elements _ -> true
    |Xsplit (_, Xsplit _, _) -> false
    |Xsplit (_,_, Xsplit _) -> false
    |Xsplit (_, t1, t2) -> (alternates t1) && (alternates t2)
    |Ysplit (_, Ysplit _, _) -> false
    |Ysplit (_,_, Ysplit _) -> false
    |Ysplit (_, t1, t2) -> (alternates t1) && (alternates t2)

let rec extract = function
    |Elements sez -> sez 
    |Xsplit (_, t1, t2) -> (extract t1) @ (extract t2)
    |Ysplit (_, t1, t2) -> (extract t1) @ (extract t2)
    

let boxed_correctly tree =
    let nodes = extract tree in 
    let richtig = List.fold_left insert_wihout_duplicates (tree) nodes in 
    richtig = tree 