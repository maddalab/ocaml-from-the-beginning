type 'a tree = 
    Br of 'a * 'a tree * 'a tree
    | Lf;;

let single_node_tree = Br (1, Lf, Lf);;

let left_sub_tree = Br ("top", Br ("left", Lf, Lf), Lf);;

let balanced_tree = Br ("top", Br ("left", Lf, Lf), Br ("right", Lf, Lf));;


let rec size tr = 
    match tr with
    | Lf -> 0
    | Br (_, lt, rt) -> 1 + size lt + size rt;;

let rec total tr =
    match tr with
        | Lf -> 0
        | Br (v, lt, rt) -> v + total lt + total rt;;

let rec maxdepth tr =
    match tr with
        | Lf -> 0
        | Br (_, lt, rt) -> 
            let ld = maxdepth lt in
                let rd = maxdepth rt in
                    if ld > rd then
                        1 + ld
                    else
                        1 + rd;;

let rec make_list tr =
    match tr with
        | Lf -> []
        | Br (v, lt, rt) -> (make_list lt) @ [v] @ (make_list rt);;

let rec tree_map f tr =
    match tr with
        | Lf -> Lf
        | Br (v, lt, rt) -> Br (f v, tree_map f lt, tree_map f rt);;


(* Trees as better dictionaries with binary search trees *)
let rec lookup k tr =
    match tr with
        | Lf -> None
        | Br ( (k', v'), lt, rt) -> 
            if k = k' then
                Some v'
            else if k < k' then
                lookup k lt
            else
                lookup k rt;;

let rec insert k v tr =
    match tr with
        | Lf -> Br ( (k, v), Lf, Lf)
        | Br ( (k', v'), lt, rt) ->
            if k' = k then
                Br ( (k, v), lt, rt)
            else if k' < k then
                Br ( (k', v'), lt, insert k v rt)
            else
                Br ( (k', v'), insert k v lt, rt);;

(* initial dictionary *)
let d = Lf;;

(* insert (1, "one") *)
let one_node = insert 1 "one" d;;

(* insert in order (3, "three") (1, "one") (2, "two")  (4, "four") (0, "zero") *)
let digit_tree = insert 0 "zero" (insert 4 "four" (insert 2 "two" (insert 1 "one" (insert 3 "three" Lf))));;

let rec exists k tr =
    match tr with
        | Lf -> false
        | Br ( (k', v'), lt, rt) ->
            if k' = k then
                true
            else if k < k' then
                exists k lt
            else
                exists k rt;;

let rec flip tr =
    match tr with
        | Lf -> Lf
        | Br ( n, lt , rt) -> Br (n, flip rt, flip lt);;

let rec shape tr ts =
    match (tr, ts) with
        | (Lf, Lf) -> true
        | (Br ( _, ltr, rtr), Br (_, lts, rts)) -> (shape ltr lts) && (shape rtr rts)
        | _ -> false;;

let char_tree = insert 'e' "e" (insert 'a' "a" (insert 'c' "c" (insert 'b' "b" (insert 'd' "d" Lf))));;

let tree_of_list l =
    let rec acc tr l =
        match l with
            | [] -> tr
            | (k, v) :: t -> acc (insert k v tr) t
    in acc Lf l;;

let rec merge tr ts  =
    match ts with
        | Lf -> tr
        | Br ( (k, v), ls, rs) ->
            if exists k tr then
                merge (merge tr ls) rs
            else
                merge (merge (insert k v tr) ls) rs;;

(* define an n-ary tree *)
type 'a ntree = 
    Br of 'a * 'a ntree list;;

let rec map f l =
    match l with
        | [] -> []
        | h :: t -> (f h) :: map f t;;

let rec add l = 
    match l with
        | [] -> 0
        | h :: t -> h + add t;;
        
let rec size ntr =
    match ntr with
        | Br (_, l) -> 1 + add (map size l);;

let rec total ntr =
    match ntr with
        | Br (v, l) -> v + add (map total l);;

let rec ntreemap f ntr =
    match ntr with
        | Br (v, l) -> Br (f v, map (ntreemap f) l);;


(* single node with 4 childre *)
let n_ary_digit_tree = Br (1, [Br (2, [Br (6, []); Br (7, [])]); Br (3, [Br (8, [])]); Br (4, [Br (9, [])])]);;
