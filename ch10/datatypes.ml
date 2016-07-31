type color = 
    Red 
    | Green 
    | Blue 
    | Yellow
    | RGB of int * int * int;;

let colors = [Red; Red; Green; Yellow; RGB (150, 0, 255)];;

let cp = ('R', Red);;

let components c = 
    match c with
        | Red -> (255, 0, 0)
        | Green -> (0, 255, 0)
        | Blue -> (0, 0, 255)
        | Yellow -> (255, 255, 0)
        | RGB (r, g, b) -> (r, g, b);;

(* type names start with small case and contructors with Uppercase; type parameter start with ' -- single quote *)
type 'a option = None | Some of 'a;;

let nothing = None;;

let number = Some 50;;

let numbers = [Some 10; None; number];;

let word = Some [ 'o'; 'p'; 't'; 'i'; 'o'; 'n'];;

let rec lookup x l = 
    match l with
        | [] -> None
        | (k, v) :: t -> if x = k then
                            Some v
                         else
                            lookup x t;;


(* type parameter are always before the type, type values are after the type *)
type 'a sequence = Nil  | Cons of 'a * 'a sequence;;

let int_seq = Cons (10, Nil);;

let empty_seq = Nil;;

let char_seq = Cons ('a', Cons ('b', Cons ('c', Nil)));;

let rec length l = 
    match l with
        | Nil -> 0
        | Cons (_, t) -> 1 + length t;;

let rec append a b =
    match a with
        | Nil -> Cons (b, Nil)
        | Cons (h, t) -> Cons (h, append t b);;

type expr =
    | Num of float
    | Add of expr * expr
    | Multiply of expr * expr
    | Subtract of expr * expr
    | Divide of expr * expr
    | Power of expr * expr;;

let math = Add (Num 1., Multiply (Num 2., Num 3.));;

let rec evaluate expr = 
    match expr with
        | Num x -> x
        | Add (e1, e2) -> evaluate e1 +. evaluate e2
        | Multiply (e1, e2) -> evaluate e1 *. evaluate e2
        | Subtract (e1, e2) -> evaluate e1 -. evaluate e2
        | Divide (e1, e2) -> evaluate e1 /. evaluate e2
        | Power (e1, e2) -> evaluate e1 ** evaluate e2;;

type rect = Rectangle of int * int | Square of int;;

let area r = 
    match r with
        | Rectangle (w, h) -> w * h
        | Square s -> s * s;;

let rotate r =
    match r with
        | Rectangle (w, h) when h < w -> Rectangle (h, w)
        | _ -> r;;

let rec merge cmp x y =
    match (x, y) with
        | hx :: tx, hy :: ty -> 
            if cmp hx hy < 0 then
                hx :: merge cmp tx y
            else
                hy :: merge cmp x ty
        | [], o | o, [] -> o;;

let rec length x =
    match x with
        | [] -> 0
        | _ :: t -> 1 + length t;;

let rec take n x =
    if n > 0 then
        match x with
            | [] -> []
            | h :: t -> h :: take (n - 1) t
    else
        [];;

let rec drop n x =
    if n > 0 then
        match x with
            | [] -> []
            | h :: t -> drop (n - 1) t
    else
        x;;

let rec msort cmp x =
    match x with
        | [] | [_] -> x
        | _ ->
            let len = length x in
                merge cmp (msort cmp (take (len / 2) x)) (msort cmp (drop (len / 2) x));;

let rec map f l =
    match l with
        | [] -> []
        | h :: t -> (f h) :: map f t;;

let width x = match x with
    | Rectangle(f, _) -> f
    | Square s -> s;;

let reorder rl = msort (fun x y -> (width x) - (width y)) (map rotate rl);;

let rec take n x =
    match x with
        | Cons (e, r) when n > 0 -> Cons (e, take (n - 1) r)
        | _ -> Nil;;

let rec drop n x =
    match x with
        | Cons (e, r) when n > 0 -> drop (n - 1) r
        | r -> r;;

let rec map f x =
    match x with
        | Nil -> Nil
	    | Cons (e, r) -> Cons (f e, map f r);;
