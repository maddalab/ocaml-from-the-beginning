let rec member x l = 
  match l with
    | [] -> false
    | h :: t -> if h = x then
                  true
                else 
                   member x t;;

let rec map f l =
  match l with
  | [] -> []
  | h :: t -> f h :: map f t;;

let rec member_all x ls =
    let contains = map (member x) ls in
        let rec check ls = 
            match ls with
                | [true] -> true
                | true :: t -> check t
                | _ -> false
        in check contains;;

let div d n = n / d;;

let halve l = map (div 2) l;;

let mapll f l = map (map (map f)) l;;


let truncate i l = map (map (fun s -> String.sub s 0 (if i < String.length s then i else String.length s))) l;;

let rec take n l = 
    if n = 0 then
        []
    else
        match l with
            | [] -> []
            | h :: t -> h :: take (n - 1) t;;

let rec length l =
    match l with
        | [] -> 0
        | _ :: t -> 1 + length t;;

let truncate_l n l = if n < length l then take n l else l;;

let truncate_ll n ls = map (truncate_l n) ls;;

let first d l = 
    match l with
        | [] -> d
        | h :: _ -> h;;

let first_l d ls = map (first d) ls;;

let first_ll d lls = map (first_l d) lls;;

let firsts d l = map (map (first d)) l;;