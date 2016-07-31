let take n l = if n <= 0 then
                 raise (Invalid_argument "take n <= 0")
               else if n > List.length l then
                 raise (Invalid_argument "take n > List.length")
               else
                 let rec itake n l = match l with
                   | h :: t when n > 0 -> h :: itake (n - 1) t
                   | _ -> []
                 in itake n l;;
                 
let drop n l = if n <= 0 then
                 raise (Invalid_argument "drop n <= 0")
               else if n > List.length l then
                 raise (Invalid_argument "drop n > List.length")
               else
                 let rec idrop n l = match l with
                   | h :: t when n > 0 -> idrop (n - 1) t
                   | _ -> l
                 in idrop n l;;

let safe_divide x y = try x / y with
    Division_by_zero -> 0;;

let rec last l = match l with
  | [] -> raise Not_found
  | [x] -> x
  | _ :: t -> last t;;