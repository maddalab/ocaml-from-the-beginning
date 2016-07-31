let smallest l = match l with
  | [] -> raise Not_found
  | [x] when x < 0 -> raise Not_found
  | h :: t -> let rec ts c l = match l with
                                 | [] -> if c < 0 then
                                           raise Not_found
                                         else
                                           c
                                 | f :: r -> if f > -1 then
                                               if c < 0 || f < c then
                                                 ts f r
                                               else
                                                 ts c r
                                             else
                                               ts c r
              in ts h t;;

let smallest_or_zero l = try smallest l with
                           Not_found -> 0;;

exception NoNegativeRoot of int;;

let largest_int_smaller_root n = if n < 1 then
                                   raise (NoNegativeRoot n)
                                 else
                                   let root = sqrt (float_of_int n) in
                                     truncate root;;

let safe_largest_int_smaller_root n = try largest_int_smaller_root n with
                                        NoNegativeRoot _ -> 0;;