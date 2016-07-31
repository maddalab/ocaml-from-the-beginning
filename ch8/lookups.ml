let first p = match p with f, _ -> f;;
let second p = match p with _, s -> s;;

let fst (f, _) = f;;
let snd (_, s) = s;;

let census = [(1,4); (2,2); (3, 2); (4, 3); (5,1); (6,2)];;

let rec lookup k l = 
  match l with
    | [] -> raise Not_found
    | (k', v') :: _ when k = k' -> v'
    | _ :: t -> lookup k t;;

let rec add k v l =
  match l with
    | [] -> [(k, v)]
    | (k', v') :: t -> if k = k' then
                         (k, v) :: t
                       else
                         (k', v') :: add k v t;;

let rec remove k l =
  match l with
    | [] -> []
    | (k', v') :: t -> if k' = k then
                        t
                      else
                        (k', v') :: remove k t;;

let rec key_exists k l =
  try
    let _ = lookup k l in true
  with
    Not_found -> false;;

let size d = List.length d;;

let replace k v l =
  let _ = lookup k l in add k v l;;

let rec make_dict lx ly = 
  match lx, ly with
    | hx :: tx, hy :: ty -> (hx, hy) :: make_dict tx ty
    | [], [] -> []
    | _ -> raise (Invalid_argument "unequal lists");;

let rec explode_dict d =
  match d with
    | [] -> ([],[])
    | (k, v) :: t -> let (keys, values) = explode_dict t in (k :: keys, v :: values);;

let rec combine x y = 
  let d = x in
    match y with 
      | [] -> d
      | (k, v) :: t -> if key_exists k d then
                    combine d t
                  else
                    combine (add k v d) t;;