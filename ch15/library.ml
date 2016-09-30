let concat lst =
    let rec cf hl tl = 
        match tl with
            | h :: t -> cf (hl @ h) t
            | [] -> hl
    in match lst with
        | hl :: tl -> cf hl tl
        | [] -> [];;

let mem bll =
    let rec check rst =
        match rst with
            | h :: t -> (List.mem true h) :: check t
            | [] -> [true]
    in
        List.find (fun x -> x == false) (check bll);;

let scntc s c =
    let cnt = ref 0 in
        String.iter (fun x -> if x == c then cnt := !cnt + 1) s;
        !cnt;;

let srepc s c r =
    String.map (fun x -> if x == c then r else x) s;;


let concat lst = String.concat "";;

let concat2 ss =
  let b = Buffer.create 100 in
    List.iter (Buffer.add_string b) ss;
    Buffer.contents b;;

let occurences s =
    let t = String.map (fun x ->
        match x with
            | 'o' | 'c' | 'a' | 'm' | 'l' -> x
            | _ -> ' ' ) (String.lowercase_ascii s) in
    let b = Buffer.create (String.length t) in
            String.iter (fun c -> if c != ' ' then Buffer.add_char b c) t;
            let f = Buffer.contents b and
                cnt = ref 0 in
                for i = 0 to (String.length f) - (String.length "ocaml") do
                    let ss = String.sub f i (String.length "ocaml") in
                        if String.equal "ocaml" ss then
                            cnt := !cnt + 1;
                done;
                !cnt;;