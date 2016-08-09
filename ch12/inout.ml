let print_dict_entry (k, v) =
    print_int k;
    print_newline();
    print_string v;
    print_newline ();;

let rec iter f d =
    match d with
        | [] -> ()
        | h :: t -> f h; iter f t;;

let print_dict = iter print_dict_entry;;

let rec read_dict () =
    try
        print_string "Enter Key - 0 to stop : ";
        let k = read_int () in
            if k = 0 then
                []
            else
                (print_string "Enter value: ";
                let v = read_line () in
                    (k, v) :: read_dict ())
    with
        Failure _ ->
            print_string "This is not a valid integer, please try again"; print_newline (); read_dict ();;


print_dict [(1, "one"); (2, "two"); (3, "three")];;

(* print_dict (read_dict ());; *)

let entry_to_channel ch (k, v) =
    output_string ch (string_of_int k);
    output_char ch '\n';
    output_string ch v;
    output_char ch '\n';;

let dictionary_to_channel ch = iter (entry_to_channel ch);;

let dictionary_to_file   file dict =
    let ch = open_out file in
        dictionary_to_channel ch dict;
        close_out ch;;

let entry_of_channel ch =
    let number = input_line ch in
        let name = input_line ch in
            (int_of_string number, name);;

let rec dictionary_of_channel ch =
    try
        let e = entry_of_channel ch in
            e :: dictionary_of_channel ch
    with
        End_of_file -> [];;

let dictionary_of_file file =
    let ch = open_in file in
        let dict = dictionary_of_channel ch in
            close_in ch;
            dict;;

let rec print_elements l =
    match l with
        | [] -> ()
        | [x] -> print_int x
        | h :: t -> print_int h; print_string "; "; print_elements t;;

let rec print_list l =
    print_char '[';
    print_elements l;
    print_char ']';
    print_newline ();;

let rec read_three_tuple () =
    try
        print_string "Provide first entry : ";
        let first = read_int () in
            print_string "Provide second entry : ";
            let second = read_int () in
                print_string "Provide third entry : ";
                let third = read_int () in
                    (first, second, third)
    with
        Failure _ ->
            print_string "This is not a valid integer, please try again"; print_newline (); read_three_tuple ();;

let read_dict2 () =
    let rec read_entries cnt =
        if cnt > 0 then
            try
                print_string "Enter Key : ";
                let k = read_int () in
                    if k = 0 then
                        []
                    else
                        (print_string "Enter value: ";
                        let v = read_line () in
                            (k, v) :: read_entries (cnt - 1))
            with
                Failure _ ->
                    print_string "This is not a valids integer, please try again"; print_newline (); read_entries cnt;
        else
            []
    in
        print_string "Enter dictionary size: ";
        let v = read_int () in
            read_entries v;;