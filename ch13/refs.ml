(* create a reference *)
let x = ref 0;;

(* access the reference *)
let p = !x;;

(* update content *)
x := 50;;

(* access the reference *)
let q = !x;;

let swap a b =
    let t = !a in
        a := !b; b := t;;

let smallest_pow2 x =
    let t = ref 1 in
        while !t < x do
            t := !t * 2
        done;
        !t;;

let channel_statistics in_channel =
    let lines = ref 0 in
        try
            while true do
                let _ = input_line in_channel in
                    lines := !lines + 1
            done
        with
            End_of_file ->
                print_string "There were ";
                print_int !lines;
                print_string " lines.";
                print_newline ();;

let channel_statistics2 in_channel =
    let lines = ref 0 and
        characters = ref 0 and
        words = ref 0 and
        sentences = ref 0 and
        histogram = Array.make 255 0 in
        try
            while true do
                let line = input_line in_channel in
                    lines := !lines + 1;
                    characters := !characters + String.length line;
                    String.iter (fun c ->
                        match c with
                            | ' ' -> words := !words + 1
                            | '.' | '?' | '!' -> sentences := !sentences + 1
                            | _ -> ()
                    ) line;
                    String.iter (fun c ->
                        histogram.(int_of_char c) <- histogram.(int_of_char c) + 1
                    ) line;
            done
        with
            End_of_file ->
                print_string "There were ";
                print_int !lines;
                print_string " lines, making up ";
                print_int !characters;
                print_string " characters with ";
                print_int !words;
                print_string " words in ";
                print_int !sentences;
                print_string " sentences ";
                print_newline ();
                print_histogram histogram;;


let print_histogram arr =
    print_string "Character frequencies";
    print_newline ();
    for x = 0 to (Array.length arr) - 1 do
        if arr.(x) > 0 then
        begin
            print_string "For character '";
            print_char (char_of_int x);
            print_string "' (character number ";
            print_int x;
            print_string ") the count is ";
            print_int arr.(x);
            print_string ".";
            print_newline ();
        end
    done

let fs fn name =
    let channel = open_in name in
        try
            fn channel;
            close_in channel;
        with
            _ -> close_in channel;;    

let file_statistics = fs channel_statistics;;

let file_statistics2 = fs channel_statistics2;;


let array_sum arr =
    let sum = ref 0 in
        for x = 0 to (Array.length arr) - 1 do
            sum := !sum + arr.(x);
        done;
        !sum;;

let reverse arr =
    let len = Array.length arr - 1 in
        if len > 0 then
            for x = 0 to len / 2 do
                print_int x;
                let t = arr.(x) in
                    begin
                        arr.(x) <- arr.(len - x);
                        arr.(len - x) <- t
                    end;
            done;;

let table size =
    let arr = Array.make size (Array.make size 0) in
        for x = 0 to size - 1 do
            arr.(x) <- (Array.make size 0);
            for y = 0 to size - 1 do
                arr.(x).(y) <- (x + 1) * (y + 1);
            done;
        done;
        arr;;

let print_table arr size =
    for x = 0 to size - 1 do
        for y = 0 to size - 1 do
            print_int arr.(x).(y);
            print_string " ";
        done;
        print_newline ();
    done;;

let uppercase str =
    String.map (fun c -> 
        let i = int_of_char c and
            sa = int_of_char 'a' and
            sz = int_of_char 'z' and
            ca = int_of_char 'A' in
        if i >= sa && i <= sz then
            char_of_int (i - sa + ca)
        else
            char_of_int i
    ) str;;

let lowercase str =
    String.map (fun c -> 
        let i = int_of_char c and
            ca = int_of_char 'A' and
            cz = int_of_char 'Z' and
            sa = int_of_char 'a' in
        if i >= ca && i <= cz then
            char_of_int (i - ca + sa)
        else
            char_of_int i
    ) str;