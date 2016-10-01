type stats = int * int * int * int * int array;;

let lines (l, _, _, _, _) = l;;

let characters (_, c, _, _, _) = c;;

let words (_, _, w, _, _) = w;;

let sentences (_, _, _, s, _) = s;;

let print_histogram (_, _, _, _, arr) = 
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
    done;;

let print_statistics stats =
    print_string "Words : ";
    print_int (words stats);
    print_newline ();
    print_string "Characters : ";
    print_int (characters stats);
    print_newline ();
    print_string "Sentences : ";
    print_int (sentences stats);
    print_newline ();
    print_string "Lines : ";
    print_int (lines stats);
    print_newline ();
    print_histogram stats;;


let stats_from_channel in_channel = 
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
                            | '.' | '?' | '!' -> sentences := !sentences + 1
                            | ' ' -> words := !words + 1
                            | _ -> ()
                    ) line;
                    String.iter (fun c ->
                        histogram.(int_of_char c) <- histogram.(int_of_char c) + 1
                    ) line;
            done;
            (!lines, !characters, !words, !sentences, histogram)
        with
            End_of_file -> (!lines, !characters, !words, !sentences, histogram);;

let stats_from_file filename =
    let channel = open_in filename in
        let result = stats_from_channel channel in
            close_in channel;
            result;;