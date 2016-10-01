(* 
A command line to obtain statistics 
compile with flag "-o stats"
*)

try
    begin match Sys.argv with
        | [|_; filename|] -> 
            let stats = Textstat.stats_from_file filename in
                Textstat.print_statistics stats;
        | _ -> print_string "Usage: stats <filename>";
            print_newline ();
    end
with
    e -> print_string "An error occurred : ";
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1;;