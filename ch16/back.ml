(*
Takes 2 filenames, input and output, reverses all lines in input file
to output file

compile: ocamlc/opt reverse.mli reverse.ml back.ml -o back

*)

try
    begin match Sys.argv with
        | [|_; input; output|] -> 
                Reverse.reverse input output;
        | _ -> print_string "Usage: back <input> <output>";
            print_newline ();
    end
with
    e -> print_string "An error occurred : ";
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1;;