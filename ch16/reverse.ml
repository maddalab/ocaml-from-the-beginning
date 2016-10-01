(*
Takes 2 filenames, input and output, reverses all lines in input file
to output file

compile: ocamlc/opt reverse.mli reverse.ml back.ml -o back

*)

let rev_string str =
  let len = String.length str in
  let res = Bytes.create len in
  let last = len - 1 in
  for i = 0 to last do
    let j = last - i in
    Bytes.set res i str.[j];
  done;
  (res)

let reverse input output =
    let ic = open_in input and
    oc = open_out output in
        try
            while true do
                let line = input_line ic in
                    Printf.fprintf oc "%s\n" (rev_string line);
            done;
        with
            _ -> close_in ic; close_out oc;;
