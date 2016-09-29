let make_vector (x0, y0) (x1, y1) =
    (x1 -. x0, y1 -. y0);;

let vector_length (x, y) =
    sqrt (x *. x +. y *. y);;

let offset_point (x, y) (px, py) =
    (x +. px, y +. py);;

let scale_to_length l (x, y) =
    let currentlength = vector_length (x, y) in
        if currentlength = 0. then (x, y) else
            let factor = l /. currentlength in
                (x *. factor, y *. factor);;

let round n =
    let fl  = floor n in
        let d = n -. fl in
            if d >= 0.5 then
                fl +. 1.
            else
                fl;;

let equi_distant (x0, y0) (x1, y1) =
    ((x0 +. x1) /. 2., (y0 +. y1) /. 2.);;

let split n =
    let fl = floor n in
        (fl, n -. fl);;

let star f =
    let col = int_of_float (f *. 50.) in
        for x = 1 to col - 1 do
            print_string " ";
        done;
        print_string "*";
        print_newline ();;

let plot fn st en step =
    let rst = ref st in
        while !rst < en do
            star (fn !rst);
            rst := !rst +. step;
        done;;

let pi = 22. /. 7. in
    plot sin 0. pi (pi /. 20.);;

let pi = 4. *. atan 1. in
    plot sin 0. pi (pi /. 20.);;

