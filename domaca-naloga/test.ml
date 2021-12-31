let f l =
    match l with
    | m :: [] -> print_int m 
    | m :: ms -> print_int (-1)
    | [] -> print_int 0