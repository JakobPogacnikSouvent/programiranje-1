let string_of_int_list (l : int list) =
  let rec f acc =
    function
    | [] -> ()
    | x :: [] -> print_int x;
    | x :: xs -> 
in

  print_string "[";
  f l;
  print_string "]"