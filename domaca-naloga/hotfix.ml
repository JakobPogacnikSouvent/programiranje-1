(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind) |> Array.to_list

let rows grid = grid |> Array.map Array.to_list |> Array.to_list

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind)) |> Array.to_list

let columns grid = List.init 9 (get_column grid)

let rec n_th_item n =
    function
    | [] -> failwith "Index out of range in n_th_item"
    | x :: xs ->
        match n with
        | y when y <= 0 -> x
        | y -> n_th_item (n-1) xs

let boxes (grid : 'a grid) : 'a list list =
  grid |> rows |> List.map (chunkify 3) |> chunkify 3
  |> List.map (function l -> [
      l |> List.map (n_th_item 0) |> List.fold_left (@) [];
      l |> List.map (n_th_item 1) |> List.fold_left (@) [];
      l |> List.map (n_th_item 2) |> List.fold_left (@) []
      ])
  |> List.fold_left (@) []

let get_box (grid : 'a grid) (box_ind : int) = (boxes grid) |> n_th_item box_ind

let get_box_of_coords (row, col) =
  let row_i = row / 3 and col_i = col / 3 in
  (row_i * 3 + col_i)

let get_box_of_field grid (row, col) =
  let b = get_box_of_coords (row, col) in
  get_box grid b

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid =
  Array.map (function l -> Array.map f l) grid

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit =
    let string_of_cell =
        function
        | None -> " "
        | Some x -> string_of_int x
    in
    print_grid string_of_cell (problem.initial_grid)

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_problem {initial_grid = (map_grid (function x -> Some x) solution)}

let no_duplicates_in_list (l : int list) =
  (* Checks if list contains all digits from 1 to length of list *)
  let seen = Array.make (List.length l) false in
  let rec f =
    function
    | [] -> true
    | x :: xs ->
      if seen.(x-1) then 
        false
      else
        let _ = seen.(x-1) <- true in
        f xs
  in
  f l

let no_duplicates_in_option_list (l : int option list) =
  let seen = Array.make (List.length l) false in
  let rec f =
    function
    | [] -> true
    | x :: xs ->
      match x with
      | None -> f xs
      | Some y ->
        if seen.(y - 1) then 
          false
        else
          let _ = seen.(y - 1) <- true in
          f xs
  in
  f l

let is_valid_solution problem solution =
  (* Check regular sudoku rules *)
  (columns solution) @ (rows solution) @ (boxes solution) |> List.filter no_duplicates_in_list |> List.length |> (=) 27

type available = { loc : int * int; possible : int list }

let print_int_list (l : int list) =
  let rec f =
    function
    | [] -> ()
    | x :: [] -> print_int x;
    | x :: xs -> 
        print_int x;
        print_string ";";
        f xs
  in
  print_string "[";
  f l;
  print_string "]"

let print_available (x : available) =
  Printf.printf "loc=(%d, %d), possible=" (fst x.loc) (snd x.loc);
  print_int_list x.possible;
  print_string "\n"

let print_available_list (l : available list) =
  let rec f =
    function
    | [] -> ()
    | x :: [] -> print_available x;
    | x :: xs -> 
        print_available x;
        print_string ";";
        f xs
  in
  print_string "[";
  f l;
  print_string "]"

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : problem; current_grid : int option grid; current_available : available list; last_move : int option * int option}

let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state

let remove_available (n : int) ((row, col) : int * int) (current_available : available list) =
  (* Give digit and coords and the function will remove invalid occurences of it from current_available digits *)

  let remove (m : int) (l : int list) =
  (* Removes m from list *)
  List.filter (function x -> x<>m) l
  in

  let rec f acc =
  function
  | [] -> acc
  | x :: xs -> (
    match x.loc with
    (* If we are in the same row and col remove all available *)
    | (a, b) when a = row && b = col -> f acc xs
    (* If in same row, collumn or box remove digit from available *)
    | (a, b) when a = row || b = col || (get_box_of_coords (a,b)) = (get_box_of_coords (row, col)) -> f ({loc=x.loc; possible=remove n x.possible} :: acc) xs
    (* Else pass *)
    | _ -> f (x :: acc) xs
  )
  in
  f [] current_available


let initialize_state (problem : problem) : state =
  let grid_copy = copy_grid problem.initial_grid in
  let to_check = ref [] in
  let filled_in = ref [] in
  for row = 0 to 8 do
    for col = 0 to 8 do
      let n = grid_copy.(row).(col) in
      if n = None then
        to_check := {loc=(row, col); possible=[1;2;3;4;5;6;7;8;9]} :: !to_check
      else
        filled_in := ((Option.get n), (row, col)) :: !filled_in
    done;
  done;
  let rec f available = 
  function
  | [] -> available
  | x :: xs -> f (remove_available (fst x) (snd x) available) xs
  in
  {current_grid = grid_copy; problem; current_available = f !to_check !filled_in; last_move=(None, None)}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

let is_valid_last_move (state : state) : bool =
  (* print_state state; *)
  match state.last_move with
  | (None, None) -> true (* If there was no previus move the move is valid *)
  | (Some row, Some col) ->
    get_row state.current_grid row |> no_duplicates_in_option_list && get_column state.current_grid col |> no_duplicates_in_option_list && get_box_of_field state.current_grid (row, col) |> no_duplicates_in_option_list 
  | _ -> failwith "Invalid state"

let remove_available_from_last_move (state : state) : state =
  match state.last_move with
  | (None, None) -> state
  | (Some row, Some col) ->
    let n = state.current_grid.(row).(col) |> Option.get in
    let available' = remove_available n (row, col) state.current_available in
    {problem = state.problem; current_grid = state.current_grid; current_available = available'; last_move=(Some row, Some col)}
  | _ -> failwith "Invalid state"

let branch_state' (state : state) : (state * state) option =
  let min_and_rest (l : available list) : (available * available list) =
    (* Finds square with least possible branching options *)
    let rec f min rest =
    function
    | [] -> (min, rest)
    | x :: xs ->
      if (List.length x.possible) < (List.length min.possible) then
        f x (min :: rest) xs
      else
        f min (x :: rest) xs
    in
    f (List.hd l) [] (List.tl l)
  in
  (* If there are no more squares to check we cannot branch *)
  if state.current_available = [] then
    None
  else
    let best_square, rest = min_and_rest state.current_available in

    match best_square.possible with
    | [] -> failwith "No more branching possibilities in branch_state."
    | n :: ns ->(
      let row, col = best_square.loc in
      let first_state_grid = copy_grid state.current_grid in
      first_state_grid.(row).(col) <- Some n;
      let first_state = {problem = state.problem; current_grid = first_state_grid; current_available = rest; last_move=(Some row, Some col)} in
      match ns with
      (* If there is only 1 possible digit in square after trying first the second branch can be initialized with said digit in square *)
       | m :: [] ->
        let second_state_grid = copy_grid state.current_grid in
        second_state_grid.(row).(col) <- Some m;
        let second_state = {problem = state.problem; current_grid = second_state_grid; current_available = rest; last_move=(Some row, Some col)} in
        Some (first_state, second_state)
      (* If there are more than 1 possible digits in square after trying first remove first digit from possible digits in second branch *)
       | m :: ms -> 
        let second_state = {problem = state.problem; current_grid = copy_grid state.current_grid; current_available = {loc = best_square.loc; possible = (m :: ms)} :: rest; last_move=(None, None)} in
        Some (first_state, second_state)
       | [] -> failwith "Shouldn't reach this possibility as we find naked singles before branching"
      )

let branch_state (state : state) : (state * state) option =
  let min_and_rest (l : available list) : (available * available list) =
    (* Finds square with least possible branching options *)
    let rec f min rest =
    function
    | [] -> (min, rest)
    | x :: xs ->
      if (List.length x.possible) < (List.length min.possible) then
        f x (min :: rest) xs
      else
        f min (x :: rest) xs
    in
    f (List.hd l) [] (List.tl l)
  in


  if state.current_available = [] then
    (* If there are no more squares to check we cannot branch *)
    None
  else
    let x = List.hd state.current_available in
    let xs = List.tl state.current_available in

    let best_square, rest = min_and_rest state.current_available in
    print_string "~~~~~~~~~~~\n";
    print_available x;
    print_available_list xs;
    print_string "------------\n";
    print_available best_square;
    print_available_list rest;
    print_string "~~~~~~~~~~~\n";
    
    match x.possible with
    | n :: ns ->(
      let row, col = x.loc in
      let first_state_grid = copy_grid state.current_grid in
      first_state_grid.(row).(col) <- Some n;
      let first_state = {problem = state.problem; current_grid = first_state_grid; current_available = xs; last_move=(Some row, Some col)} in
      match ns with
       | m :: [] ->
        let second_state_grid = copy_grid state.current_grid in
        second_state_grid.(row).(col) <- Some m;
        let second_state = {problem = state.problem; current_grid = second_state_grid; current_available = xs; last_move=(Some row, Some col)} in
        Some (first_state, second_state)
       | m :: ms -> 
        let second_state = {problem = state.problem; current_grid = copy_grid state.current_grid; current_available = {loc = x.loc; possible = (m :: ms)} :: xs; last_move=(None, None)} in
        Some (first_state, second_state)
       | [] -> failwith "Shouldn't reach this possibility as we delete lists when they have 1 elt left"
      )
    | [] -> failwith "Reached invalid state of possibilities in branch_state."

let naked_singles (state : state) : response option =
  (* Returns None if there are no naked singles and new state if there are naked singles *)
  let rec f acc =
  function
  | [] -> None
  | square :: rest ->
    match square.possible with
    | [] -> Some (Fail state)
    | n :: [] -> (
      let row, col = square.loc in
      state.current_grid.(row).(col) <- Some n;
      Some (Unsolved {problem = state.problem; current_grid = state.current_grid; current_available = (acc @ rest); last_move=(Some row, Some col)})
      )
    | _ -> f (square :: acc) rest
  in
  f [] state.current_available

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  if is_valid_last_move state then
    match validate_state state with
        | Solved solution ->
            (* če smo našli rešitev, končamo *)
            Some solution
        | Fail fail ->
            (* prav tako končamo, če smo odkrili, da rešitev ni *)
            None
        | Unsolved state' ->
            (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
            
            (* Zožimo rešitve *)
            let state'' = remove_available_from_last_move state' in
            

            (* Preverimo če obstaja polje s samo eno možnostjo *)
            match naked_singles state'' with
              (* Če da vstavimo številko *)
              | Some (Unsolved state''') -> solve_state state'''
              (* Če najdemo polje brez možnosti končamo *)
              | Some (Fail state''') -> None
              (* Če ne nadaljujemo z branchanjem *)
              | None -> explore_state state
              (* Shouldn't happen *)
              | Some (Solved solution) -> Some solution 
  else
    None

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state

let read_problem filename =
  let channel = open_in filename in
  (* Ne zna brat posebnih *)
  let str = really_input_string channel (320) in
  close_in channel;
  problem_of_string str

let find_solution problem =
  let before = Sys.time () in
  let solution = solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)

let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"

let find_and_display_solution (problem : problem) =
  Printf.printf "Rešujem:\n";
  print_problem problem;
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time

(* let () =
  let before = Sys.time () in
  (* Če se program sesuje, nam to izpiše klicni sklad. *)
  Printexc.record_backtrace true;
  (* Tabela sistemskih argumentov vsebuje ime klicanega programa ter argumente, ki mu sledijo *)
  Sys.argv
  (* Tabelo pretvorimo v seznam *)
  |> Array.to_list
  (* Odstranimo prvi element (ime klicanega programa), da dobimo seznam imen datotek *)
  |> List.tl
  (* Iz vsake datoteke preberemo problem *)
  |> List.map read_problem
  (* Probleme zaporedoma rešimo *)
  |> List.iter find_and_display_solution;
  let after = Sys.time () in
  let elapsed_time = after -. before in
  print_string "\n";
  print_float elapsed_time *)

(* Če domačo nalogo rešujete prek spletnega vmesnika, ki ne podpira branja datotek,
   lahko delovanje preizkušate prek spodnjega programa. *)

let () = "
┏━━━┯━━━┯━━━┓
┃ 1 │5  │2  ┃
┃9  │  1│   ┃
┃  2│  8│ 3 ┃
┠───┼───┼───┨
┃5  │ 3 │  7┃
┃  8│   │5  ┃
┃6  │ 8 │  4┃
┠───┼───┼───┨
┃ 4 │1  │7  ┃
┃   │7  │  6┃
┃  3│  4│ 5 ┃
┗━━━┷━━━┷━━━┛" 
  |> problem_of_string
  |> find_and_display_solution


(* 
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│345│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│564│138┃
┃136│798│245┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛

┏━━━┯━━━┯━━━┓
┃   │   │   ┃
┃   │   │   ┃
┃   │   │   ┃
┠───┼───┼───┨
┃   │   │   ┃
┃   │   │   ┃
┃   │   │   ┃
┠───┼───┼───┨
┃   │   │   ┃
┃   │   │   ┃
┃   │   │   ┃
┗━━━┷━━━┷━━━┛
┏━━━┯━━━┯━━━┓
┃ 1 │5  │2  ┃
┃9  │  1│   ┃
┃  2│  8│ 3 ┃
┠───┼───┼───┨
┃5  │ 3 │  7┃
┃  8│   │5  ┃
┃6  │ 8 │  4┃
┠───┼───┼───┨
┃ 4 │1  │7  ┃
┃   │7  │  6┃
┃  3│  4│ 5 ┃
┗━━━┷━━━┷━━━┛
*)