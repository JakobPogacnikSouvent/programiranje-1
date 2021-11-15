(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse list = 
  match list with
  | [] -> []
  | head :: tail -> reverse tail @ head :: []

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
- : string list = ["A"; "A"; "A"; "A"; "A"]
                  # repeat "A" (-2);;
- : string list = []
[*----------------------------------------------------------------------------*)

let repeat x = 
  let rec afunction n =
    match n with
    | n when n <= 0 -> []
    | n when n > 0 -> x :: afunction (n-1) 
  in
  afunction

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
Funkcija je repno rekurzivna.
                    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    # range 10;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  [*----------------------------------------------------------------------------*)

let rec range n = 
  let rec afun n acc =
    match n with
    | n when n < 0 -> [] @ acc
    | n when n >= 0 -> afun (n-1) (n :: acc)
  in
  afun n []
        

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
  [f x0; f x1; f x2; ...].
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # let plus_two = (+) 2 in
      map plus_two [0; 1; 2; 3; 4];;
- : int list = [2; 3; 4; 5; 6]
  [*----------------------------------------------------------------------------*)

let rec map f list = 
  match list with
  | [] -> []
  | head :: tail -> f head :: (map f tail)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# let plus_two = (fun x -> x + 2) in
  map_tlrec plus_two [0; 1; 2; 3; 4];;
- : int list = [2; 3; 4; 5; 6]
  [*----------------------------------------------------------------------------*)

let rec map_tlrec f = 
  let rec afun acc list =
    match list with
    | [] -> acc
    | head :: tail -> afun (acc @ (f head) :: []) tail
  in
  afun []

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

def mapi(f, list):
  mapi_list = []
  index = 0
  for x in list:
    mapi_list += [f(x, index)]
    index += 1
  return mapi_list

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# mapi (+) [0; 0; 0; 2; 2; 2];;
- : int list = [0; 1; 2; 5; 6; 7]
  [*----------------------------------------------------------------------------*)

let rec mapi f = 
  let rec afun acc index list =
    match list with
    | [] -> acc
    | head :: tail -> afun (acc @ (f head index) :: []) (index+1) tail
  in
  afun [] 0

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# zip [1; 1; 1; 1] [0; 1; 2; 3];;
- : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
# zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
Exception: Failure "Different lengths of input lists.".
                     [*----------------------------------------------------------------------------*)

let rec zip l1 l2 = 
  let rec afun l1 l2 acc =
    match (l1, l2) with
    | ([], []) -> acc
    | ([], h :: t) | (h :: t, []) -> failwith "Different lengths of input lists."
    | (head :: tail, head' :: tail') -> afun tail tail' (acc @ (head, head') :: [])
  in
  afun l1 l2 []

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# unzip [(0,"a"); (1,"b"); (2,"c")];;
- : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
  [*----------------------------------------------------------------------------*)

let unzip list =
  let rec afun acc acc' list = 
    match list with
    | [] -> (acc, acc')
    | head :: tail -> afun (acc @ (fst head) :: []) (acc' @ (snd head) :: []) tail
  in
  afun [] [] list

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
- : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
  [*----------------------------------------------------------------------------*)

let rec unzip_tlrec list = unzip list

(*----------------------------------------------------------------------------*]
Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
  while condition(x):
          x = f(x)
            return x

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# loop (fun x -> x < 10) ((+) 4) 4;;
- : int = 12
  [*----------------------------------------------------------------------------*)

let rec loop condition f x =
  match (condition x) with
  | true -> loop condition f (f x)
  | false -> x

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
f(... (f (f x0 x1) x2) ... xn).
   V primeru seznama z manj kot dvema elementoma vrne napako.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
- : string = "FICUS"
  [*----------------------------------------------------------------------------*)

let fold_left_no_acc f list =
  if List.length list < 2 then failwith "List length too short" else
    
    let rec afun acc =
      match acc with
      | head :: [] -> head
      | fst :: snd :: tail -> afun ((f fst snd) :: tail)
    in
    
    afun list
  

(*----------------------------------------------------------------------------*]
Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
  vrednosti [x] do vključno [n]-te uporabe, torej
                     [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
                       Funkcija je repno rekurzivna.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# apply_sequence (fun x -> x * x) 2 5;;
- : int list = [2; 4; 16; 256; 65536; 4294967296]
# apply_sequence (fun x -> x * x) 2 (-5);;
- : int list = []
  [*----------------------------------------------------------------------------*)

let rec apply_sequence f x n = 
  let rec afun acc to_append n =
    match n with
    | n when n < 0 -> acc
    | n when n >= 0 -> afun (acc @ to_append :: []) (f to_append) (n-1)
  in
  afun [] x n

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let filter f = 
  let rec afun acc list = 
    match list with
    | [] -> acc
    | head :: tail -> if f head then afun (acc @ head :: []) tail else afun acc tail
  in
  afun []


(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let exists f = 
  let rec afun alist =
    match alist with
    | [] -> false
    | head :: tail -> if f head then true else afun tail
  in
  afun

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let first f default = 
  let rec afun alist =
    match alist with
    | [] -> default
    | head :: tail -> if f head then head else afun tail
  in
  afun
