(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese matrix =
  let rows = Array.length matrix in
  let cols = Array.length (matrix.(0)) in 
  
  for i = 1 to rows do
    for j = 1 to cols do 
      let bottom_sum = ref 0 in
      let right_sum = ref 0 in
      if (rows-i) < (rows-1) then bottom_sum := matrix.(rows-i+1).(cols-j); 
      if (cols-j) < (cols-1) then right_sum := matrix.(rows-i).(cols-j+1); 
      matrix.(rows-i).(cols-j) <- (max !bottom_sum !right_sum) + matrix.(rows-i).(cols-j)
    done;
  done;
  matrix.(0).(0)

 (*----------------------------------------------------------------------------*]
  Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
  jo ubere, da bo prišla do ustrezne pojedine.
 
  Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
  čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
  Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
  na poti.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # optimal_path_bottom test_matrix;;
  - : mouse_direction list = [Right; Down; Down; Right; Down]
  # optimal_path test_matrix |> convert_path test_matrix;;
  - : int list = [1; 2; 4; 5; 1]
 [*----------------------------------------------------------------------------*)
 
type mouse_direction = Down | Right | Stay
 
let max_cheese_rec matrix =
  let rows = Array.length matrix in
  let cols = Array.length (matrix.(0)) in 
  
  let rec optimal row col =
    if row = rows-1 && col = cols-1 then matrix.(row).(col) else
    if row >= rows || col >= cols then (-1) else
      let bottom_sum = ref 0 in
      let right_sum = ref 0 in
      if row < rows then bottom_sum := optimal (row+1) col;
      if col < cols then right_sum := optimal row (col+1);
      (+) (max !bottom_sum !right_sum) matrix.(row).(col)
  in
  
  optimal 0 0

    
let optimal_path_bottom matrix =
  let rows = Array.length matrix in
  let cols = Array.length (matrix.(0)) in 
  
  let rec optimal row col =
    if row = rows-1 && col = cols-1 then (matrix.(row).(col), Stay :: []) else
    if row >= rows || col >= cols then (-1, Stay :: []) else
      let bottom_sum = optimal (row+1) col in
      let right_sum = optimal row (col+1) in
      if fst bottom_sum > fst right_sum then
        (fst bottom_sum |> (+) matrix.(row).(col), Down :: (snd bottom_sum))
      else
        (fst right_sum |> (+) matrix.(row).(col), Right :: (snd right_sum))
        
  in
  
  optimal 0 0

 (*----------------------------------------------------------------------------*]
  Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
  različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
  3, rdeči pa višin 1 in 2.
 
  Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
  dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
  v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
  poljubne barve.
 
  Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # alternating_towers 10;;
  - : int = 35
 [*----------------------------------------------------------------------------*) 

let alternating_towers n =
  let rec number_red n =
    if n = 0 || n = 1 || n = 2 then 1 else
      number_blue (n-1) |> (+) (number_blue (n-2))
        
  and number_blue n = 
    if n = 3 then 2 else
    if n = 1 then 0 else
    if n = 2 || n = 0 then 1 else
      number_red (n-3) |> (+) (number_red (n-2))
  in
  number_red n |> (+) (number_blue n)
 
 (*----------------------------------------------------------------------------*]
  Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
  konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
  kar kot vsotne tipe. 
 
Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
  barve in preostanek, ki je stolp nasprotne barve.
 
Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
brez) prekoračitve sklada deluje vsaj do višine 20.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# enumerate_towers 4;;
- : tower list = 
[Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
 Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
 Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
 Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
 Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
  [*----------------------------------------------------------------------------*)
 
 
type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1
 
type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom
 
type tower = Red of red_tower | Blue of blue_tower 

 (*----------------------------------------------------------------------------*]
  Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
  drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
  hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
  pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
  ki ju pojeste vsaj `k`.
 
  Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
  predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
  najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
  Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
  ko v optimalni požrtiji pojemo `i`-to slaščico.
 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # ham_ham test_shelf 1;;
  - : bool list = [false; true; false; true; false; true; false; true; false]
  # ham_ham test_shelf 2;;
  - : bool list = [false; true; false; false; false; true; false; false; false]
 [*----------------------------------------------------------------------------*)
 
let test_shelf = [1;2;-5;3;7;19;-30;1;0]
 
