(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = float;;

type dollar = float;;

let euro_to_dollar (x : euro) =
	x *. 1.13;;

let dollar_to_euro (x : dollar) = 
	x *. 0.89;;


(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency =
	| Yen of float
    | Pound of float
    | SEK of float 
    | CHF of float;;

let to_pound (x : currency) = 
	match x with
    | Yen x -> x *. 0.0065
    | Pound x -> x
    | SEK x -> x *. 0.083 
    | CHF x -> x *. 0.8;;

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list =
	| Prazen
    | Int of int * intbool_list
    | Bool of bool * intbool_list;;

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = 
	match ib_list with
     | Prazen -> Prazen
     | Int (x, xs) -> Int (f_int x, intbool_map f_int f_bool xs)
     | Bool (x, xs) -> Bool (f_bool x, intbool_map f_int f_bool xs);;

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse (list : intbool_list) = 
	let rec f (alist : intbool_list) (acc : intbool_list) = 
    	match alist with
         | Prazen -> acc
         | Int (x, xs) -> f xs (Int (x, acc))
         | Bool (x, xs) -> f xs (Bool (x, acc))
	in
    f list Prazen;;

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate list =
	let rec f (alist : intbool_list) (int_acc : int list) (bool_acc : bool list) =
    	match alist with
        | Prazen -> (int_acc, bool_acc)
        | Int (x, xs) -> f xs (x :: int_acc) (bool_acc)
        | Bool (x, xs) -> f xs (int_acc) (x :: bool_acc)
	in
    f list [] [];;

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =
	| Fire
    | Frost
    | Arcane;;
    
type specialization =
	| Historian
    | Teacher
    | Researcher;;

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status =
	| Newbie
    | Student of magic * int
    | Employed of magic * specialization;;
    
type wizard = {
	name : string;
    status : status;
};;

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {
	fire : int;
    frost : int;
    arcane : int;
};;

let update (m : magic_counter) (t : magic) =
	match t with
    | Fire -> {fire=(m.fire + 1); frost=m.frost; arcane=m.arcane}
    | Frost -> {fire=m.fire; frost=(m.frost + 1); arcane=m.arcane}
	| Arcane -> {fire=m.fire; frost=m.frost; arcane=(m.arcane + 1)};;
    

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic (w : wizard list) =
    let rec f (list : wizard list) (m_count : magic_counter) =
        match list with
        | [] -> m_count
        | wizard :: xs ->
            match wizard.status with
                | Newbie -> f xs m_count
                | Student(m, i) -> f xs (update m_count m)
                | Employed(m, sp) -> f xs (update m_count m)
	in
    f w {fire=0; frost=0; arcane=0};;

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let find_candidate (m : magic) (spec : specialization) (wizard_list : wizard list) = 
    let years_needed (s : specialization) =
        match s with
        | Historian -> 3
        | Teacher ->  5
        | Researcher -> 4
    in
	let rec f (magic_type : magic) (spec : specialization) (wizard_list : wizard list) =
        match wizard_list with
        | [] -> None
        | wizard :: xs -> 
        	match wizard.status with
            | Employed(magic_type, spec) -> Some wizard.name
            | Student(magic_type, years) when years >= years_needed spec -> Some wizard.name
            | _ -> f magic_type spec xs
	in
    f m spec wizard_list;;
