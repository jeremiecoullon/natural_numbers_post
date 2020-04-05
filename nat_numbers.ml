open Printf

type nat =
  | Zero
  | Succ of nat

let decr = function
  | Zero -> Zero
  | Succ mm -> mm

let rec (++) n m =
  match m with
  | Zero -> n
  | Succ mm -> (Succ n) ++ mm

let rec (--) n m =
  match m with
  | Zero -> n
  | Succ mm -> (decr n) -- mm

let (+*) n m =
  let rec aux n m acc =
    match m with
    | Zero -> acc
    | Succ mm -> aux n mm (n ++ acc)
  in
  aux n m Zero

let rec (<<) n m =
  match (n, m) with
  | (p, Zero) -> false
  | (Zero, q) -> true
  | (p, q) -> (decr n) << (decr m)

let (//) n m =
  let rec aux p acc =
    let lt = p << m in
    match lt with
    | true -> acc
    | false -> aux (p -- m) (Succ acc)
  in
  aux n Zero

let (%) n m =
  let p = n // m in
  n -- (p +* m)


(*
  ============
  To print out natural numbers
  ============
 *)

let print_nat_digits = function
  | Zero -> "0"
  | Succ Zero -> "1"
  | Succ Succ Zero -> "2"
  | Succ Succ Succ Zero -> "3"
  | Succ Succ Succ Succ Zero -> "4"
  | Succ Succ Succ Succ Succ Zero -> "5"
  | Succ Succ Succ Succ Succ Succ Zero -> "6"
  | Succ Succ Succ Succ Succ Succ Succ Zero -> "7"
  | Succ Succ Succ Succ Succ Succ Succ Succ Zero -> "8"
  | Succ Succ Succ Succ Succ Succ Succ Succ Succ Zero -> "9"
  | _ -> "bigger than 9"

let rec string_of_nat_raw n =
  match n with
  | Zero -> "Zero"
  | Succ a -> String.concat " " ["Succ"; string_of_nat_raw a]

(*
  ============
  Test operators
  ============
 *)
let () = printf "Test operators:\n"
let zero = Zero
let one = Succ Zero
let two = Succ (Succ Zero)
let three = (Succ (Succ (Succ Zero)))
let four = Succ three
let five = Succ four
let six = Succ five
let seven = Succ (six)
let nine = Succ (Succ seven)
let ten = Succ nine
let eleven = Succ ten

let () = printf "%s\n" (string_of_nat_raw (two ++ three ))
let () = printf "%s\n" (string_of_nat_raw (seven -- four) )
let () = printf "%s\n\n" (string_of_nat_raw (ten // three) )
(* ========= *)


(* ========= *)

(* Convert to base 10: a list of natural numbers between 0 and 9 *)
let base10 n =
  let rec aux q acc =
    let r = q % ten in
    let p = q // ten in
    match p with
    | Zero -> r::acc
    | pp -> aux p (r::acc)
  in
  aux n []

(* convert natural numbers to string *)
let string_of_nat n =
  let base_10_rep = base10 n in
  let list_strings = List.map print_nat_digits base_10_rep in
  String.concat "" list_strings


(* To go from from base 10 (list of natural numbers) to a single natural number *)
let nat_of_listnat l =
  let lr = List.rev l in
  let rec aux n b lr =
    match lr with
    | [] -> n
    | h::t -> aux (n ++ (b+*h)) (b+*ten) t
  in
  aux Zero (Succ Zero) lr


let string_to_list s =
  let rec loop acc i =
    if i = -1 then acc
    else
      loop ((String.make 1 s.[i]) :: acc) (pred i)
  in loop [] (String.length s - 1)


let nat_of_string_digits = function
  | "0" -> Zero
  | "1" -> Succ Zero
  | "2" -> Succ (Succ Zero)
  | "3" -> Succ (Succ (Succ Zero))
  | "4" -> Succ (Succ (Succ (Succ Zero)))
  | "5" -> Succ (Succ (Succ (Succ (Succ Zero))))
  | "6" -> Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
  | "7" -> Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
  | "8" -> Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
  | "9" -> Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
  | _ -> raise (Failure "string must be less than 10")


(* Converts string to nat *)
let nat_of_string s =
  let liststring = string_to_list s in
  let listNatbase = List.map nat_of_string_digits liststring in
  nat_of_listnat listNatbase


(*
  ============
  Convert from string representation of numbers to natural numbers, do operations, then convert back
  Note: struggles with large numbers (past 7ish digits)
  ============
 *)

let (+++) n m =
 string_of_nat ((nat_of_string n) ++ (nat_of_string m))

let (---) n m =
 string_of_nat ((nat_of_string n) -- (nat_of_string m))

let (+**) n m =
 string_of_nat ((nat_of_string n) +* (nat_of_string m))

let (///) n m =
 string_of_nat ((nat_of_string n) // (nat_of_string m))

let (%%) n m =
string_of_nat ((nat_of_string n) % (nat_of_string m))


let () = printf "Using string representation:\n"
let () = printf "%s\n" ("3" +++ "17")
let () = printf "%s\n" ("182" --- "93")
let () = printf "%s\n" ("12" +** "3")
let () = printf "%s\n" ("41" /// "3")
let () = printf "%s\n" ("41" %% "3")
