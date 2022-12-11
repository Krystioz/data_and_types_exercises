type student = { first_name : string; last_name : string; gpa : float }

let krystek = { first_name = "krystek"; last_name = "gluszko"; gpa = 2. }
let get_name = function { first_name = name; _ } -> name
let cr_std first last sc = { first_name = first; last_name = last; gpa = sc }

(*
   Exercise: pokerecord [★★]

   Here is a variant that represents a few Pokémon types:

   type poketype = Normal | Fire | Water
   Define the type pokemon to be a record with fields name (a string), hp (an integer), and ptype (a poketype).

   Create a record named charizard of type pokemon that represents a Pokémon with 78 HP and Fire type.

   Create a record named squirtle of type pokemon that represents a Pokémon with 44 HP and Water type.
*)

type poketype = Normal | Fire | Water
type poke = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }
let safe_hd = function [] -> None | hd :: _ -> Some hd
let safe_tl = function [] -> None | _ :: tl -> Some tl

(*
   Exercise: pokefun [★★★]

   Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP.
*)

let rec max_hp = function
  | [] -> None
  | poke1 :: t -> (
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> if poke1.hp >= poke2.hp then Some poke1 else Some poke2)

(*
         Exercise: date before [★★]

   Define a date-like triple to be a value of type int * int * int. Examples of date-like triples include (2013, 2, 1) and (0, 0, 1000). A date is a date-like triple whose first part is a positive year (i.e., a year in the common era), second part is a month between 1 and 12, and third part is a day between 1 and 31 (or 30, 29, or 28, depending on the month and year). (2013, 2, 1) is a date; (0, 0, 1000) is not.

   Write a function is_before that takes two dates as input and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.)

   Your function needs to work correctly only for dates, not for arbitrary date-like triples. However, you will probably find it easier to write your solution if you think about making it work for arbitrary date-like triples. For example, it’s easier to forget about whether the input is truly a date, and simply write a function that claims (for example) that January 100, 2013 comes before February 34, 2013—because any date in January comes before any date in February, but a function that says that January 100, 2013 comes after February 34, 2013 is also valid. You may ignore leap years.
*)

type date = int * int * int

(* year month day *)

let check_date = function
  | a, b, c ->
      if a < 9999 && a > 1 && b > 0 && b < 12 && c > 0 && c < 30 then true
      else false
;;

check_date (1, 2, 3)


