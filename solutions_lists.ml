let x = 12 in
x * x

(* Exercise: list expressions [★]

   Construct a list that has the integers 1 through 5 in it. Use the square bracket notation for lists.

   Construct the same list, but do not use the square bracket notation. Instead use :: and [].

   Construct the same list again. This time, the following expression must appear in your answer: [2; 3; 4]. Use the @ operator, and do not use ::.
*)

let lst = [ 1; 2; 3; 4; 5 ]

(* let lst_x = 1 :: 2 :: 3 :: 4 :: 5 :: []; *)

(* Exercise: product [★★]

   Write a function that returns the product of all the elements in a list. The product of all the elements of an empty list is 1. *)

let rec product = function [] -> 0 | h :: t -> h + product t

(* Exercise: concat [★★] Write a function that concatenates all the strings in a list. The concatenation of all the strings in an empty list is the empty string "". *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(* Exercise: product test [★★]

   Unit test the function product that you wrote in an exercise above. *)

(* solution located in file tests.ml *)

(*
   Using pattern matching, write three functions, one for each of the following properties. Your functions should return true if the input list has the property and false otherwise.

      the list’s first element is "bigred"

      the list has exactly two or four elements; do not use the length function

      the first two elements of the list are equal
*)

let rec member el lst =
  match lst with [] -> false | h :: t -> if h = el then true else member el t

let two_or_four = function
  | [] -> false
  | [ _; _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let first_two_equal = function
  | [] -> false
  | _ :: [] -> false
  | x :: y :: _ -> if x = y then true else false

(*
     Exercise: library [★★★]

   Consult the List standard library to solve these exercises:

   Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the list has fewer than five elements, return 0. Hint: List.length and List.nth.

   Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with Stdlib.compare as its first argument, and List.rev.
*)

let fifth lst =
  match lst with
  | [] -> 0
  | _ :: _ -> if List.length lst < 5 then 0 else List.nth lst 4

let sort_desc lst = function
  | [] -> []
  | _ :: [] -> lst
  | _ :: _ -> List.sort Stdlib.compare lst |> List.rev

(*
     Exercise: library puzzle [★★★]

   Write a function that returns the last element of a list. Your function may assume that the list is non-empty. Hint: Use two library functions, and do not write any pattern matching code of your own.

   Write a function any_zeroes : int list -> bool that returns true if and only if the input list contains at least one 0. Hint: use one library function, and do not write any pattern matching code of your own.

   Your solutions will be only one or two lines of code each.
*)

let last lst =
  let len = List.length lst in
  List.nth lst @@ (len - 1)

let inc_zeros lst = List.mem 0 lst

(*
   Exercise: take drop [★★★]

   Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst. If lst has fewer than n elements, return all of them.

   Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst. If lst has fewer than n elements, return the empty list.
*)

let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

let long_list = 0 -- 1_000_000

let rec take n lst =
  if n <= 0 then []
  else if n > List.length lst then lst
  else match lst with [] -> [] | hd :: tl -> hd :: take (n - 1) tl

(* natural implementation to drop is already tail recursive *)

let rec drop n lst =
  if n <= 0 then lst
  else if n > List.length lst then []
  else match lst with [] -> [] | _ :: tl -> drop (n - 1) tl

(* here is an implementation of take tail function in one function (just like the examples in sicp) *)

let take_tr n lst =
  let rec take_tail n xs acc =
    if n = 0 then acc
    else match xs with [] -> acc | hd :: tl -> take_tail (n - 1) tl (hd :: acc)
  in
  take_tail n lst [] |> List.rev

(********************************************************************
  * exercise: unimodal
  ********************************************************************)

let rec is_mon_dec = function
  | [] | [ _ ] -> true
  | h1 :: (h2 :: _ as t) -> h1 >= h2 && is_mon_dec t

(** returns: whether the input list is monotonically increasing
    then monotonically decreasing *)
let rec is_mon_inc_then_dec = function
  | [] | [ _ ] -> true
  | h1 :: (h2 :: _ as t) as lst ->
      if h1 <= h2 then is_mon_inc_then_dec t else is_mon_dec lst

let is_unimodal lst = is_mon_inc_then_dec lst

(*
   Exercise: powerset [★★★]

   Write a function powerset : int list -> int list list that takes a set S represented as a list and returns the set of all subsets of S. The order of subsets in the powerset and the order of elements in the subsets do not matter.

   Hint: Consider the recursive structure of this problem. Suppose you already have p, such that p = powerset s. How could you use p to compute powerset (x :: s)?
*)

let rec powerset lst =
  match lst with
  | [] -> [ [] ]
  | x :: xs ->
      let subsets = powerset xs in
      subsets @ List.map (fun subset -> x :: subset) subsets

(* print list recursive*)

let rec print_int_list = function
  | [] -> ()
  | hd :: tl ->
      string_of_int hd |> print_endline;
      print_int_list tl

(*
         Exercise: print int list iter [★★]

   Write a function print_int_list' : int list -> unit whose specification is the same as print_int_list. Do not use the keyword rec in your solution, but instead to use the List module function List.iter.
*)

let print_int_list_iter lst =
  List.iter (fun x -> string_of_int x |> print_endline) lst

(* Find the longest string in a list *)

let rec longest_string arr =
  match arr with
  | [] -> ""
  | x :: xs ->
      let longest_rest = longest_string xs in
      if String.length x > String.length longest_rest then x else longest_rest
