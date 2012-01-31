(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*)

let amount = function
  | n when n mod 3 == 0 -> n
  | n when n mod 5 == 0 -> n
  | _ -> 0

let rec sum current top accum = 
    match current == top with
      true -> accum
      | false -> sum (current + 1) top (accum + (amount current))

let problem1 top =
  sum 1 top 0;;
