(*
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
 *)

let even_filter n=
  match n mod 2 == 0 with
  true -> n
  | false -> 0

let rec sum current top prev accum =
  match current > top with
  true -> accum
  | false -> sum (current + prev) top current (accum + (even_filter current))

let problem2 top =
  sum 2 top 1 0;;