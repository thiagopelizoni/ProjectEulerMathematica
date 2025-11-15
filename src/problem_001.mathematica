(* Problem: https://projecteuler.net/problem=1 *)
(* Sum of all multiples of 3 or 5 below a given limit. *)

(*
   Problem statement (Project Euler 1):
   If we list all the natural numbers below 10 that are multiples of 3 or 5,
   we get 3, 5, 6 and 9. The sum of these multiples is 23.
   Find the sum of all the multiples of 3 or 5 below 1000.

   Mathematical idea:
   We want the sum of all n with 0 <= n < limit such that n is divisible by 3 or 5.

   Let:
     A = set of multiples of 3 below limit
     B = set of multiples of 5 below limit

   We want the sum over A ∪ B.
   Using the inclusion–exclusion principle for sums:
     sum_{x in A ∪ B} x = sum_{x in A} x + sum_{x in B} x - sum_{x in A ∩ B} x

   Here:
     A ∩ B = set of multiples of lcm(3, 5) = 15 below limit.

   So the final answer is:
     (sum of multiples of 3) + (sum of multiples of 5) - (sum of multiples of 15).

   Each sum of multiples can be expressed as an arithmetic series:

   For a positive integer m and an upper bound "limit":

     Multiples of m below "limit" are:
       m, 2m, 3m, ..., k m

     where k is the largest integer with k m < limit, i.e.
       k = floor((limit - 1) / m).

     Then:
       m + 2m + 3m + ... + k m
       = m (1 + 2 + 3 + ... + k)
       = m * (k (k + 1) / 2)

   This is exactly what sumOfMultiples[m, limit] computes.
*)

sumOfMultiples[m_, limit_] := Module[{k},
  (* k is the number of multiples of m strictly below limit:
     m, 2m, ..., k m with k m < limit.
     Thus k = floor((limit - 1)/m). *)
  k = Floor[(limit - 1)/m];

  (* Sum of the arithmetic series m + 2m + ... + k m
     = m * (1 + 2 + ... + k)
     = m * k * (k + 1) / 2 *)
  m * k * (k + 1) / 2
]

compute[limit_] := Module[{total},
  (* Using inclusion–exclusion on the sets of multiples of 3 and 5:
     - sumOfMultiples[3, limit]  sums all multiples of 3 below limit
     - sumOfMultiples[5, limit]  sums all multiples of 5 below limit
     - sumOfMultiples[15, limit] sums multiples of 15 (common to both),
       which we subtract to avoid double counting. *)
  total = sumOfMultiples[3, limit] +
          sumOfMultiples[5, limit] -
          sumOfMultiples[15, limit];

  (* Final result: sum of all multiples of 3 or 5 below limit. *)
  total
]

(* Example: sum of all multiples of 3 or 5 below 1000.
   The expected answer for Project Euler problem 1 is 233168. *)
compute[1000]
