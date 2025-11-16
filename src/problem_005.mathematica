(* Problem: https://projecteuler.net/problem=5 *)
(* Smallest positive number that is evenly divisible by all numbers from 1 to n. *)

(*
   Problem statement (Project Euler 5, paraphrased):
   What is the smallest positive number that is evenly divisible
   by all of the numbers from 1 to 20?

   In more general terms:
   For a given positive integer n, find the smallest positive integer M
   such that:
       M mod k == 0
   for every integer k with 1 <= k <= n.

   This M is exactly the least common multiple (LCM) of the integers 1, 2, ..., n:
       M = lcm(1, 2, 3, ..., n).

   ---------------------------------------------------------------------------
   Mathematical background: Least Common Multiple (LCM)
   ---------------------------------------------------------------------------

   1) Definition of LCM for two integers
      For two nonzero integers a and b, the least common multiple lcm(a, b) is
      the smallest positive integer that is a multiple of both a and b.
      
      Equivalent characterization:
        - lcm(a, b) is divisible by a and by b;
        - any common multiple of a and b is divisible by lcm(a, b).

   2) Relationship between GCD and LCM
      For nonzero integers a and b, the greatest common divisor gcd(a, b)
      and the least common multiple lcm(a, b) are related by:
      
        |a * b| = gcd(a, b) * lcm(a, b)

      Therefore:
      
        lcm(a, b) = |a * b| / gcd(a, b)

      This formula allows us to compute the LCM efficiently if we can compute
      the GCD.

   3) LCM of more than two integers
      For a finite list of integers a_1, a_2, ..., a_n, the LCM can be built
      iteratively using the binary LCM:
      
        L_1 = a_1
        L_2 = lcm(L_1, a_2)
        L_3 = lcm(L_2, a_3)
        ...
        L_n = lcm(L_{n-1}, a_n)

      Then:
        L_n = lcm(a_1, a_2, ..., a_n).

      In the specific case of Project Euler 5:
        a_k = k, for k = 1, 2, ..., n
      so:
        L_n = lcm(1, 2, ..., n).

      For n = 10, for example:
        lcm(1,2,...,10) = 2520.
      For n = 20 (the problem’s main question), the known result is:
        lcm(1,2,...,20) = 232792560.

   4) Why this LCM is the smallest multiple divisible by all numbers 1..n?
      - By construction, L_n is a multiple of each integer k in [1, n].
      - If there were a smaller positive integer X < L_n that was divisible
        by all k in [1, n], then X would also have to be a common multiple
        of all those integers.
      - But L_n is *the least* common multiple, so no smaller positive
        integer with that property exists.
      - Therefore, L_n is exactly the smallest positive integer divisible
        by all numbers from 1 to n.

   ---------------------------------------------------------------------------
   Algorithm implemented here (matching the provided Python logic)
   ---------------------------------------------------------------------------

   1) Define a function lcm[a, b] using the GCD–LCM relation:
        lcm(a, b) = |a * b| / gcd(a, b).

   2) Define a function smallestMultiple[n] that:
      - starts with an accumulator lcmResult = 1;
      - for each integer i from 1 to n:
            lcmResult = lcm(lcmResult, i);
      - at the end, lcmResult is lcm(1, 2, ..., n).

   3) For the specific case n = 20, the function returns 232792560,
      which is the answer to Project Euler problem 5.
*)

(* Least common multiple of two integers, expressed via GCD. *)
lcm[a_Integer, b_Integer] := Abs[a*b]/GCD[a, b]

(* Smallest positive integer divisible by all numbers from 1 to n. *)
smallestMultiple[n_Integer] := Module[
  {
    lcmResult = 1
  },
  
  (* Iteratively accumulate the LCM of 1, 2, ..., n. *)
  Do[
    lcmResult = lcm[lcmResult, i],
    {i, 1, n}
  ];
  
  (* At this point, lcmResult = lcm(1, 2, ..., n). *)
  lcmResult
]

(* Example: smallest multiple evenly divisible by all numbers from 1 to 20.
   The expected answer for Project Euler problem 5 is 232792560. *)
smallestMultiple[20]
