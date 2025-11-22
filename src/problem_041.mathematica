(* Project Euler Problem 41: Pandigital Prime

   Problem Description:
   We seek the largest integer n that is both prime and n-pandigital.
   An n-pandigital number makes use of all the digits 1 to n exactly once.
   (e.g., 2143 is a 4-pandigital prime).

   Mathematical Solution:
   1. Dimensionality Reduction (Modular Arithmetic):
      A necessary condition for primality is that the sum of digits is not divisible by 3.
      - For n=9: Sum(1..9) = 45. 45 == 0 (mod 3). All 9-pandigital numbers are composite.
      - For n=8: Sum(1..8) = 36. 36 == 0 (mod 3). All 8-pandigital numbers are composite.
      - For n=7: Sum(1..7) = 28. 28 == 1 (mod 3). Primes may exist.
      
      Although the algorithm implemented below generally iterates from n=9 down to 1 to ensure
      correctness without assuming this theorem, a mathematician knows the solution space 
      is effectively restricted to permutations of {1,..,7}.

   2. Algorithm:
      Iterate n from 9 down to 1 (descending search ensures the first result is the maximum).
      Generate the symmetric group S_n (all permutations of digits 1..n).
      Map permutations to integers and sort in descending order.
      The first element satisfying the primality test is the global maximum.
*)

LargestPandigitalPrime[] := Module[{n, candidates, result},
  Do[
    candidates = Reverse[FromDigits /@ Permutations[Range[n]]];
    result = SelectFirst[candidates, PrimeQ];
    If[IntegerQ[result], Return[result]]
  , {n, 9, 1, -1}]
]

LargestPandigitalPrime[]