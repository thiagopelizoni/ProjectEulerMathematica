(* Project Euler Problem 35: Circular Primes

   Problem Description:
   A "circular prime" is defined as a prime number with the property that all cyclic permutations 
   of its digits are also prime numbers. For example, 197 is a circular prime because the set 
   of permutations {197, 971, 719} consists entirely of primes. The objective is to determine 
   the cardinality of the set of all circular primes below 1,000,000.

   Mathematical Solution:
   1. Domain Definition: Let S be the set of prime numbers p such that p < 10^6.
      (Note: Iterating only over primes is mathematically sufficient, as any composite number 
      fails the condition immediately on the 0-th rotation).
   2. Cyclic Permutation Group: For each p in S, generate the orbit O_p under the action of 
      cyclic shifting of its decimal digits.
   3. Verification: A prime p satisfies the condition if and only if for all q in O_p, q is prime.
      The function tests primality for every generated rotation.
   4. Aggregation: Count the number of elements in S that satisfy the verification step.
*)

CircularPrimeQ[n_Integer] := Module[{digits, rotations},
  digits = IntegerDigits[n];
  rotations = FromDigits /@ Table[RotateLeft[digits, i], {i, 0, Length[digits] - 1}];
  AllTrue[rotations, PrimeQ]
]

limit = 1000000;
candidatePrimes = Prime[Range[PrimePi[limit]]];

Count[candidatePrimes, _?CircularPrimeQ]