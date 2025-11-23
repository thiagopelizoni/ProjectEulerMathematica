(* Project Euler Problem 52: Permuted Multiples

   Problem Description:
   Find the smallest positive integer x such that 2x, 3x, 4x, 5x, and 6x contain exactly the same digits.

   Mathematical Solution:
   1. Constraints on Magnitude:
      For x and 6x to share the same digits, they must have the same number of digits (d).
      This implies that x cannot produce a carry into the next power of 10 when multiplied by 6.
      Therefore, x must satisfy the bound $10^{d-1} \le x < \frac{10^d}{6}$.
      This necessitates that the leading digit of x is always 1.

   2. Digit Signature Comparison:
      To verify the permutation property efficiently, we define a signature function $S(n)$ 
      which sorts the decimal digits of integer n. The condition is satisfied if and only if:
      $S(x) = S(2x) = S(3x) = S(4x) = S(5x) = S(6x)$.

   3. Parallel Search Strategy:
      Since the property check for any integer x is independent of other integers, the search space 
      can be partitioned across available processor cores. We iterate through a sufficient range 
      starting from 1, distributing the workload dynamically. The first integer satisfying the 
      multiplicity predicate is the solution.
*)

If[Length[Kernels[]] < $ProcessorCount, LaunchKernels[]];

hasPermutedMultiples[n_] := Module[{target = Sort[IntegerDigits[n]]},
  AllTrue[Range[2, 6], Sort[IntegerDigits[n * #]] === target &]
];

DistributeDefinitions[hasPermutedMultiples];

SelectFirst[
  ParallelTable[
    If[hasPermutedMultiples[x], x, Nothing],
    {x, 1, 200000},
    Method -> "CoarsestGrained"
  ],
  NumberQ
]