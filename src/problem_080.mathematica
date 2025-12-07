(*
  Project Euler Problem 80: Square Root Digital Expansion
  URL: https://projecteuler.net/problem=080

  Problem Statement:
  Calculate the total sum of the digital sums of the first 100 decimal digits for all irrational square roots of natural
  numbers up to 100. For rational roots (perfect squares), the sum is zero (or rather, they are excluded). The "decimal
  digits" include the integer part (e.g., for sqrt(2) = 1.414..., the digits are 1, 4, 1, 4...).

  Mathematical Analysis:
  For a non-square integer n, sqrt(n) is irrational. Its decimal expansion does not terminate or repeat. To find the
  first k digits, one can compute integer square roots of large numbers. Specifically, the first k digits of sqrt(n)
  correspond to the integer part of sqrt(n * 10^(2k)) (with appropriate scaling if leading zeros existed, which is not
  the case for n >= 1). Alternatively, using high-precision floating-point arithmetic is sufficient provided the
  precision exceeds k digits to avoid rounding errors affecting the k-th digit.
  Since the constraint is small (n <= 100, k = 100 digits), the complexity is dominated by the arbitrary-precision
  arithmetic, which is effectively O(k^1.585) or O(M(k)) per number. With k=100, this is computationally trivial. The
  calculation involves identifying perfect squares (which have integer roots) and summing the digits of the irrational
  ones.

  Parallelization Strategy:
  The calculation for each number n is independent. We can distribute the range [1, 100] across available processor
  cores. A parallel summation calculates the digital sum for each n (returning 0 if n is a perfect square) and aggregates
  the results. This minimizes total runtime, although the problem is solvable in milliseconds even serially.

  Wolfram Language Implementation:
  - Detect available cores using $ProcessorCount.
  - Use ParallelSum to iterate n from 1 to 100.
  - Inside the loop, check if n is a perfect square using IntegerQ[Sqrt[n]].
  - If irrational, use RealDigits[Sqrt[n], 10, 100] to extract exactly 100 digits. Wolfram Language automatically
    handles the required internal precision when RealDigits is called on an exact surd.
  - Compute the Total of these digits.
  - Sum all contributions.
*)

solve[] := Module[{nCores, result},
  nCores = $ProcessorCount;

  result = ParallelSum[
    If[IntegerQ[Sqrt[n]],
      0,
      Total[First[RealDigits[Sqrt[n], 10, 100]]]
    ],
    {n, 1, 100},
    Method -> "CoarsestGrained"
  ];

  result
]

solve[]