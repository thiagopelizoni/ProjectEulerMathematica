(*
  Project Euler Problem 94: Almost Equilateral Triangles
  URL: https://projecteuler.net/problem=094

  Problem Statement:
  We are interested in "almost equilateral triangles" defined by having integer side lengths and integer area. These
  triangles must have two sides of equal length 'a' and a third side 'c' that differs from 'a' by exactly 1 (i.e.,
  c = a + 1 or c = a - 1). The goal is to find the sum of the perimeters of all such triangles whose perimeter does
  not exceed 1,000,000,000.

  Mathematical Analysis:
  Let the sides be (a, a, c). The semi-perimeter s is (2a + c)/2. The area A is given by Heron's formula:
  A = Sqrt[s(s - a)^2 (s - c)].
  For the area to be an integer, the expression under the square root must be a perfect square.
  Substituting c = 2a +/- 1 leads to complex fractions, but using the altitude h simplifies the analysis.
  The altitude h to base c satisfies 4h^2 = 4a^2 - c^2.
  
  Case 1: c = a + 1. 4h^2 = 4a^2 - (a + 1)^2 = (3a + 1)(a - 1).
  Case 2: c = a - 1. 4h^2 = 4a^2 - (a - 1)^2 = (3a - 1)(a + 1).
  
  These conditions map to the Pell-like equation x^2 - 3y^2 = 1. The solutions to this Diophantine equation generate
  the sequence of valid triangles. The valid perimeters P_k correspond to the recurrence derived from the Chebyshev
  polynomials of the first kind, T_k(x). Specifically, the k-th valid perimeter can be calculated as:
  P_k = 2 * ChebyshevT[k, 2] + 2 * (-1)^k.
  
  This sequence grows exponentially with a ratio of approximately 2 + Sqrt[3] (~3.73). Given the upper bound of 10^9,
  the number of terms is very small (around 19 terms), making the problem computationally trivial O(log N).

  Parallelization Strategy:
  Despite the small number of terms, we implement dynamic parallelism as requested. We first determine the maximum
  index k (k_max) that satisfies P_k <= 10^9 using a fast sequential check (iterating until the limit is breached).
  Then, we use `ParallelSum` to compute the sum of perimeters for k from 2 to k_max. The calculation for each k is
  independent and O(1).

  Wolfram Language Implementation:
  - Detect available cores using `$ProcessorCount`.
  - Use a `While` loop to find the upper limit index `limitK`.
  - Use `ParallelSum` with `Method -> "CoarsestGrained"` to sum the perimeters.
  - `ChebyshevT` provides efficient access to the recurrence values without manual matrix exponentiation.
*)

solve[] := Module[{maxPerimeter, limitK, nCores},
  nCores = $ProcessorCount;
  maxPerimeter = 1000000000;

  limitK = 2;
  
  While[
    Module[{p},
      p = 2 * ChebyshevT[limitK + 1, 2] + 2 * (-1)^(limitK + 1);
      p <= maxPerimeter
    ],
    limitK++
  ];

  ParallelSum[
    Module[{p},
      p = 2 * ChebyshevT[k, 2] + 2 * (-1)^k;
      p
    ],
    {k, 2, limitK},
    Method -> "CoarsestGrained"
  ]
];

solve[]