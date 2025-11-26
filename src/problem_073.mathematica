(*
  Project Euler Problem 73: Counting Fractions in a Range
  URL: https://projecteuler.net/problem=073

  Problem Statement:
  Consider the set of reduced proper fractions n/d with denominators d <= 12,000. We wish to count the number of such
  fractions that lie strictly between 1/3 and 1/2. That is, we must find the cardinality of the set
  { n/d | 1 <= d <= 12,000, 1/3 < n/d < 1/2, gcd(n, d) = 1 }.

  Mathematical Analysis:
  The condition 1/3 < n/d < 1/2 is equivalent to d/3 < n < d/2. For a fixed denominator d, the numerators n must be
  integers in the range floor(d/3) + 1 to ceiling(d/2) - 1.
  Let this range of numerators be R(d). We need to count the number of n in R(d) such that gcd(n, d) = 1.
  The total count is the summation over d from 2 to 12,000 of count({n in R(d) | gcd(n, d) = 1}).
  
  Alternative approach (Recursive Mobius Inversion / Farey Sequences):
  The Stern-Brocot tree or Farey sequence properties can be used to generate fractions in order, but given the upper
  bound D = 12,000, an O(D^2) naive generation is feasible (approx 1.44 * 10^8 operations, less with the range
  constraint). The fraction of pairs (n, d) in the range is roughly (1/2 - 1/3) = 1/6 of total pairs.
  Total pairs roughly D^2/2 * (6/pi^2). For D=12,000, total fractions ~ 0.6 * 72 * 10^6 ~ 43 million.
  1/6 of this is ~7 million.
  A direct check of gcd(n, d) for n in the specific range is computationally inexpensive for D=12,000.
  
  Complexity:
  The loop iterates d from 4 (since for d=2,3 no fraction strictly between 1/3 and 1/2 exists) to 12,000.
  For each d, n ranges from ~d/3 to ~d/2, a range of length d/6.
  Total operations ~ Sum(d/6) for d=1 to D = 1/6 * D^2/2 = D^2/12.
  For D=12,000, D^2 = 1.44*10^8. Operations ~ 1.2*10^7 gcd checks. This is extremely fast (sub-second) in compiled or
  parallelized Wolfram Language code. No advanced sieving is strictly necessary for this limit, but we will use a
  direct summation of EulerPhi-like logic or simple GCD tests distributed across cores.

  Parallelization Strategy:
  We can partition the range of denominators [4, 12000] into disjoint intervals. Each worker computes the count of valid
  numerators for its subset of denominators. The results are aggregated via summation.
  
  Implementation:
  - Define function `countRange[d]` that counts integers n in (d/3, d/2) with gcd(n, d) == 1.
  - Use ParallelSum or ParallelMap to distribute d.
  - Using GCD is native and fast.

  Optimization note:
  Floor(d/3) + 1 can be written as Quotient[d, 3] + 1.
  Ceiling(d/2) - 1 is Quotient[d - 1, 2] (checking edge cases: if d=2k, d/2-1 = k-1; if d=2k+1, ceil(k+0.5)-1 = k.
  Quotient[(2k)-1, 2] = k-1. Quotient[(2k+1)-1, 2] = k. Correct).
*)

solve[] := Module[{nCores, limit, countForDenominator, totalCount},
  nCores = $ProcessorCount;
  limit = 12000;

  (*
    Worker function to count valid numerators for a specific denominator d.
    Range: (d/3, d/2)
    Lower bound integer n_min: Floor(d/3) + 1
    Upper bound integer n_max: Ceiling(d/2) - 1
  *)
  countForDenominator = Function[{d},
    Module[{nMin, nMax, count, n},
      nMin = Quotient[d, 3] + 1;
      (* Ceiling[x] - 1 is equivalent to Floor[x - epsilon] for integers, or simple arithmetic.
         Ceiling(d/2) - 1. If d is even, d/2 - 1. If d is odd, (d+1)/2 - 1 = (d-1)/2.
         This is exactly Quotient[d - 1, 2].
      *)
      nMax = Quotient[d - 1, 2];
      
      count = 0;
      Do[
        If[GCD[n, d] == 1, count++],
        {n, nMin, nMax}
      ];
      count
    ]
  ];

  (* ParallelSum automatically distributes the range of d values across available kernels.
     We start from 4 because for d=1,2,3 the range (d/3, d/2) contains no integers.
  *)
  totalCount = ParallelSum[
    countForDenominator[d],
    {d, 4, limit},
    Method -> "CoarsestGrained"
  ];

  totalCount
]

solve[]