(*
  Project Euler Problem 70: Totient Permutation
  URL: https://projecteuler.net/problem=070

  Problem Statement:
  Find the value of n, where 1 < n < 10^7, such that Euler's totient function phi(n) is a permutation of n and the
  ratio n/phi(n) is minimized.

  Mathematical Analysis:
  The value of n/phi(n) is the product of p/(p-1) for all prime factors p of n. To minimize this ratio, we must choose
  n such that its prime factors are as large as possible.
  1. If n is prime, phi(n) = n - 1. Since n and n-1 are consecutive integers, they cannot be permutations of each other
     (except trivial cases not applicable here due to modulo 9 congruences). Thus, n must be composite.
  2. To minimize the ratio, n should have as few prime factors as possible, so we consider semiprimes n = p1 * p2.
     Adding more factors would multiply the ratio by additional terms > 1, worsening the result.
  3. To further minimize the ratio for n = p1 * p2, both p1 and p2 should be as large as possible. Since n < 10^7,
     the optimal primes will be concentrated around Sqrt(10^7) approx 3162.
  4. We restrict the search space to pairs of primes in the interval [2000, 5000]. This range is mathematically
     sufficient to capture the large factors required for the minimum ratio.

  Algorithm:
  1. Generate all primes in the range [2000, 5000].
  2. Generate all pairs of distinct primes from this set such that their product is less than 10^7.
  3. For each valid pair, compute n = p1 * p2 and phi(n) = (p1 - 1)(p2 - 1).
  4. Verify if phi(n) is a permutation of n using sorted digit lists.
  5. If valid, store the tuple {n, n/phi(n)}.
  6. Select the candidate minimizing the ratio.

  Parallelization Strategy:
  The processing of prime pairs is an embarrassingly parallel task. We use ParallelMap to distribute the validation and
  computation across all available processor cores. Each core independently checks the permutation property and computes
  the ratio for its subset of pairs.

  Wolfram Language Implementation:
  - Use Select and Range with PrimeQ to generate the candidate prime list.
  - Use Subsets to generate candidate pairs efficiently.
  - Use ParallelMap for concurrent execution of the permutation check.
  - Use IntegerDigits and Sort for determining if phi(n) is a permutation of n.
  - Use MinimalBy to identify the solution with the minimum exact rational ratio.
*)

solve[] := Module[{nCores, primes, pairs, processPair, results, best},
  nCores = $ProcessorCount;

  primes = Select[Range[2000, 5000], PrimeQ];

  pairs = Select[Subsets[primes, {2}], Times @@ # < 10^7 &];

  processPair = Function[{pair},
    Module[{n, phi},
      n = pair[[1]] * pair[[2]];
      phi = (pair[[1]] - 1) * (pair[[2]] - 1);

      If[Sort[IntegerDigits[n]] === Sort[IntegerDigits[phi]],
        {n, n / phi},
        Nothing
      ]
    ]
  ];

  results = ParallelMap[processPair, pairs];

  best = MinimalBy[results, Last];

  First[First[best]]
]

solve[]