(*
  Project Euler Problem 87: Prime Power Triples
  URL: https://projecteuler.net/problem=087

  Problem Statement:
  The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact,
  there are exactly four numbers below 50 that can be expressed in such a way:
  28 = 2^2 + 2^3 + 2^4
  33 = 3^2 + 2^3 + 2^4
  49 = 5^2 + 2^3 + 2^4
  47 = 2^2 + 3^3 + 2^4
  How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?

  Mathematical Analysis:
  We seek the size of the set { n < 50,000,000 | n = p1^2 + p2^3 + p3^4, where p1, p2, p3 are primes }.
  Let N = 50,000,000.
  The maximum possible values for the primes are bounded by the roots of N:
  - p1_max = Floor[N^(1/2)] approx 7071
  - p2_max = Floor[N^(1/3)] approx 368
  - p3_max = Floor[N^(1/4)] approx 84
  
  The search space is the Cartesian product of the sets of primes up to these bounds.
  Let P2 be primes <= 7071, P3 be primes <= 368, P4 be primes <= 84.
  We iterate through all combinations (p1, p2, p3) in P2 x P3 x P4, compute the sum, check if it is less than N,
  and store the valid sums. Since different combinations might produce the same sum, we must count the number of
  unique sums.
  
  Complexity:
  |P2| approx 908, |P3| approx 73, |P4| approx 23.
  Total triplets approx 908 * 73 * 23 approx 1.5 million.
  This is extremely small. Generating all sums and finding the length of the union is computationally trivial (<< 1s).
  
  Parallelization Strategy:
  We can decompose the work by the outermost loop (e.g., iterating over p3 from P4).
  We distribute the calculation of valid partial sums (p1^2 + p2^3 + fixed_p3^4) across available cores.
  Each worker returns a list of valid sums.
  Finally, we flatten the results and apply `Union` (or `Length[DeleteDuplicates[...]]`) to count unique values.

  Wolfram Language Implementation:
  - Compute prime lists `primes2`, `primes3`, `primes4` using `PrimePi` and `Prime`.
  - Use `ParallelTable` iterating over `primes4` (the smallest set, suitable for chunking) or `primes3`.
    Given the small size, parallelizing over the largest set is usually safer for load balancing, but here total work
    is minimal. Parallelizing over `primes3` (73 items) is a good balance for `CoarsestGrained`.
  - Inside the worker, iterate over `primes2` and `primes3`.
  - Collect all sums < N.
  - Return distinct counts via `Union`.
*)

solve[] := Module[{nCores, limit, primes2, primes3, primes4, sums},
  nCores = $ProcessorCount;
  limit = 50000000;

  primes2 = Prime[Range[PrimePi[Floor[limit^(1/2)]]]];
  primes3 = Prime[Range[PrimePi[Floor[limit^(1/3)]]]];
  primes4 = Prime[Range[PrimePi[Floor[limit^(1/4)]]]];

  sums = ParallelTable[
    Module[{p3, p4, currentSum, s2, s3, s4, validSums = {}},
      s3 = pCube^3;
      Do[
        s4 = pFourth^4;
        If[s3 + s4 >= limit, Continue[]];
        
        Do[
          s2 = pSquare^2;
          currentSum = s2 + s3 + s4;
          If[currentSum < limit,
            AppendTo[validSums, currentSum],
            Break[]
          ],
          {pSquare, primes2}
        ],
        {pFourth, primes4}
      ];
      validSums
    ],
    {pCube, primes3},
    Method -> "CoarsestGrained"
  ];

  Length[Union[Flatten[sums]]]
]

solve[]