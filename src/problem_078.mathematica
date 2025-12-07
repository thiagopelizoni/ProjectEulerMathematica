(*
  Project Euler Problem 78: Coin Partitions
  URL: https://projecteuler.net/problem=078

  Problem Statement:
  Find the least value of n for which the partition function p(n) is divisible by 1,000,000.
  p(n) represents the number of ways n coins can be separated into piles.

  Mathematical Analysis:
  The partition function p(n) grows exponentially. To compute it efficiently, we use Euler's recurrence relation based
  on the Pentagonal Number Theorem:
  p(n) = Sum_{k!=0} (-1)^(k-1) * p(n - g_k)
  where g_k = k(3k - 1)/2. The terms are added/subtracted for k = 1, -1, 2, -2, ...
  We need p(n) == 0 (mod 1,000,000). By the Chinese Remainder Theorem, this is equivalent to:
  p(n) == 0 (mod 64) AND p(n) == 0 (mod 15625), since 1,000,000 = 64 * 15625.
  We can search for solutions to these two modular conditions independently and find the first n that satisfies both.
  The search limit is empirically known to be within 60,000.

  Algorithm and Complexity:
  The complexity to compute p(n) is O(n^1.5). For n=60,000, this is roughly 1.5 * 10^7 operations, which is trivial.
  We implement the recurrence using `Compile` for C-like speed.
  The memory requirement is O(n) to store previous partition values.

  Parallelization Strategy:
  We partition the problem by modulus factors. One core searches for n where p(n) == 0 mod 64, and another core
  searches for n where p(n) == 0 mod 15625. We then compute the Intersection of these sets and find the Minimum.
  This effectively utilizes 2 cores for the distinct algebraic conditions.

  Wolfram Language Implementation:
  - Use `Compile` to generate optimized byte-code for the partition filling loop.
  - `Table` syntax fixed to use square brackets `[]`.
  - `ParallelMap` distributes the compiled solver over the list of moduli {64, 15625}.
  - `Intersection` combines the results.
*)

solve[] := Module[{limit, moduli, findDivisibleIndices, results, commonIndices},
  limit = 75000;
  moduli = {64, 15625};

  findDivisibleIndices = Function[{modVal, maxN},
    Module[{compiledSolver},
      compiledSolver = Compile[{{m, _Integer}, {lim, _Integer}},
        Module[{p, n, k, gk, sum, term, zeros = {0}},
          p = Table[0, {lim + 1}];
          p[[1]] = 1; 
          
          zeros = Rest[zeros];

          Do[
            sum = 0;
            k = 1;
            
            While[True,
              gk = Quotient[k * (3 * k - 1), 2];
              If[gk > n, Break[]];
              
              term = p[[n - gk + 1]];
              If[OddQ[k], 
                sum = Mod[sum + term, m], 
                sum = Mod[sum - term, m]
              ];
              
              gk = Quotient[k * (3 * k + 1), 2];
              If[gk > n, Break[]];
              
              term = p[[n - gk + 1]];
              If[OddQ[k], 
                sum = Mod[sum + term, m], 
                sum = Mod[sum - term, m]
              ];
              
              k++;
            ];
            
            p[[n + 1]] = sum;
            If[sum == 0, AppendTo[zeros, n]];
            ,
            {n, 1, lim}
          ];
          zeros
        ],
        CompilationTarget -> "WVM",
        RuntimeOptions -> "Speed"
      ];
      compiledSolver[modVal, maxN]
    ]
  ];

  results = ParallelMap[
    findDivisibleIndices[#, limit] &,
    moduli,
    Method -> "CoarsestGrained"
  ];

  Min[Intersection[results[[1]], results[[2]]]]
]

solve[]