(*
  Project Euler Problem 77: Prime Summations
  URL: https://projecteuler.net/problem=077

  Problem Statement:
  Find the first value which can be written as the sum of primes in over 5000 different ways.

  Mathematical Analysis:
  We are looking for the smallest integer n such that the number of partitions of n into prime parts, denoted as Q(n),
  exceeds 5000. This is a classic integer partition problem restricted to a specific set of summands (the prime numbers).
  The generating function for partitions into primes is given by the infinite product:
  G(x) = Product[1 / (1 - x^p), {p in Primes}] = Sum[Q(n) * x^n, {n, 0, Infinity}].
  While one could use Euler's pentagonal number theorem for unrestricted partitions, for restricted partitions (especially
  with irregular parts like primes), a dynamic programming approach (or essentially expanding the polynomial) is more
  efficient for small n.
  Let ways[j] be the number of ways to write j as a sum of primes.
  We can compute this iteratively. Initialize ways[0] = 1 and ways[j] = 0 for j > 0.
  For each prime p, update the array: ways[j] += ways[j - p] for j from p to the target limit.
  Since we need to find the *first* value exceeding 5000, we can guess an upper bound or dynamically resize.
  Given the exponential growth of partition functions, the target n will be small (likely < 100).
  A bound of n=100 is sufficient to check. If the threshold isn't met, we could extend, but 5000 is a very small number
  combinatorially.

  Complexity and Feasibility:
  The algorithm is O(N * pi(N)) where N is the upper search limit and pi(N) is the number of primes up to N.
  For N ~ 100, this is negligible. It's effectively instantaneous.
  
  Parallelization Strategy:
  Given the extremely small search space (N approx 70-80), parallelization adds overhead rather than speed. The dependencies
  in the dynamic programming array (knapsack-style update) make it inherently serial for a single array update.
  However, to satisfy the prompt's requirement for parallelism and demonstrate the technique, we can implement a
  parallelized search over candidate ranges if we were checking a property for each n independently, or use
  ParallelMap to compute partitions for disjoint ranges if we were using a different (non-DP) method.
  A better approach for this specific structure that respects the prompt's constraints is to calculate the partition
  values using the generating function approach where polynomial multiplication is parallelizable, OR simply acknowledge
  that for this specific instance, the "parallelism" is the detection of the core count to strictly follow the template,
  even if the computation runs serially due to its trivial size.
  Alternatively, we can define a function `countPrimePartitions[n]` that computes the value independently for a specific n
  (using recursion with memoization or generating functions) and map this over a range in parallel. This is computationally
  more expensive per n but perfectly parallelizable. We will use the independent calculation strategy to robustly demonstrate
  parallelism.
  
  Independent calculation of Q(n):
  Q(n) is the coefficient of x^n in Product_{p <= n} (1-x^p)^-1.
  Computing this coefficient for a single n can be done via SeriesCoefficient or polynomial operations.
  We will map `countPrimePartitions` over a candidate range (e.g., 2 to 100) in parallel and find the first n where
  result > 5000.

  Wolfram Language Implementation:
  - Detect core count.
  - Define a worker function `getPrimePartitionCount[n]` that computes the coefficient of x^n in the prime generating function.
  - Use `SeriesCoefficient` with `InverseSeries` or `Series` of the product term.
  - Map this function over the range 2..100 using `ParallelMap`.
  - Use `FirstPosition` or `SelectFirst` to identify the answer.
*)

solve[] := Module[{nCores, limit, primes, partitions, index, threshold, calcPartition},
  nCores = $ProcessorCount;
  threshold = 5000;
  limit = 100; (* Heuristic upper bound, sufficient for 5000 *)

  (* Function to calculate prime partitions for a specific n explicitly *)
  (* Q(n) is Coeff of x^n in Product[1/(1-x^p)]. *)
  (* We only need the product of primes up to n to determine the coef for x^n accurately. *)
  calcPartition = Function[{n},
    Module[{pList, genFunc},
      pList = Prime[Range[PrimePi[n]]];
      (* We construct the Series product. Normal[Series[...]] gives the polynomial. *)
      (* We need terms up to x^n. O[x]^(n+1) handles truncation automatically. *)
      genFunc = Product[
        Series[1/(1 - x^p), {x, 0, n}], 
        {p, pList}
      ];
      SeriesCoefficient[genFunc, n]
    ]
  ];

  (* Perform the calculation in parallel across the range *)
  partitions = ParallelMap[
    calcPartition, 
    Range[2, limit],
    Method -> "CoarsestGrained"
  ];

  (* Find the first index where the count exceeds the threshold *)
  (* The list starts at n=2, so index i corresponds to n = i + 1 *)
  index = FirstPosition[partitions, count_ /; count > threshold];
  
  (* Convert list index back to number n *)
  (* If index is {i}, the number is i + 1 (since Range started at 2) => i + 1 is actually Range[2,limit][[i]] *)
  (* Range[2, limit][[index[[1]]]] *)
  Range[2, limit][[index[[1]]]]
]

solve[]