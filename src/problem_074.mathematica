(*
  Project Euler Problem 74: Digit Factorial Chains
  URL: https://projecteuler.net/problem=074

  Problem Statement:
  A digit factorial chain is formed by calculating the sum of the factorials of the digits of the current number to
  produce the next number. The chain ends when a value repeats, forming a cycle. The "length" of the chain is the number
  of distinct non-repeating terms in the sequence. For example, the chain starting at 169 (169 -> 363601 -> 1454 -> 169)
  has length 3. We must find how many starting numbers below 1,000,000 produce a chain of exactly 60 non-repeating terms.

  Mathematical Analysis:
  We define a transformation S(n) as the sum of the factorials of the digits of n. The sequence is x_0 = n, x_{i+1} =
  S(x_i). Since the maximum sum of factorials for a 7-digit number (9,999,999) is 7 * 9! = 2,540,160, all sequences
  starting below 1,000,000 are bounded and must eventually repeat. We can view this as a walk on a functional graph
  where every component consists of a set of trees leading into a unique cycle. The problem requires finding nodes whose
  path to the first repeated element contains exactly 60 distinct vertices.

  Complexity and Feasibility:
  The input size is N = 10^6. The maximum chain length of interest is 60. Calculating S(n) involves digit splitting and
  summing precomputed factorials, which is an O(log n) operation (very fast). The total complexity is proportional to
  N * L, where L=60. This results in approximately 6 * 10^7 basic operations, which is computationally trivial for a
  modern CPU (taking a few seconds). Memory usage is minimal as we process numbers independently or in parallel chunks.

  Parallelization Strategy:
  The problem is embarrassingly parallel. We partition the search space [1, 999,999] among the available processor cores
  using `ParallelSum`. Each core independently simulates the chain for its assigned numbers. To avoid race conditions
  and synchronization overhead, we do not share a global cache; instead, each worker computes the chain from scratch using
  local data structures. Given the short target length (60), the redundant re-computation of shared path segments is
  negligible compared to the overhead of managing a distributed cache.

  Wolfram Language Implementation:
  - We define a main `solve` function that sets up the parallel computation.
  - Inside `ParallelSum`, we execute a `Module` for each number `n`.
  - We use a local `Association` (hash map) `seen` to track visited numbers in the current chain for O(1) lookups.
  - We use `IntegerDigits` and a precomputed list of factorials to efficiently calculate the next term.
  - The loop terminates when a repeat is found or the chain exceeds 60 terms.
*)

solve[] := Module[{nCores, result},
  nCores = $ProcessorCount;

  result = ParallelSum[
    Module[{current, seen, count, fact, nextTerm},
      fact = {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880};
      
      current = n;
      seen = <|current -> True|>;
      count = 1;
      
      While[count <= 60,
        nextTerm = Total[Part[fact, IntegerDigits[current] + 1]];
        
        If[KeyExistsQ[seen, nextTerm], Break[]];
        
        seen[nextTerm] = True;
        current = nextTerm;
        count++;
      ];
      
      If[count == 60, 1, 0]
    ],
    {n, 1, 999999},
    Method -> "CoarsestGrained"
  ];

  result
]

solve[]