(*
  Project Euler Problem 97: Large Non-Mersenne Prime
  URL: https://projecteuler.net/problem=097

  Problem Statement:
  The objective is to find the last ten digits of the massive prime number defined by the expression 28433 * 2^7830457 + 1.
  This number is a non-Mersenne prime. Calculating the complete number is unnecessary and memory-intensive; we strictly
  need the lower-order digits, which is equivalent to computing the value modulo 10^10.

  Mathematical Analysis:
  The problem requires computing R = (28433 * 2^7830457 + 1) mod 10^10. The modular exponentiation 2^7830457 mod 10^10
  can be performed efficiently using binary exponentiation (square-and-multiply), which has a time complexity of
  O(log E), where E is the exponent. Given E ~ 7.8 * 10^6, the operation count is trivial. To adhere to the requirement
  for parallel processing and demonstrate a scalable strategy for modular exponentiation with potentially larger
  exponents, we utilize the property 2^(a + b) = 2^a * 2^b (mod M). By partitioning the exponent E into additive
  components distributed across available cores, we can compute partial modular powers in parallel and reduce them via
  multiplication.

  Parallelization Strategy:
  We define the number of available cores, N. We decompose the exponent E = 7830457 into N integers {e_1, ..., e_N}
  such that their sum equals E. Each worker kernel independently computes p_i = 2^(e_i) mod 10^10. These independent
  tasks are managed via `ParallelMap`. The partial results {p_1, ..., p_N} are then aggregated on the main kernel using
  modular multiplication to obtain the full power 2^E mod 10^10. Finally, the linear transformation (multiplication by
  28433 and addition of 1) is applied modulo 10^10.

  Wolfram Language Implementation:
  - Detect `$ProcessorCount` to determine the split factor.
  - Use `Quotient` and `Mod` to evenly distribute the exponent E into a list of integers.
  - Execute `PowerMod` in parallel over this list using `ParallelMap`.
  - Use `Fold` to combine the partial powers.
  - Perform the final arithmetic and return the result.
*)

solve[] := Module[{
  nCores, exponent, multiplier, modulus,
  baseChunk, remainder, exponentChunks,
  partialPowers, totalPower, finalResult
},
  nCores = $ProcessorCount;
  exponent = 7830457;
  multiplier = 28433;
  modulus = 10^10;

  baseChunk = Quotient[exponent, nCores];
  remainder = Mod[exponent, nCores];
  
  exponentChunks = Table[baseChunk, {nCores}];
  exponentChunks = MapAt[# + remainder &, exponentChunks, 1];

  partialPowers = ParallelMap[
    PowerMod[2, #, modulus] &,
    exponentChunks,
    Method -> "CoarsestGrained"
  ];

  totalPower = Fold[
    Mod[#1 * #2, modulus] &,
    1,
    partialPowers
  ];

  finalResult = Mod[multiplier * totalPower + 1, modulus];

  finalResult
];

solve[]