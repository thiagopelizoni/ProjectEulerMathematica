(*
  Project Euler Problem 90: Cube Digit Pairs
  URL: https://projecteuler.net/problem=090

  Problem Statement:
  The objective is to count the number of distinct unordered pairs of cubes, each marked with 6 digits chosen from {0..9},
  such that the pair can collectively display all 9 square numbers below 100 (01, 04, 09, 16, 25, 36, 49, 64, 81). The
  digits 6 and 9 are functionally interchangeable.

  Mathematical Analysis:
  The solution space is extremely small: C(10, 6) = 210 unique cubes, leading to 22,155 unordered pairs. This enables a
  complete brute-force search. To ensure maximum stability and speed in a script environment, we use integer bitmasks to
  represent the sets of digits on each cube, which allows O(1) checks.
  
  Each cube set is mapped to an integer (mask) where the k-th bit is set if digit k is present. The 6/9 rule is handled
  by ensuring that if bit 6 or bit 9 is set, both are set (via BitOr with 576 = 2^6 + 2^9).
  The required check for a pair of masks (m1, m2) against a square (d1, d2) uses bitwise operations:
  (BitAnd[m1, 2^d1] > 0 AND BitAnd[m2, 2^d2] > 0) OR (BitAnd[m1, 2^d2] > 0 AND BitAnd[m2, 2^d1] > 0).
  
  The data (masks and squares) is defined globally to ensure correct distribution to parallel worker kernels, resolving
  scoping issues common in `wolframscript` execution of parallel code.

  Parallelization Strategy:
  We use `ParallelSum` to distribute the outer loop of the pair generation across all available CPU cores. The logic is
  purely functional and independent for each pair check, resulting in an embarrassingly parallel workload.
  
  Wolfram Language Implementation:
  All constants (`normalizedMasks`, `squares`) and the validation function (`isValidPair`) are defined globally to ensure
  reliable visibility within the parallel context. The `solve[]` function executes the parallel summation.
*)

(* --- Global Definitions for Robust Parallel Execution --- *)

nCores = $ProcessorCount;

rawSubsets = Subsets[Range[0, 9], {6}];

normalizedMasks = Map[
  Function[s,
    Module[{m = Total[2^s]},
      If[BitAnd[m, 576] > 0, BitOr[m, 576], m]
    ]
  ],
  rawSubsets
];

squares = {{0, 1}, {0, 4}, {0, 9}, {1, 6}, {2, 5}, {3, 6}, {4, 9}, {6, 4}, {8, 1}};

isValidPair[m1_, m2_] := AllTrue[
  squares,
  Function[{sq},
    With[{d1 = sq[[1]], d2 = sq[[2]], mask1 = 2^sq[[1]], mask2 = 2^sq[[2]]},
      (BitAnd[m1, mask1] > 0 && BitAnd[m2, mask2] > 0) ||
      (BitAnd[m1, mask2] > 0 && BitAnd[m2, mask1] > 0)
    ]
  ]
];

solve[] := Module[{nCubes = Length[normalizedMasks]},
  ParallelSum[
    If[isValidPair[normalizedMasks[[i]], normalizedMasks[[j]]], 1, 0],
    {i, 1, nCubes},
    {j, i, nCubes},
    Method -> "CoarsestGrained"
  ]
];

solve[]