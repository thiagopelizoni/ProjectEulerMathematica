(*
  Project Euler Problem 90: Cube Digit Pairs
  URL: https://projecteuler.net/problem=090

  Problem Statement:
  We must determine the number of distinct pairs of cubes (each with 6 faces labeled with digits 0-9) that can display
  all square numbers below 100: 01, 04, 09, 16, 25, 36, 49, 64, and 81. The digits 6 and 9 are interchangeable; a face
  marked 6 can be used as a 9 and vice versa. The pair of cubes is unordered, meaning {A, B} is equivalent to {B, A}.

  Mathematical Analysis:
  There are Binomial[10, 6] = 210 distinct ways to label a single cube. The number of unordered pairs of such cubes is
  the triangular number T(210) = 210 * 211 / 2 = 22,155. This search space is small enough for brute-force enumeration.
  To maximize efficiency, we represent each cube as an integer bitmask (10 bits, where the k-th bit represents digit k).
  The 6/9 interchangeability rule implies that if a cube contains a 6 or a 9, it effectively contains both. In bitmask
  terms: if bit 6 or bit 9 is set, we force both bits to be set.
  A pair of cubes (mask1, mask2) allows forming a square number (d1, d2) if:
  (mask1 has d1 AND mask2 has d2) OR (mask1 has d2 AND mask2 has d1).
  We iterate through all unique pairs, checking this condition for all 9 required squares.

  Computational Complexity:
  With N=210 cubes, we check N*(N+1)/2 pairs. For each pair, we perform 9 checks. The total number of operations is
  roughly 2 * 10^5, which runs almost instantly.

  Parallelization Strategy:
  We use `ParallelSum` to distribute the iteration over the pairs. The outer loop index `i` (1 to 210) is partitioned
  among available cores. The bitmask data is small and immutable, allowing efficient distribution to worker kernels
  without explicit synchronization or shared variables.

  Wolfram Language Implementation:
  - Define `solve[]` to encapsulate the logic.
  - Generate subsets and convert to normalized integer bitmasks (handling 6/9 logic via bitwise OR).
  - Define the target squares as pairs of digits.
  - Use `ParallelSum` with `Method -> "CoarsestGrained"` to count valid pairs.
  - Use fast integer arithmetic (`BitAnd`, `BitOr`) for validation.
*)

solve[] := Module[{subsets, masks, squares, nCubes, coreCount},
  coreCount = $ProcessorCount;
  
  subsets = Subsets[Range[0, 9], {6}];
  
  masks = Map[
    Function[s,
      Module[{m, has6, has9},
        m = Total[2^s];
        has6 = BitAnd[m, 64];  (* 2^6 *)
        has9 = BitAnd[m, 512]; (* 2^9 *)
        If[has6 > 0 || has9 > 0, BitOr[m, 576], m] (* 576 = 2^6 + 2^9 *)
      ]
    ],
    subsets
  ];
  
  nCubes = Length[masks];
  squares = {{0, 1}, {0, 4}, {0, 9}, {1, 6}, {2, 5}, {3, 6}, {4, 9}, {6, 4}, {8, 1}};
  
  ParallelSum[
    Module[{m1, m2, valid},
      m1 = masks[[i]];
      m2 = masks[[j]];
      valid = True;
      
      Do[
        If[
          !((BitAnd[m1, 2^sq[[1]]] > 0 && BitAnd[m2, 2^sq[[2]]] > 0) ||
            (BitAnd[m1, 2^sq[[2]]] > 0 && BitAnd[m2, 2^sq[[1]]] > 0)),
          valid = False;
          Break[]
        ],
        {sq, squares}
      ];
      
      If[valid, 1, 0]
    ],
    {i, 1, nCubes},
    {j, i, nCubes},
    Method -> "CoarsestGrained"
  ]
];

solve[]