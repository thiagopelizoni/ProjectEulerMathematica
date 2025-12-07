(*
  Project Euler Problem 81: Path Sum: Two Ways
  URL: https://projecteuler.net/problem=081

  Problem Statement:
  Find the minimal path sum from the top-left cell to the bottom-right cell of the official 80x80 matrix provided by
  Project Euler, moving only to the right or down.

  Mathematical Analysis:
  Let C[i, j] be the minimal path sum to reach cell (i, j). The recurrence relation is:
  C[i, j] = Matrix[i, j] + Min(C[i-1, j], C[i, j-1]).
  Boundary conditions apply for the first row (only accessible from the left) and first column (only accessible from
  above). The calculation for cell (i, j) depends only on (i-1, j) and (i, j-1).
  This dependency structure allows for "wavefront" parallelization: all cells on the k-th anti-diagonal (where
  row + col = k) are independent of each other and can be computed simultaneously.

  Complexity:
  For an N x N matrix, the complexity is O(N^2). With N=80, this is computationally trivial (~6400 operations).
  The parallel overhead is likely higher than the computation cost for this small N, but the strategy demonstrates
  scalability for larger grids.

  Data Handling:
  To be fully self-contained and solve the specific "427337" instance, this script downloads the official 80x80
  matrix data directly from the Project Euler resources URL. If internet access is restricted, the 'matrix' variable
  must be manually populated with the 80x80 array.

  Implementation:
  - Import the data using `Import[..., "CSV"]`.
  - Initialize a cost matrix.
  - Iterate through anti-diagonals k = 3 to 2N.
  - Use `ParallelMap` to compute the minimum path for the current wavefront.
  - Update the cost matrix strictly in the main kernel to maintain state consistency.
*)

solve[] := Module[{nCores, matrix, url, Ndim, Cmat, k, indices, newValues, calcCost},
  nCores = $ProcessorCount;
  url = "https://projecteuler.net/project/resources/p081_matrix.txt";

  matrix = Import[url, "CSV"];

  If[!MatchQ[matrix, {{_Integer..}..}] || Length[matrix] != 80,
    Return["Error: Failed to load 80x80 matrix data correctly."]
  ];

  Ndim = Length[matrix];
  
  Cmat = matrix;

  calcCost = Function[{idx, currentMat},
    Module[{r, c, val, fromTop, fromLeft},
      r = idx[[1]];
      c = idx[[2]];
      val = currentMat[[r, c]];
      
      fromTop = If[r > 1, currentMat[[r - 1, c]], Infinity];
      fromLeft = If[c > 1, currentMat[[r, c - 1]], Infinity];
      
      If[fromTop === Infinity && fromLeft === Infinity,
        val,
        val + Min[fromTop, fromLeft]
      ]
    ]
  ];

  Do[
    indices = Table[
      {i, k - i},
      {i, Max[1, k - Ndim], Min[Ndim, k - 1]}
    ];

    newValues = ParallelMap[
      calcCost[#, Cmat] &, 
      indices,
      Method -> "CoarsestGrained"
    ];

    Do[
      Cmat[[ indices[[i, 1]], indices[[i, 2]] ]] = newValues[[i]],
      {i, 1, Length[indices]}
    ];
    ,
    {k, 3, 2 * Ndim}
  ];

  Cmat[[Ndim, Ndim]]
]

solve[]