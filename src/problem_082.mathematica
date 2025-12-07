(*
  Project Euler Problem 82: Path Sum: Three Ways
  URL: https://projecteuler.net/problem=082

  Problem Statement:
  Find the minimal path sum in an 80x80 matrix by moving from the left column to the right column. The allowed moves
  for any cell (i, j) are: Right (i, j+1), Up (i-1, j), and Down (i+1, j). The path can start at any row in the
  first column and end at any row in the last column.

  Mathematical Analysis:
  Let M be the N x N matrix. We treat the problem as finding the shortest path in a DAG-like structure where columns
  act as stages. While a standard O(N^2) Dynamic Programming approach exists (sweeping each column vertically then
  moving right), it is inherently sequential. To utilize parallel architectures effectively, we lift the computation
  to the (min, +) semiring.
  We decompose the matrix into vertical strips (blocks of columns). For each strip, we compute a Transfer Matrix T,
  where T[u, v] represents the minimum path cost to traverse the strip starting at row u of the first column and
  ending at row v of the last column.
  The merge operation for two adjacent strips with transfer matrices A and B is the matrix product over the (min, +)
  semiring: (A . B)[i, j] = min_k (A[i, k] + B[k, j]).
  This formulation transforms the linear dependency chain into an associative reduction, allowing us to compute
  strip matrices in parallel and then reduce them.
  
  Complexity:
  Let N be the matrix dimension (80) and P be the number of cores.
  1. Strip Computation: Each of the P strips has width W = N/P. We compute the N x N transfer matrix by running N
     independent DP sweeps. Cost: O(P * N * (N * W)) = O(N^3). Parallel time: O(N^3 / P).
  2. Reduction: Log P matrix multiplications. Cost: O(N^3 log P).
  Total work is O(N^3), which is computationally feasible for N=80 (~5*10^5 ops). This approach trades O(N^2) serial
  efficiency for O(N^3) parallel scalability, appropriate for scientific computing contexts.

  Implementation Strategy:
  - Import data from the provided URL.
  - Define a compiled function `minPlusDot` for efficient (min, +) matrix multiplication.
  - Define `processBlock` to compute the transfer matrix for a range of columns using sequential DP sweeps (up/down).
  - Use `ParallelMap` to compute transfer matrices for column chunks.
  - Aggregate results using `Fold` (or parallel reduction) with `minPlusDot`.
  - The final answer is the minimum value in the resulting matrix.
*)

solve[] := Module[{
  nCores, url, matrix, nRows, nCols, infinity,
  minPlusDot, processBlock, chunks, transferMatrices, finalMatrix
},
  nCores = $ProcessorCount;
  url = "https://projecteuler.net/project/resources/p082_matrix.txt";
  
  matrix = Import[url, "CSV"];
  {nRows, nCols} = Dimensions[matrix];
  infinity = 2^60;

  minPlusDot = Compile[{{A, _Integer, 2}, {B, _Integer, 2}},
    Module[{n = Length[A], m = Length[B[[1]]], p = Length[B], C, i, j, k, minVal, sum},
      C = Table[0, {n}, {m}];
      Do[
        Do[
          minVal = 9223372036854775807;
          Do[
            sum = A[[i, k]] + B[[k, j]];
            If[sum < minVal, minVal = sum];
            , {k, 1, p}];
          C[[i, j]] = minVal;
          , {j, 1, m}];
        , {i, 1, n}];
      C
    ],
    CompilationTarget -> "WVM",
    RuntimeOptions -> "Speed"
  ];

  processBlock = Function[{range},
    Module[{cStart = range[[1]], cEnd = range[[2]], blockRes, startRow, costs, j, i},
      blockRes = Table[0, {nRows}, {nRows}];
      
      Do[
        costs = ConstantArray[infinity, nRows];
        costs[[startRow]] = matrix[[startRow, cStart]];
        
        Do[
          costs[[i]] = Min[costs[[i]], costs[[i - 1]] + matrix[[i, cStart]]],
          {i, 2, nRows}
        ];
        Do[
          costs[[i]] = Min[costs[[i]], costs[[i + 1]] + matrix[[i, cStart]]],
          {i, nRows - 1, 1, -1}
        ];
        
        Do[
          Do[costs[[i]] = costs[[i]] + matrix[[i, j]], {i, 1, nRows}];
          
          Do[
            costs[[i]] = Min[costs[[i]], costs[[i - 1]] + matrix[[i, j]]],
            {i, 2, nRows}
          ];
          Do[
            costs[[i]] = Min[costs[[i]], costs[[i + 1]] + matrix[[i, j]]],
            {i, nRows - 1, 1, -1}
          ];
          , {j, cStart + 1, cEnd}
        ];
        
        blockRes[[startRow]] = costs;
        , {startRow, 1, nRows}
      ];
      blockRes
    ]
  ];

  chunks = Partition[Range[nCols], UpTo[Ceiling[nCols / nCores]]];
  
  transferMatrices = ParallelMap[
    processBlock[{First[#], Last[#]}] &, 
    chunks,
    Method -> "CoarsestGrained"
  ];

  finalMatrix = Fold[minPlusDot, First[transferMatrices], Rest[transferMatrices]];

  Min[finalMatrix]
]

solve[]