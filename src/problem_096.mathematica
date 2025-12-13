(*
  Project Euler Problem 96: Su Doku
  URL: https://projecteuler.net/problem=096

  Problem Statement:
  The problem involves solving 50 distinct 9x9 Sudoku puzzles provided in a text file. For each solved puzzle, we
  must extract the 3-digit number formed by the digits in the top-left corner (row 1, columns 1, 2, and 3). The final
  answer is the sum of these 50 distinct 3-digit numbers. Standard Sudoku rules apply: each row, column, and 3x3
  sub-grid must contain the digits 1 through 9 exactly once.

  Mathematical Analysis:
  Sudoku is a Constraint Satisfaction Problem (CSP). For a 9x9 grid, the search space is finite but large. Efficient
  solving is achieved using a Depth-First Search (DFS) backtracking algorithm augmented with the Minimum Remaining
  Values (MRV) heuristic.
  1. MRV Heuristic: At each step of the recursion, we select the empty cell with the fewest valid candidate digits.
     This minimizes the branching factor of the search tree and detects dead ends (0 candidates) early.
  2. Constraints: A digit is valid for a cell if it is not already present in the cell's row, column, or 3x3 block.
  3. Backtracking: If a valid assignment leads to a state where a future cell has no candidates, the algorithm
     backtracks.
  Given the small number of puzzles (50) and the efficiency of MRV on 9x9 grids, the total computation time is
  negligible.

  Parallelization Strategy:
  The 50 puzzles are completely independent tasks. We parse the input data into a list of 50 matrices and verify that
  solver definitions are distributed to all worker kernels. We then use `ParallelMap` to solve the puzzles concurrently.
  This "embarrassingly parallel" structure allows linear scaling with the number of cores.

  Wolfram Language Implementation:
  - We define the solver functions (`getCandidates`, `findBestCell`, `backtrack`, `solveOne`) in the global scope to
    ensure they are correctly distributed to parallel kernels.
  - `Import` is used to retrieve the dataset directly from the Project Euler resources.
  - `LaunchKernels` and `DistributeDefinitions` are explicitly called to stabilize the parallel environment.
  - The final aggregation uses `Total` on the extracted top-left digits.
*)

getCandidates[board_, r_, c_] := Module[{row, col, blockRow, blockCol, block, used},
  row = board[[r]];
  col = board[[All, c]];
  blockRow = 3 * Quotient[r - 1, 3] + 1;
  blockCol = 3 * Quotient[c - 1, 3] + 1;
  block = Flatten[board[[blockRow ;; blockRow + 2, blockCol ;; blockCol + 2]]];
  
  used = Union[row, col, block];
  Complement[{1, 2, 3, 4, 5, 6, 7, 8, 9}, used]
];

findBestCell[board_] := Module[{emptyPos, bestPos, minLen, cands, currentCands, currentLen},
  emptyPos = Position[board, 0, {2}];
  
  If[Length[emptyPos] == 0, Return[Null]];
  
  minLen = 10;
  bestPos = $Failed;
  cands = {};
  
  Do[
    currentCands = getCandidates[board, pos[[1]], pos[[2]]];
    currentLen = Length[currentCands];
    
    If[currentLen < minLen,
      minLen = currentLen;
      bestPos = pos;
      cands = currentCands;
      If[minLen <= 1, Break[]];
    ];
  , {pos, emptyPos}];
  
  If[minLen == 0, Return[$Failed]];
  
  {bestPos, cands}
];

backtrack[board_] := Module[{target, pos, cands, r, c},
  target = findBestCell[board];
  
  If[target === Null, Throw[board]];
  
  If[target === $Failed, Return[$Failed]];
  
  {pos, cands} = target;
  {r, c} = pos;
  
  Do[
    backtrack[ReplacePart[board, {r, c} -> val]];
  , {val, cands}];
  
  $Failed
];

solveOne[grid_] := Catch[backtrack[grid]];

solve[] := Module[{nCores, rawData, puzzles, solvedGrids},
  nCores = $ProcessorCount;
  
  LaunchKernels[nCores];
  DistributeDefinitions[getCandidates, findBestCell, backtrack, solveOne];
  
  rawData = Import["https://projecteuler.net/project/resources/p096_sudoku.txt", "Lines"];
  
  puzzles = Map[
    Function[chunk,
      Map[ToExpression[Characters[#]] &, Rest[chunk]]
    ],
    Partition[rawData, 10]
  ];
  
  solvedGrids = ParallelMap[solveOne, puzzles, Method -> "CoarsestGrained"];
  
  Total[
    Map[
      FromDigits[#[[1, 1 ;; 3]]] &,
      solvedGrids
    ]
  ]
];

solve[]