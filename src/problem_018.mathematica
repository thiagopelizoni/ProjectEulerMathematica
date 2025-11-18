(* Problem: https://projecteuler.net/problem=18 *)
(* Maximum path sum through a triangle. *)

(*
   Problem statement (Project Euler 18, paraphrased):
   
   By starting at the top of the triangle below and moving to adjacent numbers
   on the row below, the maximum total from top to bottom is 23.
   
        3
       7 4
      2 4 6
     8 5 9 3
   
   That is, 3 + 7 + 4 + 9 = 23.
   
   Find the maximum total from top to bottom of the given 15-row triangle.
   
   NOTE: As there are only 16384 routes through the triangle, it is possible
   to solve this problem by trying every route. However, Problem 67, is the
   same challenge with a triangle containing one-hundred rows; it cannot be
   solved by brute force, and requires a clever method! ;o)

   ---------------------------------------------------------------------------
   Mathematical background: Dynamic Programming on Triangular Arrays
   ---------------------------------------------------------------------------

   1) Problem setup:
      We have a triangular array of numbers:
      - Row 0 (top) has 1 element
      - Row 1 has 2 elements
      - Row k has k+1 elements
      - We have n rows total (0 to n-1)
      
      Movement rule: From position (row, col), we can move to either:
      - (row+1, col) - directly below
      - (row+1, col+1) - diagonally below-right
      
      Goal: Find the path from top to bottom that maximizes the sum of elements.

   2) Naive approach: Exhaustive search
      There are 2^(n-1) possible paths (at each of n-1 steps, we choose left or right).
      For n=15, this is 2^14 = 16,384 paths - feasible but inefficient.
      For n=100 (Problem 67), this is 2^99 ≈ 6×10^29 paths - completely infeasible!

   3) Dynamic Programming approach:
      Key insight: Many subproblems are solved repeatedly in the naive approach.
      
      Define: maxSum[row][col] = maximum sum achievable from position (row, col)
                                  to the bottom of the triangle
      
      Recurrence relation:
        maxSum[row][col] = triangle[row][col] + 
                           max(maxSum[row+1][col], maxSum[row+1][col+1])
      
      Base case:
        maxSum[n-1][col] = triangle[n-1][col] (bottom row elements)
      
      The answer is maxSum[0][0] (starting from the top).

   4) Bottom-up computation:
      Instead of computing top-down with memoization, we can compute bottom-up:
      - Start with the second-to-last row
      - For each element, add the maximum of its two children below
      - Move up row by row
      - After processing all rows, the top element contains the answer
      
      This approach modifies the triangle in-place, which is memory-efficient.

   5) Complexity analysis:
      Time: O(n²) - we visit each element once, and there are n(n+1)/2 elements
      Space: O(1) if we modify in-place, or O(n²) if we create a copy
      
      For n=100, this is only ~10,000 operations - extremely fast!

   6) Example walkthrough (small triangle):
      Original:
           3
          7 4
         2 4 6
        8 5 9 3
      
      Step 1: Process row 2 (second-to-last)
        2 += max(8,5) = 10
        4 += max(5,9) = 13
        6 += max(9,3) = 15
      Result:
           3
          7 4
        10 13 15
        8 5 9 3
      
      Step 2: Process row 1
        7 += max(10,13) = 20
        4 += max(13,15) = 19
      Result:
           3
         20 19
        10 13 15
        8 5 9 3
      
      Step 3: Process row 0 (top)
        3 += max(20,19) = 23
      Result:
          23
         20 19
        10 13 15
        8 5 9 3
      
      Answer: 23 ✓

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

(* Triangle data from Project Euler Problem 18 *)
triangle = {
  {75},
  {95, 64},
  {17, 47, 82},
  {18, 35, 87, 10},
  {20, 4, 82, 47, 65},
  {19, 1, 23, 75, 3, 34},
  {88, 2, 77, 73, 7, 63, 67},
  {99, 65, 4, 28, 6, 16, 70, 92},
  {41, 41, 26, 56, 83, 40, 80, 70, 33},
  {41, 48, 72, 33, 47, 32, 37, 16, 94, 29},
  {53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14},
  {70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57},
  {91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48},
  {63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31},
  {4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23}
};

maximumPathSum[tri_List] := Module[
  {
    workingTriangle, numRows, row, col
  },
  
  (* Create a working copy to avoid modifying the input *)
  workingTriangle = tri;
  numRows = Length[workingTriangle];
  
  (* Dynamic programming: process from second-to-last row upward *)
  For[row = numRows - 1, row >= 1, row--,
    (* For each element in the current row *)
    For[col = 1, col <= Length[workingTriangle[[row]]], col++,
      (* Add the maximum of the two adjacent elements from the row below.
         workingTriangle[[row, col]] is the current element.
         workingTriangle[[row+1, col]] is directly below.
         workingTriangle[[row+1, col+1]] is diagonally below-right. *)
      workingTriangle[[row, col]] += Max[
        workingTriangle[[row + 1, col]],
        workingTriangle[[row + 1, col + 1]]
      ];
    ];
  ];
  
  (* After processing all rows, the top element contains the maximum sum *)
  workingTriangle[[1, 1]]
]

(* More functional version using MapIndexed and Fold *)
maximumPathSumFunctional[tri_List] := Module[
  {
    workingTriangle = tri,
    numRows = Length[tri]
  },
  
  (* Process each row from second-to-last to first *)
  Do[
    workingTriangle[[row]] = MapIndexed[
      (* For each element at position col, add max of two children below *)
      #1 + Max[workingTriangle[[row + 1, #2[[1]]]], 
               workingTriangle[[row + 1, #2[[1]] + 1]]]&,
      workingTriangle[[row]]
    ],
    {row, numRows - 1, 1, -1}
  ];
  
  (* Return the top element *)
  workingTriangle[[1, 1]]
]

(* Calculate the maximum path sum for the given triangle *)
maximumPathSum[triangle]
