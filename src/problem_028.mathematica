(* Problem: https://projecteuler.net/problem=28 *)
(* Number spiral diagonals *)

(*
   Problem: Starting with the number 1 and moving to the right in a clockwise
   direction, a 5×5 spiral is formed as follows:
   
   21 22 23 24 25
   20  7  8  9 10
   19  6  1  2 11
   18  5  4  3 12
   17 16 15 14 13
   
   The sum of the numbers on the diagonals is 101.
   
   What is the sum of the numbers on the diagonals in a 1001×1001 spiral formed
   in the same way?
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: The spiral is constructed in layers. The center is 1.
   Each subsequent layer adds a square frame around the previous layers.
   
   Observation: The diagonal elements follow a pattern based on their layer.
   
   For a layer k (where k=1 is the first layer around center 1):
   - The layer forms a square of side length (2k+1)
   - The four corners of this layer lie on the diagonals
   - The step size between consecutive corners is 2k
   
   Starting from the center (value 1), as we move outward:
   - Layer 1: side length 3, step 2, corners at positions: 3, 5, 7, 9
   - Layer 2: side length 5, step 4, corners at positions: 13, 17, 21, 25
   - Layer 3: side length 7, step 6, corners at positions: 31, 37, 43, 49
   
   Pattern for layer k:
   - Last number of previous layer: (2k-1)²
   - Step between corners: 2k
   - Four diagonal values: (2k-1)² + 2k, (2k-1)² + 4k, (2k-1)² + 6k, (2k-1)² + 8k
   
   For an n×n spiral (n odd), we have (n-1)/2 layers plus the center.
   
   Algorithm:
   1. Start with total = 1 (center value) and currentNumber = 1
   2. For each layer k from 1 to (n-1)/2:
      a. Step size = 2k
      b. Add four corner values, each incremented by the step size
      c. Update currentNumber and accumulate sum
   3. Return total sum
*)

spiralDiagonalsSum[size_Integer] := Module[
  {total, currentNumber, layer, step},
  
  total = 1;
  currentNumber = 1;
  
  Do[
    step = 2 * layer;
    Do[
      currentNumber += step;
      total += currentNumber,
      {4}
    ],
    {layer, 1, Quotient[size, 2]}
  ];
  
  total
]

spiralDiagonalsSum[1001]
