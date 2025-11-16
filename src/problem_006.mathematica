(* Problem: https://projecteuler.net/problem=6 *)
(* Difference between the square of the sum and the sum of the squares
   of the first n natural numbers. *)

(*
   Problem statement (Project Euler 6, paraphrased):
   The sum of the squares of the first ten natural numbers is

     1^2 + 2^2 + ... + 10^2 = 385.

   The square of the sum of the first ten natural numbers is

     (1 + 2 + ... + 10)^2 = 55^2 = 3025.

   Hence, the difference between the sum of the squares of the first ten
   natural numbers and the square of the sum is

     3025 - 385 = 2640.

   The task is:
     Find the difference between the sum of the squares of the first
     one hundred natural numbers and the square of the sum. :contentReference[oaicite:0]{index=0}


   ---------------------------------------------------------------------------
   Mathematical definitions
   ---------------------------------------------------------------------------

   For a given positive integer n, define:

   1) Sum of the squares:
        S_sq(n) = 1^2 + 2^2 + 3^2 + ... + n^2.

   2) Square of the sum:
        S_sum(n) = (1 + 2 + 3 + ... + n)^2.

   We are interested in the difference
        D(n) = S_sum(n) - S_sq(n).

   In the Python code, this is computed directly by:
     - summing i^2 for i from 1 to n,
     - summing i for i from 1 to n, squaring that sum,
     - and then taking the difference.

   ---------------------------------------------------------------------------
   Closed-form formulas (not required by the code, but useful conceptually)
   ---------------------------------------------------------------------------

   There are well-known closed forms:

   1) Sum of the first n natural numbers (triangular number):

        T(n) = 1 + 2 + ... + n = n (n + 1) / 2. :contentReference[oaicite:1]{index=1}

      Then the square of the sum is:

        S_sum(n) = [T(n)]^2
                  = [n (n + 1) / 2]^2.

   2) Sum of the squares of the first n natural numbers
      (square pyramidal number):

        S_sq(n) = 1^2 + 2^2 + ... + n^2
                = n (n + 1) (2n + 1) / 6. :contentReference[oaicite:2]{index=2}

   Substituting, we get:

        D(n) = S_sum(n) - S_sq(n)
             = [n (n + 1) / 2]^2 - [n (n + 1) (2n + 1) / 6].

   This closed form is often used in analytical solutions, but the
   original Python implementation uses straightforward finite sums,
   which we will mirror conceptually in Wolfram Language.

   ---------------------------------------------------------------------------
   Algorithm in the given Python solution
   ---------------------------------------------------------------------------

   Given n:

   1) Compute
        sum_of_squares = Σ_{i=1}^n i^2.

   2) Compute
        sum = Σ_{i=1}^n i,
      then
        square_of_sum = (sum)^2.

   3) Return
        square_of_sum - sum_of_squares.

   This is exactly what we implement in Wolfram Language below, using
   the built-in Sum function for clarity and precision.

   For n = 100, the known answer to Project Euler problem 6 is:

        D(100) = 25,164,150. :contentReference[oaicite:3]{index=3}
*)

sumSquareDifference[n_Integer] := Module[
  {
    sumOfSquares,
    squareOfSum
  },
  
  (* Sum of the squares: 1^2 + 2^2 + ... + n^2 *)
  sumOfSquares = Sum[i^2, {i, 1, n}];
  
  (* Square of the sum: (1 + 2 + ... + n)^2 *)
  squareOfSum = (Sum[i, {i, 1, n}])^2;
  
  (* Difference: square of the sum minus sum of the squares *)
  squareOfSum - sumOfSquares
]

(* Example: difference for the first 100 natural numbers.
   The expected answer for Project Euler problem 6 is 25164150. *)
sumSquareDifference[100]
