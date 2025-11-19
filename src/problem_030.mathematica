(* Problem: https://projecteuler.net/problem=30 *)
(* Digit fifth powers *)

(*
   Problem: Surprisingly there are only three numbers that can be written as the
   sum of fourth powers of their digits:
   
   1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴
   8208 = 8⁴ + 2⁴ + 0⁴ + 8⁴
   9474 = 9⁴ + 4⁴ + 7⁴ + 4⁴
   
   As 1 = 1⁴ is not a sum, it is not included.
   
   Find the sum of all the numbers that can be written as the sum of fifth powers
   of their digits.
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: We need to find all numbers n where n equals the sum
   of the fifth powers of its digits. For example, if n = abcd (a 4-digit number),
   we need: n = a⁵ + b⁵ + c⁵ + d⁵.
   
   Key question: What is the upper bound for our search?
   
   The maximum contribution per digit is 9⁵ = 59,049. For a d-digit number:
   - Minimum value: 10^(d-1) (smallest d-digit number)
   - Maximum digit sum: d × 9⁵ = d × 59,049
   
   For d = 6: minimum = 100,000, maximum sum = 6 × 59,049 = 354,294
   For d = 7: minimum = 1,000,000, maximum sum = 7 × 59,049 = 413,343
   
   Since 7-digit numbers start at 1,000,000 but the maximum possible sum is only
   413,343, no 7-digit number can equal the sum of fifth powers of its digits.
   
   Therefore, our upper bound is 6 × 9⁵ = 354,294. The Python code uses this
   reasoning with the general formula: limit = (power + 1) × 9^power, though
   6 × 9⁵ is a tighter bound for this specific problem.
   
   Algorithm:
   1. Set upper limit based on maximum possible digit power sum
   2. For each number from 2 to limit (excluding 1 as it's not a sum):
      a. Extract digits
      b. Compute sum of fifth powers of digits
      c. If equal to the number itself, add to result
   3. Return total sum
*)

power = 5;
limit = 9^power * 6;

sumOfPowersOfDigits[n_Integer, p_Integer] := 
  Total[IntegerDigits[n]^p]

digitFifthPowers[] := Module[
  {answer = 0},
  
  Do[
    If[i == sumOfPowersOfDigits[i, power],
      answer += i
    ],
    {i, 2, limit}
  ];
  
  answer
]

digitFifthPowers[]
