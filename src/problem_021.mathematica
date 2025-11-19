(* Problem: https://projecteuler.net/problem=21 *)
(* Sum of amicable numbers under 10000. *)

(*
   Problem statement (Project Euler 21, paraphrased):
   
   Let d(n) be defined as the sum of proper divisors of n (numbers less than n
   which divide evenly into n).
   
   If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
   and each of a and b are called amicable numbers.
   
   For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
   55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
   71 and 142; so d(284) = 220.
   
   Evaluate the sum of all the amicable numbers under 10000.

   ---------------------------------------------------------------------------
   Mathematical background: Amicable numbers and divisor functions
   ---------------------------------------------------------------------------

   1) Proper divisors:
      The proper divisors of a positive integer n are all positive divisors of n
      except n itself. Equivalently, they are divisors d where 1 ≤ d < n.
      
      Example: Proper divisors of 12 are {1, 2, 3, 4, 6}
      Note: 12 itself is NOT included

   2) Divisor sum function:
      Let d(n) denote the sum of proper divisors of n.
      This is related to the standard divisor function σ(n), which sums ALL
      divisors including n:
        σ(n) = sum of all divisors of n
        d(n) = σ(n) - n
      
      Example: σ(12) = 1 + 2 + 3 + 4 + 6 + 12 = 28
               d(12) = 28 - 12 = 16

   3) Amicable pairs:
      Two distinct integers a and b form an amicable pair if:
        d(a) = b  AND  d(b) = a  AND  a ≠ b
      
      The condition a ≠ b excludes perfect numbers (where d(n) = n).
      
      Classical example: (220, 284)
        d(220) = 1+2+4+5+10+11+20+22+44+55+110 = 284
        d(284) = 1+2+4+71+142 = 220
      
      These numbers have been known since antiquity and were studied by
      Pythagorean mathematicians.

   4) Finding proper divisors efficiently:
      Naive approach: Test every integer from 1 to n-1
      Time complexity: O(n)
      
      Optimized approach: Test only up to √n
      - For each divisor i where i divides n and i ≤ √n:
        * i is a divisor
        * n/i is also a divisor (unless i = √n, in which case we count it once)
      - Always include 1 as a divisor
      - Never include n itself (proper divisors condition)
      Time complexity: O(√n)

   5) Algorithm for finding all amicable numbers under N:
      For each number a from 1 to N-1:
        1. Compute b = d(a) (sum of proper divisors of a)
        2. Check if a ≠ b (to exclude perfect numbers)
        3. Compute d(b) (sum of proper divisors of b)
        4. If d(b) = a, then a is amicable
        5. Add a to the running sum
      
      Note: This algorithm counts each amicable pair twice if both numbers
      are under N, but that's correct—we want the sum of all amicable numbers,
      not the sum of amicable pairs.

   6) Wolfram Language advantages:
      - DivisorSigma[1, n] computes σ(n) efficiently
      - d(n) = DivisorSigma[1, n] - n
      - Select and Count provide functional filtering
      - Built-in divisor functions use optimized number-theoretic algorithms

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

sumOfProperDivisors[n_Integer?Positive] := Module[
  {
    divisors
  },
  
  (* Find all divisors by checking up to √n *)
  divisors = {1}; (* Always include 1 *)
  
  Do[
    If[Mod[n, i] == 0,
      (* i is a divisor, add it *)
      AppendTo[divisors, i];
      (* Also add n/i if it's different from i and not equal to n *)
      If[i != n/i && n/i != n,
        AppendTo[divisors, n/i]
      ]
    ],
    {i, 2, Floor[Sqrt[n]]}
  ];
  
  Total[divisors]
]

(* More efficient version using DivisorSigma *)
sumOfProperDivisorsEfficient[n_Integer?Positive] :=
  DivisorSigma[1, n] - n

amicableNumbersSum[limit_Integer?Positive] := Module[
  {
    amicableSum = 0,
    a, b
  },
  
  (* Check each number from 1 to limit-1 *)
  Do[
    (* Compute b = d(a), the sum of proper divisors of a *)
    b = sumOfProperDivisorsEfficient[a];
    
    (* Check if a and b form an amicable pair:
       - a ≠ b (exclude perfect numbers)
       - d(b) = a (the amicable condition) *)
    If[a != b && sumOfProperDivisorsEfficient[b] == a,
      amicableSum += a
    ],
    {a, 1, limit - 1}
  ];
  
  amicableSum
]

(* Functional version using Select *)
amicableNumbersSumFunctional[limit_Integer?Positive] := Module[
  {
    d = sumOfProperDivisorsEfficient,
    isAmicable
  },
  
  (* A number a is amicable if d(d(a)) = a and d(a) ≠ a *)
  isAmicable[a_] := Module[{b = d[a]}, a != b && d[b] == a];
  
  (* Sum all amicable numbers in range *)
  Total[Select[Range[1, limit - 1], isAmicable]]
]

(* Calculate the sum of all amicable numbers under 10000 *)
amicableNumbersSum[10000]
