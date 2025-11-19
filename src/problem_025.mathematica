(* Problem: https://projecteuler.net/problem=25 *)
(* Index of the first Fibonacci number with 1000 digits *)

(*
   Problem: The Fibonacci sequence is defined by the recurrence relation:
   F_n = F_{n-1} + F_{n-2}, where F_1 = 1 and F_2 = 1.
   
   The 12th term, F_12, is the first term to contain three digits.
   What is the index of the first term in the Fibonacci sequence to contain
   1000 digits?
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: The Fibonacci sequence grows exponentially, approximately
   as φ^n/√5, where φ = (1+√5)/2 ≈ 1.618 is the golden ratio. A Fibonacci number
   F_n has approximately n·log₁₀(φ) - log₁₀(√5) decimal digits.
   
   However, rather than using this formula to estimate the index, we directly
   compute Fibonacci numbers iteratively, counting digits until we find the first
   with at least 1000 digits.
   
   The solution:
   1. Initialize F_1 = 1, F_2 = 1, index = 2
   2. Iteratively compute F_{n+1} = F_n + F_{n-1}
   3. After each computation, check the digit count of the new Fibonacci number
   4. When we find a Fibonacci number with ≥ 1000 digits, return its index
   
   Key observations:
   - Fibonacci numbers grow rapidly, so we reach 1000 digits relatively quickly
   - Using arbitrary-precision arithmetic (built into Python and Wolfram) allows
     exact computation without overflow
   - IntegerLength[n, 10] gives the number of decimal digits in n
   - We use a While loop to iterate until the condition is met
*)

targetDigits = 1000;

findFibonacciIndex[digitCount_Integer] := Module[
  {a = 1, b = 1, index = 2},
  
  While[IntegerLength[b, 10] < digitCount,
    {a, b} = {b, a + b};
    index++
  ];
  
  index
]

findFibonacciIndex[targetDigits]
