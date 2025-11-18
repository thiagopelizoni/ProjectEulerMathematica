(* Problem: https://projecteuler.net/problem=20 *)
(* Sum of digits in a factorial. *)

(*
   Problem statement (Project Euler 20, paraphrased):
   
   n! means n × (n-1) × ... × 3 × 2 × 1
   
   For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
   and the sum of the digits in the number 10! is
   3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
   
   Find the sum of the digits in the number 100!

   ---------------------------------------------------------------------------
   Mathematical background: Factorials and digit sums
   ---------------------------------------------------------------------------

   1) Factorial definition:
      For a non-negative integer n, the factorial n! is defined as:
        n! = n × (n-1) × (n-2) × ... × 2 × 1
      
      With the special case:
        0! = 1 (by convention)
      
      Factorials grow extremely rapidly. For instance:
        10! = 3,628,800
        20! = 2,432,902,008,176,640,000
        100! has 158 digits

   2) The magnitude of 100!:
      Using Stirling's approximation:
        n! ≈ √(2πn) × (n/e)^n
      
      For n = 100:
        100! ≈ 9.33 × 10^157
      
      This is a number with 158 decimal digits, far exceeding the capacity
      of standard floating-point arithmetic. We need arbitrary-precision
      integer arithmetic.

   3) Arbitrary-precision arithmetic:
      Both Python (built-in) and Wolfram Language (built-in) handle
      arbitrarily large integers seamlessly. Computing 100! exactly is
      straightforward with these tools.

   4) Problem objective:
      Compute 100!, extract all its decimal digits, and sum them.
      
      This is similar to Problem 16 (sum of digits of 2^1000), but now
      we're working with a factorial instead of a power.

   5) Algorithm steps:
      Step 1: Compute 100! using arbitrary-precision arithmetic
      Step 2: Extract the individual decimal digits
      Step 3: Sum the digits

   6) Verification with smaller example (10!):
      10! = 3,628,800
      Digits: {3, 6, 2, 8, 8, 0, 0}
      Sum: 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27 ✓

   7) Implementation approach:
      Wolfram Language provides:
      - Factorial[n] or n! computes the factorial
      - IntegerDigits[n] extracts digits as a list
      - Total[list] sums the elements
      
      The solution is remarkably concise: Total[IntegerDigits[100!]]

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

factorialDigitSum[n_Integer?NonNegative] := Module[
  {
    factorialValue, digits
  },
  
  (* Step 1: Compute n! using arbitrary-precision arithmetic.
     Wolfram Language's Factorial function handles large integers exactly. *)
  factorialValue = Factorial[n];
  
  (* Step 2: Extract the individual decimal digits.
     IntegerDigits returns a list of digits in base 10. *)
  digits = IntegerDigits[factorialValue];
  
  (* Step 3: Sum all the digits. *)
  Total[digits]
]

(* More concise functional version *)
factorialDigitSumFunctional[n_Integer?NonNegative] :=
  Total[IntegerDigits[Factorial[n]]]

(* Most concise version using postfix notation and factorial operator *)
factorialDigitSumComposed[n_Integer?NonNegative] :=
  Total@IntegerDigits[n!]

(* Calculate the sum of digits in 100! *)
factorialDigitSum[100]