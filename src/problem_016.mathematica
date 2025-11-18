(* Problem: https://projecteuler.net/problem=16 *)
(* Sum of the digits of a power. *)

(*
   Problem statement (Project Euler 16, paraphrased):
   
   2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
   
   What is the sum of the digits of the number 2^1000?
   
   The known correct answer is:
     1366

   ---------------------------------------------------------------------------
   Mathematical background: Arbitrary-precision arithmetic and digit sum
   ---------------------------------------------------------------------------

   1) Problem setup:
      We need to:
      a) Compute 2^1000 (a very large number with about 302 decimal digits)
      b) Extract all individual digits of this number
      c) Sum these digits

   2) Why this is non-trivial in most languages:
      2^1000 is an enormous number:
        2^1000 ≈ 1.07 × 10^301
      
      Most programming languages use fixed-precision arithmetic:
      - Standard 64-bit integers can only represent up to about 2^63 ≈ 9 × 10^18
      - Standard 64-bit floating point loses precision beyond 15-17 digits
      
      Therefore, computing 2^1000 exactly requires arbitrary-precision
      arithmetic (also called "big integers" or "unlimited precision integers").

   3) Arbitrary-precision arithmetic:
      Languages like Python (built-in) and Wolfram Language (built-in) support
      arbitrary-precision integers, which can represent integers of any size
      limited only by available memory.
      
      In Wolfram Language, all integers are automatically arbitrary-precision.
      There is no overflow - calculations like 2^1000 work seamlessly.

   4) Extracting digits:
      To sum the digits of a number n, we need to extract each digit.
      
      Method 1 (String conversion):
        Convert n to a string, split into characters, convert each back to int
        Python: sum(map(int, str(number)))
      
      Method 2 (Mathematical):
        Repeatedly extract the last digit using modulo 10, then divide by 10
        While more "pure" mathematically, string conversion is simpler
      
      Method 3 (Built-in, Wolfram Language):
        IntegerDigits[n] returns a list of digits directly
        Example: IntegerDigits[32768] gives {3, 2, 7, 6, 8}

   5) Algorithm steps:
      Step 1: Compute 2^1000 using arbitrary-precision arithmetic
      Step 2: Extract the digits using IntegerDigits
      Step 3: Sum the digits using Total

   6) Example verification (2^15):
      2^15 = 32768
      Digits: {3, 2, 7, 6, 8}
      Sum: 3 + 2 + 7 + 6 + 8 = 26 ✓

   7) Wolfram Language advantages:
      - Built-in arbitrary-precision arithmetic (no special libraries needed)
      - IntegerDigits function elegantly extracts digits
      - Total function sums list elements
      - Composition with @ operator creates concise, readable code

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

sumOfDigits[base_Integer, power_Integer?NonNegative] := Module[
  {
    number, digits
  },
  
  (* Step 1: Compute base^power using arbitrary-precision arithmetic.
     In Wolfram Language, this automatically handles arbitrarily large integers. *)
  number = base^power;
  
  (* Step 2: Extract the individual digits of the number.
     IntegerDigits[n] returns a list of digits in base 10.
     Example: IntegerDigits[32768] returns {3, 2, 7, 6, 8} *)
  digits = IntegerDigits[number];
  
  (* Step 3: Sum all the digits.
     Total efficiently sums all elements in the list. *)
  Total[digits]
]

(* More concise functional version using function composition:
   This is idiomatic Wolfram Language style. *)
sumOfDigitsFunctional[base_Integer, power_Integer?NonNegative] :=
  Total[IntegerDigits[base^power]]

(* Or even more concise with the composition operator:
   Total@IntegerDigits@(base^power) reads right-to-left:
   compute base^power, extract digits, sum them *)
sumOfDigitsComposed[base_Integer, power_Integer?NonNegative] :=
  Total@IntegerDigits[base^power]

(* Calculate the sum of the digits of 2^1000. *)
sumOfDigits[2, 1000]
