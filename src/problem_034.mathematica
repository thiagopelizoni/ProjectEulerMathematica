(* Project Euler Problem 34: Digit Factorials

   Problem Description:
   Find the sum of all numbers which are equal to the sum of the factorial of their digits.
   Note: as 1! = 1 and 2! = 2 are not sums, they are not included.

   Mathematical Solution:
   1. Domain Analysis (Upper Bound Derivation):
      Let n be a positive integer with d digits.
      The minimum value of n is 10^(d-1).
      The maximum possible sum of the factorials of the digits is d * 9!.
      We seek the crossing point where the magnitude of the number exceeds the maximum possible sum:
      10^(d-1) > d * 9!
      For d = 8: 10^7 = 10,000,000 > 8 * 362,880 = 2,903,040.
      Thus, no solution can exist for d >= 8. The search space is bounded by the maximum value
      for d = 7, which is effectively bounded by 7 * 9! = 2,540,160.

   2. Algorithm:
      Define a predicate function P(x) such that x == Sum(digits(x)!).
      Iterate through the domain D = [10, 2,540,160].
      Accumulate the sum of all x in D satisfying P(x).
*)

FactorialSumQ[n_Integer] := n == Total[Factorial /@ IntegerDigits[n]]

upperLimit = 7 * Factorial[9];

specialNumbers = Select[Range[10, upperLimit], FactorialSumQ];

Total[specialNumbers]