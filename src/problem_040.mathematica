(* Project Euler Problem 40: Champernowne's Constant

   Problem Description:
   Champernowne's constant C_10 is an irrational decimal fraction created by concatenating the 
   positive integers: 0.123456789101112131415...
   Let d_n represent the n-th digit of the fractional part. The objective is to evaluate the 
   expression: d_1 * d_10 * d_100 * d_1000 * d_10000 * d_100000 * d_1000000.

   Mathematical Solution:
   1. Sequence Construction:
      We define the sequence S as the concatenation of the decimal digits of integers i for i >= 1.
      S = {1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 0, 1, 1, ...}
      
   2. Dimensionality Analysis:
      We require the digit at index 1,000,000.
      The cumulative digit count for integers up to k digits is given by sum(9 * 10^(j-1) * j).
      1-digit numbers: 9 digits (cumulative 9)
      2-digit numbers: 180 digits (cumulative 189)
      ...
      5-digit numbers: 450,000 digits (cumulative 488,889)
      6-digit numbers: 5,400,000 digits (cumulative > 1,000,000)
      Therefore, generating integers up to 200,000 (approx 1.2 million digits) is sufficient 
      to cover the domain.

   3. Computation:
      We construct the digit list D explicitly and compute the product of the elements at indices 
      10^k for k in {0, ..., 6}.
*)

indices = Power[10, Range[0, 6]];

digits = Flatten[IntegerDigits /@ Range[200000]];

Times @@ digits[[indices]]