(* Problem: https://projecteuler.net/problem=27 *)
(* Quadratic primes *)

(*
   Problem: Euler discovered the remarkable quadratic formula n² + n + 41, which
   produces 40 primes for consecutive integer values 0 ≤ n ≤ 39. However, when
   n = 40, the value is divisible by 41, and when n = 41, it's divisible by 41².
   
   The formula n² - 79n + 1601 produces 80 primes for 0 ≤ n ≤ 79.
   
   Considering quadratics of the form n² + an + b, where |a| < 1000 and |b| ≤ 1000,
   find the product of the coefficients a and b for the quadratic expression that
   produces the maximum number of primes for consecutive values of n, starting
   with n = 0.
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: For a quadratic n² + an + b to produce primes starting
   from n = 0, we have immediate constraints:
   
   1) At n = 0: f(0) = b must be prime, so b must be positive and prime
   2) At n = 1: f(1) = 1 + a + b must be prime (and positive), so a + b ≥ 1
   3) For any prime produced, f(n) > 0, which constrains the range of valid a
   
   However, the Python solution uses a brute-force search over all |a| < 1000
   and |b| ≤ 1000 without these optimizations. While less efficient, this approach
   is straightforward and completes quickly enough for the given bounds.
   
   Algorithm:
   1. For each pair (a, b) with -999 ≤ a ≤ 999 and -1000 ≤ b ≤ 1000:
      a. Starting from n = 0, compute f(n) = n² + an + b
      b. Count consecutive n values for which f(n) is prime
      c. Stop when f(n) is not prime
   2. Track the (a, b) pair that produces the longest sequence
   3. Return the product a × b
   
   The quadratic_primes function tests consecutive values of n starting from 0
   until the formula produces a non-prime, returning the count of primes generated.
*)

quadraticPrimes[a_, b_] := Module[
  {n = 0},
  
  While[PrimeQ[n^2 + a*n + b],
    n++
  ];
  
  n
]

findQuadraticCoefficients[] := Module[
  {maxN = 0, answer = 0, a, b, n},
  
  Do[
    n = quadraticPrimes[a, b];
    If[n > maxN,
      maxN = n;
      answer = a * b
    ],
    {a, -999, 999},
    {b, -1000, 1000}
  ];
  
  answer
]

findQuadraticCoefficients[]
