(* Problem: https://projecteuler.net/problem=23 *)
(* Sum of all positive integers which cannot be written as the sum of two abundant numbers. *)

(*
   Problem statement (Project Euler 23, paraphrased):
   
   A perfect number is a number for which the sum of its proper divisors is
   exactly equal to the number. For example, the sum of the proper divisors
   of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
   
   A number n is called deficient if the sum of its proper divisors is less than n
   and it is called abundant if this sum exceeds n.
   
   As 12 is the smallest abundant number (1 + 2 + 3 + 4 + 6 = 16), the smallest
   number that can be written as the sum of two abundant numbers is 24. By
   mathematical analysis, it can be shown that all integers greater than 28123
   can be written as the sum of two abundant numbers. However, this upper limit
   cannot be reduced any further by analysis even though it is known that the
   greatest number that cannot be expressed as the sum of two abundant numbers
   is less than this limit.
   
   Find the sum of all the positive integers which cannot be written as the sum
   of two abundant numbers.

   ---------------------------------------------------------------------------
   Mathematical background: Number classification and divisor sums
   ---------------------------------------------------------------------------

   1) Proper divisors and their sum:
      The proper divisors of n are all positive divisors of n except n itself.
      Let σ(n) denote the sum of all divisors of n (including n).
      Let d(n) denote the sum of proper divisors: d(n) = σ(n) - n
      
      Example: n = 12
        All divisors: {1, 2, 3, 4, 6, 12}
        Proper divisors: {1, 2, 3, 4, 6}
        σ(12) = 1 + 2 + 3 + 4 + 6 + 12 = 28
        d(12) = 28 - 12 = 16

   2) Classification by divisor sum:
      - Perfect: d(n) = n (sum of proper divisors equals n)
        Examples: 6, 28, 496, 8128
      - Deficient: d(n) < n (sum is less than n)
        Examples: All primes, many composites
      - Abundant: d(n) > n (sum exceeds n)
        Examples: 12, 18, 20, 24, 30, ...

   3) Abundant numbers:
      12 is the smallest abundant number:
        d(12) = 1 + 2 + 3 + 4 + 6 = 16 > 12
      
      All multiples of perfect numbers (except the first) are abundant.
      All multiples of abundant numbers are abundant.
      Every integer greater than 20161 can be expressed as the sum of
      two abundant numbers.

   4) The 28123 bound:
      Mathematical analysis proves that every integer greater than 28123
      can be written as the sum of two abundant numbers. This is our
      upper limit for searching.

   5) Algorithm strategy:
      Step 1: Find all abundant numbers up to 28123
      Step 2: Generate all possible sums of two abundant numbers (≤ 28123)
      Step 3: Find all integers from 1 to 28123 that are NOT such sums
      Step 4: Sum these integers

   6) Implementation details:
      - Computing d(n): Find divisors up to √n, include both i and n/i
      - Generating abundant numbers: Filter integers 1 to 28123 by abundance
      - Computing sums: Nested loop over all pairs of abundant numbers
      - Use a set/hash table for O(1) lookup of abundant sums
      - Complement: Sum integers not in the abundant sums set

   7) Optimization considerations:
      - Use DivisorSigma for efficient divisor sum computation
      - Use Complement to find non-abundant-sum numbers
      - Wolfram's built-in set operations are highly optimized

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

(* Upper limit from mathematical analysis *)
limit = 28123;

(* Check if a number is abundant *)
isAbundant[n_Integer?Positive] := Module[
  {
    properDivisorSum
  },
  
  (* Sum of proper divisors = σ(n) - n *)
  properDivisorSum = DivisorSigma[1, n] - n;
  
  (* Number is abundant if sum of proper divisors exceeds n *)
  properDivisorSum > n
]

nonAbundantSumsTotal[] := Module[
  {
    abundantNumbers, abundantSums, nonAbundantNumbers
  },
  
  (* Step 1: Find all abundant numbers up to limit *)
  abundantNumbers = Select[Range[1, limit], isAbundant];
  
  (* Step 2: Generate all sums of two abundant numbers efficiently *)
  (* Use Outer to create all pairwise sums, then filter and remove duplicates *)
  abundantSums = Union[
    Select[
      Flatten[Outer[Plus, abundantNumbers, abundantNumbers]],
      # <= limit&
    ]
  ];
  
  (* Step 3: Find all numbers that are NOT sums of two abundant numbers *)
  nonAbundantNumbers = Complement[Range[1, limit], abundantSums];
  
  (* Step 4: Sum all non-abundant-sum numbers *)
  Total[nonAbundantNumbers]
]

(* More functional and efficient version *)
nonAbundantSumsTotalFunctional[] := Module[
  {
    abundantNumbers, abundantSums, nonAbundantNumbers
  },
  
  (* Find all abundant numbers *)
  abundantNumbers = Select[Range[limit], 
    DivisorSigma[1, #] - # > #&
  ];
  
  (* Generate all sums using outer product and flattening *)
  abundantSums = DeleteDuplicates[
    Select[
      Flatten[Outer[Plus, abundantNumbers, abundantNumbers]],
      # <= limit&
    ]
  ];
  
  (* Find complement and sum *)
  Total[Complement[Range[limit], abundantSums]]
]

(* Most concise version using built-in operations *)
nonAbundantSumsTotalComposed[] := Module[
  {
    abundant, sums
  },
  
  abundant = Select[Range[28123], DivisorSigma[1, #] > 2*#&];
  sums = DeleteDuplicates@Select[
    Flatten@Outer[Plus, abundant, abundant],
    # <= 28123&
  ];
  Total@Complement[Range[28123], sums]
]

(* Calculate the answer *)
nonAbundantSumsTotal[]
