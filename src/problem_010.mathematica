(* Problem: https://projecteuler.net/problem=10 *)
(* Summation of all prime numbers strictly less than a given upper bound. *)

(*
   Problem statement (Project Euler 10, paraphrased):
   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   Find the sum of all the primes below two million.

   The known correct answer for n = 2,000,000 is:
     142913828922.

   ---------------------------------------------------------------------------
   Mathematical background: Sieve of Eratosthenes
   ---------------------------------------------------------------------------

   1) Goal:
      For a given integer n > 2, we wish to identify all primes p with 2 <= p < n
      and compute their sum.

   2) Prime definition:
      A prime number is an integer greater than 1 that has no positive divisors
      other than 1 and itself.

   3) Sieve of Eratosthenes idea:
      - We create a boolean array 'sieve' that represents the integers
        0, 1, 2, ..., n-1.
      - Initially, we consider all entries from 2 to n-1 to be "prime candidates".
      - Then, for each i from 2 up to floor(sqrt(n)):
          * If i is still marked as a prime candidate,
            we mark all multiples of i greater than or equal to i^2
            as composite (not prime).
      - At the end of this process, the remaining numbers marked as candidates
        are exactly the prime numbers up to n-1.

      Why start at i^2?
      - Any composite multiple of i less than i^2 can be written as i * k
        with k < i, and will have been marked when we processed the smaller
        factor k earlier.

   4) Restricting to primes below n:
      Our array indices run from 1 to n, corresponding to integers 0 to n-1.
      We want all primes p with 2 <= p < n, so we sum all indices
      that remain marked as prime.

   5) Complexity:
      The sieve runs in O(n log log n) time and uses O(n) memory, which is
      efficient enough for n = 2,000,000.

   6) Vectorization in Wolfram Language:
      Unlike procedural For loops, Wolfram Language excels at vectorized
      operations. We use:
      - Range to generate sequences of multiples
      - Part assignment with ranges to mark composites
      - Pick to extract primes
      - Total to sum them
      This approach is orders of magnitude faster than iterative loops.

   We now implement this algorithm in optimized Wolfram Language.
*)

sumOfPrimesBelow[n_Integer?Positive] := Module[
  {
    sieve,  (* boolean array marking prime candidates *)
    limit, i
  },

  (* Create an array of length n where sieve[[i+1]] corresponds to integer i.
     Initially, mark all entries as True (candidate primes). *)
  sieve = ConstantArray[True, n];

  (* 0 and 1 are not prime, so mark them False.
     sieve[[1]] represents 0, sieve[[2]] represents 1. *)
  sieve[[1]] = False;
  sieve[[2]] = False;

  (* We only need to consider potential factors i up to floor(sqrt(n)). *)
  limit = Floor[Sqrt[n]];

  (* Sieve of Eratosthenes core loop.
     For each prime i, mark all its multiples as composite. *)
  Do[
    If[sieve[[i + 1]],
      (* i is still considered prime; mark all multiples of i starting at i^2
         as composite using vectorized assignment.
         Range[i*i, n-1, i] generates the sequence: i^2, i^2+i, i^2+2i, ...
         We add 1 to convert from integer values to array indices. *)
      sieve[[Range[i*i, n - 1, i] + 1]] = False;
    ],
    {i, 2, limit}
  ];

  (* Extract all prime numbers: Pick selects elements from Range[0, n-1]
     where the corresponding sieve entry is True.
     Then Total computes their sum efficiently. *)
  Total[Pick[Range[0, n - 1], sieve]]
]

(* Example: sum of all primes below 2,000,000.
   The expected Project Euler problem 10 answer is 142913828922. *)
sumOfPrimesBelow[2000000]
