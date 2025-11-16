(* Problem: https://projecteuler.net/problem=7 *)
(* Find the n-th prime number. *)

(*
   Problem statement (Project Euler 7, paraphrased):
   By listing the first six prime numbers:
     2, 3, 5, 7, 11, 13,
   we can see that the 6th prime is 13.
   What is the 10001st prime number?

   The known answer for n = 10001 is:
     104743.

   ---------------------------------------------------------------------------
   Mathematical background: prime numbers and the n-th prime
   ---------------------------------------------------------------------------

   1) Prime numbers
      A prime number p is an integer greater than 1 whose only positive
      divisors are 1 and p itself.

      Examples:
        2, 3, 5, 7, 11, 13, 17, ...

      A composite number has at least one divisor d with 1 < d < n.

   2) The n-th prime
      If we list primes in increasing order:

        p_1 = 2
        p_2 = 3
        p_3 = 5
        p_4 = 7
        p_5 = 11
        p_6 = 13
        ...

      Then p_n denotes the n-th prime in this ordered sequence.

      The goal of the problem is to compute p_n for n = 10001.

   3) Concept of a prime-counting approach
      One straightforward way (mirroring the given Python code) to find
      the n-th prime is:

      - Start from an integer num = 1.
      - Maintain a counter count = 0 for how many primes we have seen.
      - Repeatedly:
          * Increment num by 1.
          * Test if num is prime.
          * If num is prime, increment count.
          * Stop when count reaches n.
      - The final num is then the n-th prime.

      This is essentially an incremental scan of the natural numbers,
      enumerating primes in order and stopping at the n-th one.

   4) Primality test
      In the Python solution, primality is tested via SymPy’s isprime().
      In Wolfram Language, primality testing is provided by the built-in
      predicate:

        PrimeQ[x]

      which returns True if x is prime, and False otherwise.

   ---------------------------------------------------------------------------
   Algorithm implemented here
   ---------------------------------------------------------------------------

   Given a positive integer n:

   1) Initialize:
        count = 0
        num   = 1

   2) While count < n:
        - Increase num by 1.
        - If PrimeQ[num] is True (num is prime),
            increment count by 1.

   3) When count == n, we exit the loop and return num.
      At this point, num is exactly the n-th prime p_n.

   Complexity:
   - This is a simple, conceptually clear approach.
   - Not asymptotically optimal (compared to sieves), but sufficient
     for n = 10001.

   The result for n = 10001 is known to be:
     104743.
*)

nthPrime[n_Integer?Positive] := Module[
  {
    count = 0,
    num   = 1
  },
  
  (* Increment num and count primes until we have found n of them. *)
  While[count < n,
    num++;
    If[PrimeQ[num],
      count++;
    ];
  ];
  
  (* When count == n, num is the n-th prime. *)
  num
]

nthPrime[10001]
