(* Problem: https://projecteuler.net/problem=3 *)
(* Largest prime factor of a given integer. *)

(*
   Problem statement (Project Euler 3):
   The prime factors of 13195 are 5, 7, 13 and 29.
   What is the largest prime factor of the number 600851475143?

   Mathematical background:
   Every integer n > 1 can be uniquely written as a product of prime factors:
     n = p1 * p2 * ... * pk
   where each p_i is prime. We want the largest prime factor among them.

   Naive approach:
   - Factor n completely (e.g., by trial division), then take the maximum prime factor.
   - However, storing all factors or testing primality repeatedly is unnecessary.
   
   Idea of this algorithm (trial division with progressive reduction):
   1. We keep a working variable m, initially m = n.
   2. We try to divide m by small integers starting from factor = 2.
   3. While factor * factor <= m:
      - If factor divides m, we divide m by factor:
          m := m / factor
        This removes one occurrence of that prime factor from m.
        We do NOT increment factor here, because the same factor may divide m
        multiple times (e.g., for m = 2^k * ...).
      - If factor does NOT divide m, we increment factor by 1 and try the next integer.
        Composite values of factor are harmless:
        if factor is composite, it is made up of smaller primes that we have already
        tried and divided out, so it will no longer divide m at that point.
   4. The loop condition factor * factor <= m ensures:
      - If m still had a composite factor, at least one of its prime factors
        would be <= sqrt(m), hence we would find it during the loop.
   5. When the loop ends:
      - Either m = 1 (which does not happen here for a nontrivial n like 600851475143),
        or m > 1 and has no divisors <= sqrt(m).
      - If m > 1 and has no divisors <= sqrt(m), then m must be prime.
   6. Since we have successively divided out all smaller prime factors,
      the remaining m at the end of the loop is the largest prime factor of n.

   Summary:
   - We never explicitly test primality.
   - We progressively strip off small prime factors from n.
   - The final remaining value of m is the largest prime factor.
*)

largestPrimeFactor[n_] := Module[
  {
    (* m = working copy of n that we progressively factor
       factor = current candidate divisor *)
    m = n,
    factor = 2
  },
  
  (* While factor^2 <= m, there is still a possibility that m has
     a factor <= factor, hence m may still be composite. *)
  While[factor*factor <= m,
   
    (* If m is not divisible by factor, try the next integer.
       Otherwise, divide m by factor to remove this prime factor. *)
    If[Mod[m, factor] =!= 0,
      factor++,
      m = Quotient[m, factor]
    ];
  ];
  
  (* When the loop ends, m is either 1 or a prime.
     For the Project Euler input, m will be the largest prime factor. *)
  m
]

(* Constant from the problem statement: *)
LPF = 600851475143;

(* Compute the largest prime factor of LPF.
   The expected answer for Project Euler problem 3 is 6857. *)
largestPrimeFactor[LPF]
