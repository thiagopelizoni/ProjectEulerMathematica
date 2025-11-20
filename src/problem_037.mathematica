(* Project Euler Problem 37: Truncatable Primes

   Problem Description:
   A truncatable prime is a prime number $p$ such that repeatedly removing digits from either the 
   left or the right results in a number that is always prime. By definition, single-digit primes 
   (2, 3, 5, 7) are excluded. The problem states there are exactly 11 such primes and asks for 
   their summation.

   Mathematical Solution:
   1. Definitions:
      Let $D(n)$ be the decimal digit sequence of $n$.
      Right-truncation implies determining primality for the set of prefixes: $P_{pre} = \{ d_1...d_k \mid 1 \le k < |D(n)| \}$.
      Left-truncation implies determining primality for the set of suffixes: $P_{suf} = \{ d_k...d_{|D(n)|} \mid 1 < k \le |D(n)| \}$.
   
   2. Predicate Logic:
      A number $n$ is a valid truncatable prime iff:
      $n \in \mathbb{P}$ (where $\mathbb{P}$ is the set of primes) AND $n > 7$ AND
      $\forall x \in (P_{pre} \cup P_{suf}), x \in \mathbb{P}$.

   3. Algorithm:
      We employ a search strategy starting from $n=11$ (the first possible candidate). Since the 
      problem guarantees exactly 11 solutions, we iterate through odd integers, accumulating valid 
      $n$ until the count reaches 11.
*)

TruncatablePrimeQ[n_Integer] := Module[{digits, len},
  If[n <= 7, Return[False]];
  digits = IntegerDigits[n];
  len = Length[digits];
  
  AllTrue[Range[1, len - 1], Function[i,
    PrimeQ[FromDigits[digits[[1 ;; i]]]] && 
    PrimeQ[FromDigits[digits[[i + 1 ;; -1]]]]
  ]]
]

foundPrimes = {};
candidate = 11;

While[Length[foundPrimes] < 11,
  If[PrimeQ[candidate] && TruncatablePrimeQ[candidate],
     AppendTo[foundPrimes, candidate]
  ];
  candidate += 2
];

Total[foundPrimes]