(*
  Problem: Project Euler 53 - Combinatoric Selections
  
  Mathematical Formulation:
  Let C(n, r) denote the binomial coefficient \binom{n}{r} = \frac{n!}{r!(n-r)!}.
  We define the set of indices \Omega = \{ (n, r) \in \mathbb{Z}^2 \mid 1 \le n \le 100, 1 \le r \le n \}.
  The objective is to determine the cardinality of the subset \Phi \subseteq \Omega defined by the condition:
  \Phi = \{ (n, r) \in \Omega \mid C(n, r) > L \}, where L = 10^6.
  
  Computational Strategy:
  The algorithm performs a direct enumeration over the triangular domain defined by \Omega.
  To exploit the intrinsic data parallelism of the independent binomial evaluations, we utilize the 
  ParallelSum construct. The workload is dynamically distributed across all available logical cores 
  identified by $ProcessorCount, aggregating the boolean truth values of the threshold inequality.
*)

LaunchKernels[$ProcessorCount];

Module[{limit = 10^6},
  ParallelSum[
    Boole[Binomial[n, r] > limit],
    {n, 1, 100},
    {r, 1, n}
  ]
]