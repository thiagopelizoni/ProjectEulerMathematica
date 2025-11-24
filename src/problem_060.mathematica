(*
  Problem: Project Euler 60 - Prime Pair Sets.

  Mathematical Analysis:
  We treat the set of prime numbers as vertices V in an undirected graph G = (V, E).
  An edge (u, v) exists in E if and only if both concatenations (u || v) and (v || u) 
  are prime numbers. The problem demands the identification of a 5-clique (a complete 
  subgraph K_5) such that the scalar sum of its vertices is minimized.
  
  Computational Strategy:
  1. Vertex Space: We restrict the domain to the first 1,200 primes, a heuristic upper 
     bound sufficient to contain the minimal sum solution (max prime ~9800).
  2. Adjacency Logic: We forego the explicit construction of the O(N^2) adjacency matrix 
     in favor of dynamic predicate evaluation (`concatsToPrimeQ`) to conserve memory.
  3. Distributed Depth-First Search (DFS): The search for the clique is performed via 
     recursive backtracking. To satisfy the requirement for Dynamic Parallelism, we 
     partition the search tree at the root level. The first node of the potential clique 
     is distributed across the kernel pool ($ProcessorCount) using `ParallelMap`.
  4. Pruning: The validity of a candidate vertex is checked against the current partial 
     clique intersection, aggressively pruning the recursion tree.
*)
 
Module[{
    primeLimit = 1200, 
    primes, 
    concatsToPrimeQ, 
    findClique, 
    searchRoots
  },

  If[Length[Kernels[]] < $ProcessorCount, LaunchKernels[$ProcessorCount]];

  primes = Prime[Range[primeLimit]];

  concatsToPrimeQ[a_, b_] := 
      PrimeQ[a * 10^IntegerLength[b] + b] && 
      PrimeQ[b * 10^IntegerLength[a] + a];

  findClique[currentClique_, idx_] := Module[{candidates, res},
      If[Length[currentClique] == 5, 
          Return[Total[currentClique]]
      ];

      candidates = Select[
          primes[[idx + 1 ;;]], 
          Function[p, AllTrue[currentClique, concatsToPrimeQ[#, p] &]]
      ];

      res = MapIndexed[
          findClique[Append[currentClique, #1], idx + #2[[1]]] &, 
          candidates
      ];
      
      Flatten[res]
  ];

  DistributeDefinitions[primes, concatsToPrimeQ, findClique];
  
  Min[
      ParallelMap[
          findClique[{#}, First[First[Position[primes, #]]]] &, 
          primes[[1 ;; Length[primes] - 4]]
      ]
  ]
]