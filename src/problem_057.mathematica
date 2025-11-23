(*
  Problem: Project Euler 57 - Square Root Convergents.
  
  Mathematical Context:
  The infinite continued fraction of Sqrt[2] corresponds to the periodic sequence [1; 2, 2, ...]. 
  The convergents p_k/q_k satisfy the recurrence p_{k+1} = p_k + 2q_k, q_{k+1} = p_k + q_k.
  We are tasked with identifying the cardinality of the subset of the first 1,000 iterations 
  where the logarithmic magnitude of the numerator strictly exceeds the denominator:
  Floor[Log10(p_k)] > Floor[Log10(q_k)].

  Algorithmic Strategy:
  1. State Space Generation: We employ `NestList` to generate the trajectory of pairs {p, q}.
     We utilize functional recursion to strictly emulate the sequential dependency of the terms.
     Crucially, we apply `Rest` to match the Python logic which updates the state *before* performing the condition check (pre-increment vs post-check logic).
  2. Parallel Map-Reduce: Instead of pattern counting, we broadcast the Boolean evaluation 
     (`IntegerLength`) via `ParallelMap` across all $ProcessorCount cores. 
  3. Aggregation: The boolean vector is collapsed into a scalar integer using `Total`.
     This guarantees a clean numerical return, completely suppressing intermediate structures.
*)

Module[{convergents, limit = 1000},
    (* Initialize kernel pool if not already saturated *)
    If[Length[Kernels[]] < $ProcessorCount, LaunchKernels[$ProcessorCount]];

    (* Generation Phase:
       Constructs the sequence. The semicolon (;) at the end is CRITICAL to prevent 
       the front-end from attempting to render huge integers.
    *)
    convergents = NestList[
        {#[[1]] + 2 #[[2]], #[[1]] + #[[2]]} &, 
        {3, 2}, 
        limit - 1
    ];

    (* Execution Phase (Map-Reduce):
       1. Rest: Discards the initial {3,2} to match Python's loop behavior (update-then-check).
       2. ParallelMap: Computes 1 (true) or 0 (false) for the digit condition on distributed kernels.
       3. Total: Sums the results into a single scalar.
    *)
    Total[
        ParallelMap[
            Boole[IntegerLength[#[[1]]] > IntegerLength[#[[2]]]] &, 
            Rest[convergents]
        ]
    ]
]