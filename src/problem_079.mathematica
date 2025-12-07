(*
  Project Euler Problem 79: Passcode Derivation
  URL: https://projecteuler.net/problem=079

  Problem Statement:
  A common security method used for online banking is to ask the user for three random characters from a passcode. For
  example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be:
  317. The text file keylog.txt contains fifty successful login attempts. Given that the three characters are always
  asked for in order, analyze the file so as to determine the shortest possible secret passcode of unknown length.

  Mathematical Analysis:
  The problem asks for the shortest supersequence S such that every provided 3-digit login attempt is a subsequence of S.
  We can model this using a directed graph where the vertices are the unique digits (0-9) present in the keylog. For
  every login attempt "xyz", we deduce two precedence constraints: x must precede y (x -> y) and y must precede z
  (y -> z). The shortest passcode corresponds to a topological ordering of this graph. If the graph is a Directed Acyclic
  Graph (DAG) and contains a Hamiltonian path (a path visiting every vertex exactly once), the topological sort is
  unique and represents the shortest sequence containing all digits exactly once. If no Hamiltonian path exists but the
  graph is acyclic, any topological sort is a valid supersequence, though "shortest" implies we assume each digit appears
  only once if consistent with the data. Given the constraints of Project Euler problems, the graph is expected to define
  a strict total ordering of the digits involved.

  Complexity and Feasibility:
  The number of unique digits is small (at most 10), and the number of constraints is linear with the input size (50
  entries). Constructing the graph takes O(N) time, and topological sorting takes O(V + E) time. For V <= 10 and E <= 100,
  this is computationally instantaneous. The algorithm's efficiency is bounded by the graph traversal, which is negligible.

  Parallelization Strategy:
  Although the problem size is trivial, we implement a parallel decomposition to satisfy the architectural requirement.
  We distribute the parsing of the 50 login attempts across available processor cores. Each core processes a subset of
  the codes to extract the directed edges (constraints). These partial edge lists are then aggregated (flattened and
  unioned) in the main process to construct the graph. This parallel map ensures that the edge extraction—the primary
  data ingestion step—is vectorized.

  Wolfram Language Implementation:
  - Embed the `keylog.txt` data directly to ensure self-containment.
  - Use `ParallelMap` to convert each 3-digit integer into a pair of directed rules (edges).
  - Use `Union` to remove duplicate constraints.
  - Construct a `Graph` object from the unique edges.
  - Apply `TopologicalSort` to determine the linear ordering of the vertices.
  - Reconstruct the passcode using `FromDigits`.
*)

solve[] := Module[{nCores, keylogData, edgeExtraction, uniqueEdges, dependencyGraph, orderedDigits},
  nCores = $ProcessorCount;

  keylogData = {
    319, 680, 180, 690, 129, 620, 762, 689, 762, 318, 368, 710, 720, 710, 629, 168, 160, 689, 716, 731,
    736, 729, 316, 729, 729, 710, 769, 290, 719, 680, 318, 389, 162, 289, 162, 718, 729, 319, 790, 680,
    890, 362, 319, 760, 316, 729, 380, 319, 728, 716
  };

  edgeExtraction = Function[{code},
    Module[{digits},
      digits = IntegerDigits[code];
      {digits[[1]] -> digits[[2]], digits[[2]] -> digits[[3]]}
    ]
  ];

  uniqueEdges = Union[Flatten[ParallelMap[edgeExtraction, keylogData]]];

  dependencyGraph = Graph[uniqueEdges];

  orderedDigits = TopologicalSort[dependencyGraph];

  FromDigits[orderedDigits]
]

solve[]