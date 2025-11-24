(*
Project Euler Problem 61: Cyclical Polygonal Numbers
URL: https://projecteuler.net/problem=061

Problem Analysis:
We are looking for a set of six 4-digit numbers {n1, ..., n6} such that:
1. The set contains exactly one number of each polygonal type: Triangle (3), Square (4), ..., Octagonal (8).
2. The numbers form a cycle: the last two digits of ni are the first two digits of n(i+1), and n6 wraps to n1.
3. All numbers are in [1000, 9999].

Algorithm:
1. Generate all 4-digit polygonal numbers for s=3..8.
2. Build a directed graph where an edge exists from u to v if Suffix(u) == Prefix(v).
   - Nodes are pairs {number, type}.
   - To optimize, we group nodes by their Prefix (Quotient[n, 100]).
3. The cycle must contain exactly one Octagonal number. We can fix the starting node of the cycle to be an Octagonal number to break cyclic symmetry and avoid duplicates.
4. Perform a Depth-First Search (DFS) starting from each Octagonal number.
   - State: (current_node, start_node, visited_types_mask, path).
   - Goal: Path length 6 with Suffix(last) == Prefix(start).
   - Optimization: Use bitmasks to track visited types.
5. Parallelism: The set of Octagonal numbers is partitioned across available cores. Each core runs the DFS for its subset.

Complexity:
- N (total numbers) ~ 450.
- P (paths) is small due to strict suffix constraints.
- Solution fits comfortably within seconds on a single core; parallelization makes it instantaneous.
*)

nCores = $ProcessorCount;
polyFormula[3, n_] := n (n + 1) / 2;
polyFormula[4, n_] := n^2;
polyFormula[5, n_] := n (3 n - 1) / 2;
polyFormula[6, n_] := n (2 n - 1);
polyFormula[7, n_] := n (5 n - 3) / 2;
polyFormula[8, n_] := n (3 n - 2);

getPolys[s_] := Module[{n = 1, val, list = {}},
  val = polyFormula[s, n];
  While[val < 10000,
    If[val >= 1000 && Mod[val, 100] >= 10,
      AppendTo[list, {val, s}]
    ];
    n++;
    val = polyFormula[s, n];
  ];
  list
];

dfs[curr_, start_, mask_, path_, graph_] := Module[{suffix, candidates, newMask, typeBit},
  If[Length[path] == 6,
    If[Mod[curr[[1]], 100] == Quotient[start[[1]], 100],
      Throw[path, "Found"]
    ];
    Return[]
  ];

  suffix = Mod[curr[[1]], 100];
  candidates = Lookup[graph, suffix, {}];

  Do[
    typeBit = BitShiftLeft[1, cand[[2]] - 3];
    If[BitAnd[mask, typeBit] == 0,
      newMask = BitOr[mask, typeBit];
      dfs[cand, start, newMask, Append[path, cand], graph]
    ],
    {cand, candidates}
  ];
];

findCycle[startNode_, graph_] := Catch[
  dfs[startNode, startNode, BitShiftLeft[1, 8 - 3], {startNode}, graph];
  {},
  "Found"
];

solveProjectEuler61[] := Module[{allNumbers, graph, octagonalStarts, results, validSet},
  allNumbers = Join @@ Table[getPolys[s], {s, 3, 8}];
  graph = GroupBy[allNumbers, Quotient[#[[1]], 100] &];
  octagonalStarts = Select[allNumbers, #[[2]] == 8 &];
  DistributeDefinitions[dfs, findCycle];
  results = ParallelMap[findCycle[#, graph] &, octagonalStarts];
  validSet = FirstCase[results, x_List /; Length[x] == 6];
  Total[validSet[[All, 1]]]
];

solveProjectEuler61[]