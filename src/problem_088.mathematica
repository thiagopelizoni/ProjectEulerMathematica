(*
  Project Euler Problem 88: Product-sum numbers
  URL: https://projecteuler.net/problem=088

  Problem Statement:
  A natural number N that can be written as the sum and product of a given set of k natural numbers is called a
  product-sum number: N = a_1 + a_2 + ... + a_k = a_1 * a_2 * ... * a_k.
  For a given k, there may exist multiple N satisfying this property. We are interested in the minimal product-sum
  number for a specific k.
  For example, for k=2: 4 = 2+2 = 2*2 (minimal is 4).
  For k=3: 6 = 1+2+3 = 1*2*3 (minimal is 6).
  For k=4: 8 = 1+1+2+4 = 1*1*2*4 (minimal is 8).
  Find the sum of all distinct minimal product-sum numbers for 2 <= k <= 12000.

  Mathematical Analysis:
  We need to find the minimal N for each k in the range [2, 12000].
  Let the set of numbers be {a_1, ..., a_k}.
  Product P = Product(a_i) = N.
  Sum S = Sum(a_i) = N.
  Generally, to satisfy Product = Sum with k numbers where Product > Sum for non-unit factors, we fill the remaining
  count of numbers with 1s.
  If we choose non-unit factors {x_1, ..., x_m}, then the Product P = x_1 * ... * x_m.
  The current sum of these factors is S_part = x_1 + ... + x_m.
  To satisfy the condition, we need to add (k - m) ones.
  The equation becomes: P = S_part + (k - m) * 1.
  Therefore, k = P - S_part + m.
  Since we want minimal N for a specific k, we can iterate through possible sets of factors {x_1, ..., x_m}.
  Since k <= 12000, we know N must be slightly larger than k. In the worst case (k=12000), consider factors {2, k}:
  Sum = k+2, Product = 2k. We need k-2 ones. Total terms = 2 + (k-2) = k.
  N = 2k = 24000 is an upper bound for the minimal N for k=12000.
  So we only need to check composite numbers N up to 2*k_max = 24000.
  
  Algorithm Strategy:
  Instead of iterating k and finding N, we iterate through possible factor sets {x_1, ..., x_m} to generate N.
  For each generated N and calculated k, we update a global "best N for k" array.
  We iterate recursively to generate products:
  N = x_1 * ... * x_m.
  We check if the implied k = N - Sum(x_i) + m is within range [2, 12000].
  If so, Minimize MinK[k] with N.
  
  Bounds:
  k_max = 12000.
  Max N to consider is 2 * k_max = 24000.
  The recursion depth is small (log_2(24000) approx 14).
  The number of factorizations is manageable.
  
  Parallelization:
  The recursive search space can be partitioned. The first factor x_1 ranges from 2 up to Sqrt(24000).
  We can parallelize the outer loop over x_1. Each worker explores factor trees starting with a specific x_1
  and returns a list of {k, N} pairs found.
  Finally, we aggregate these pairs to find the global minimum for each k.
  Since the aggregation array is small (size 12001), we can just flatten the results and use a fast accumulation
  strategy (e.g., creating a sparse array or using Min on grouped data).
*)

solve[] := Module[{
  kMax, nMax, minNforK, initialResults, 
  processBranch, aggregateResults, distinctSum
  },
  
  kMax = 12000;
  nMax = 2 * kMax;

  processBranch = Function[{product, sum, count, start},
    Module[{p, s, c, k, res = {}, localRes},
      Do[
        p = product * x;
        s = sum + x;
        c = count + 1;
        
        k = p - s + c;
        
        If[k <= kMax,
          AppendTo[res, {k, p}];
          
          localRes = processBranch[p, s, c, x];
          If[Length[localRes] > 0,
            res = Join[res, localRes]
          ];
        ];
        ,
        {x, start, Floor[nMax / product]}
      ];
      res
    ]
  ];

  initialResults = ParallelTable[
    processBranch[i, i, 1, i],
    {i, 2, Floor[nMax/2]},
    Method -> "CoarsestGrained"
  ];

  initialResults = Flatten[initialResults, 1];

  minNforK = ConstantArray[Infinity, kMax + 1];

  Scan[
    Function[{pair},
      Module[{k = pair[[1]], n = pair[[2]]},
        If[n < minNforK[[k]], minNforK[[k]] = n]
      ]
    ],
    initialResults
  ];

  distinctSum = Total[Union[minNforK[[2 ;; kMax]]]];

  distinctSum
]

solve[]