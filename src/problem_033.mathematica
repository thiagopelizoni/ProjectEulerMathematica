(* Project Euler Problem 33: Digit Cancelling Fractions

   Problem Description:
   The objective is to identify the set of non-trivial fractions n/d < 1, where n and d are 
   two-digit integers (10 <= n < d <= 99), such that incorrectly canceling a common digit 
   results in a fraction equivalent to the original value (e.g., 49/98 = 4/8). 
   Trivial examples involving multiples of 10 (e.g., 30/50) are excluded.

   Mathematical Solution:
   1. Define the sample space S = {(n, d) | 10 <= n < d <= 99}.
   2. For each pair in S, determine the intersection set I of their decimal digits.
   3. Filter criteria: I is not empty and 0 is not an element of I (avoids trivial cases).
   4. Verification: For a common digit k in I, verify if n/d == (n without k)/(d without k).
   5. The result is the denominator of the product of all valid fractions expressed in lowest terms.
*)

CuriousFractionQ[{n_, d_}] := Module[{nDigits, dDigits, common, nReduced, dReduced},
  nDigits = IntegerDigits[n];
  dDigits = IntegerDigits[d];
  common = Intersection[nDigits, dDigits];

  If[common === {} || MemberQ[common, 0], Return[False]];

  AnyTrue[common, Function[k,
    dReduced = DeleteCases[dDigits, k, 1, 1];
    If[First[dReduced] == 0, False,
       nReduced = DeleteCases[nDigits, k, 1, 1];
       n/d == First[nReduced]/First[dReduced]
    ]
  ]]
]

domain = Flatten[Table[{n, d}, {n, 10, 99}, {d, n + 1, 99}], 1];

solutions = Select[domain, CuriousFractionQ];

Denominator[Times @@ (Divide @@@ solutions)]