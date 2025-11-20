(* Problem: https://projecteuler.net/problem=32 *)
(* Pandigital products *)

(*
   Problem: We shall say that an n-digit number is pandigital if it makes use of
   all the digits 1 to n exactly once. For example, 15234 is 1 through 5 pandigital.
   
   The product 7254 is unusual, as the identity: 39 × 186 = 7254, containing
   multiplicand, multiplier, and product is 1 through 9 pandigital.
   
   Find the sum of all products whose multiplicand/multiplier/product identity
   can be written as a 1 through 9 pandigital.
   
   HINT: Some products can be obtained in more than one way so be sure to only
   include it once in your sum.
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: We need to find all products where concatenating the
   multiplicand, multiplier, and product forms a 1-9 pandigital string (using
   each digit 1-9 exactly once, with no 0).
   
   Constraint analysis: If multiplicand has a digits, multiplier has b digits,
   and product has c digits, then: a + b + c = 9
   
   Also, for multiplication: 10^(a-1) × 10^(b-1) ≤ product < 10^a × 10^b
   This means: c ≈ a + b (possibly c = a + b - 1 or c = a + b)
   
   From a + b + c = 9 and c ≈ a + b:
   - If c = a + b: then 2(a + b) = 9, impossible
   - If c = a + b - 1: then 2(a + b) - 1 = 9, so a + b = 5, c = 4
   
   Valid digit patterns:
   - 1 × 4 = 4 (1 digit × 4 digits = 4 digits)
   - 2 × 3 = 4 (2 digits × 3 digits = 4 digits)
   
   Search strategy:
   - Case 1: i is 1-digit (2-9), j is 4-digit (1234-9876/i)
   - Case 2: i is 2-digit (10-99), j is 3-digit (123-9876/i)
   
   For each pair (i, j), concatenate str(i) + str(j) + str(i×j) and check if
   it's 1-9 pandigital (contains each digit 1-9 exactly once).
   
   Note: Products may appear multiple times (e.g., 4 × 1738 = 6952 and 
   4 × 1963 = 7852 if both were pandigital), so use a set to track unique products.
*)

isPandigital[s_String] := 
  StringLength[s] == 9 && Sort[Characters[s]] == Characters["123456789"]

pandigitalProducts[] := Module[
  {products, i, j, product, concatenated, start},
  
  products = {};
  
  Do[
    start = If[i < 10, 1234, 123];
    Do[
      product = i * j;
      concatenated = StringJoin[ToString[i], ToString[j], ToString[product]];
      If[isPandigital[concatenated],
        AppendTo[products, product]
      ],
      {j, start, Floor[10000 / i]}
    ],
    {i, 2, 99}
  ];
  
  Total[DeleteDuplicates[products]]
]

pandigitalProducts[]
