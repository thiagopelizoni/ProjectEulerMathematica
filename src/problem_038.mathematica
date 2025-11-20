(* Project Euler Problem 38: Pandigital Multiples

   Problem Description:
   Take a positive integer x and multiply it by the integer sequence (1, 2, ..., n) where n > 1.
   Concatenate the results to form a new number P. The goal is to find the largest such P that 
   is a 1 to 9 pandigital number (contains digits 1 through 9 exactly once).

   Mathematical Solution:
   1. Domain Constraints (Upper Bound):
      The resulting number P must have exactly 9 digits.
      If x is a 5-digit number, the concatenation of x*1 (5 digits) and x*2 (at least 5 digits) 
      results in a length >= 10, which violates the pandigital constraint.
      Therefore, x must be strictly less than 10,000 (a 4-digit number).
   
   2. Constructive Algorithm:
      We iterate through the candidate integers x in the domain [1, 9999].
      For each x, we generate the concatenated product sequence iteratively until the length 
      is >= 9.
      
   3. Verification:
      If the length is exactly 9, we check if the set of digits is exactly {1, 2, ..., 9}.
      The maximum of these valid values is the solution.
*)

PandigitalQ[s_String] := Sort[Characters[s]] == Characters["123456789"]

GetPandigitalCandidate[k_Integer] := Module[{s = "", n = 1},
  While[StringLength[s] < 9,
    s = s <> ToString[k * n];
    n++
  ];
  If[StringLength[s] == 9 && PandigitalQ[s], FromDigits[s], 0]
]

Max[Table[GetPandigitalCandidate[i], {i, 1, 9999}]]