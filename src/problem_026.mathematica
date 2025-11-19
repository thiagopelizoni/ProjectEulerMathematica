(* Problem: https://projecteuler.net/problem=26 *)
(* Longest recurring cycle in unit fractions *)

(*
   Problem: A unit fraction contains 1 in the numerator. The decimal representation
   of the unit fraction 1/d has different behaviors:
   
   1/2 = 0.5 (terminating)
   1/3 = 0.333... (recurring cycle length 1)
   1/6 = 0.1666... (recurring cycle length 1)
   1/7 = 0.142857142857... (recurring cycle length 6)
   
   Find the value of d < 1000 for which 1/d contains the longest recurring cycle
   in its decimal fraction part.
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: When computing 1/d by long division, we repeatedly:
   1. Multiply the remainder by 10
   2. Divide by d to get the next digit
   3. Keep the new remainder
   
   A recurring cycle begins when we encounter a remainder we've seen before. The
   cycle length is the number of steps between the two occurrences of that remainder.
   
   Key insight: If the remainder becomes 0, the decimal terminates (no cycle).
   Otherwise, since remainders are in {1, 2, ..., d-1}, we must eventually see
   a repeated remainder by the pigeonhole principle, creating a cycle.
   
   The cycle length is related to the multiplicative order of 10 modulo d. For
   d coprime to 10 (i.e., d not divisible by 2 or 5), the cycle length equals
   the smallest k such that 10^k ≡ 1 (mod d).
   
   Algorithm:
   1. For each d from 1 to 999:
      a. Simulate long division, tracking which remainders we've seen
      b. Store the position where each remainder first appeared
      c. When we see a repeated remainder, the cycle length is the difference
         in positions
      d. If remainder becomes 0, there's no cycle (terminating decimal)
   2. Find the d with maximum cycle length
*)

findRecurringCycleLength[d_Integer] := Module[
  {seenRemainders, value, position},
  
  seenRemainders = <||>;
  value = 1;
  position = 0;
  
  While[!KeyExistsQ[seenRemainders, value],
    seenRemainders[value] = position;
    value = Mod[value * 10, d];
    position++
  ];
  
  If[value == 0,
    0,
    position - seenRemainders[value]
  ]
]

longestRecurringCycle[] := Module[
  {maxLength, numberWithMaxCycle, d, currentLength},
  
  maxLength = 0;
  numberWithMaxCycle = 0;
  
  Do[
    currentLength = findRecurringCycleLength[d];
    If[currentLength > maxLength,
      maxLength = currentLength;
      numberWithMaxCycle = d
    ],
    {d, 1, 999}
  ];
  
  numberWithMaxCycle
]

longestRecurringCycle[]
