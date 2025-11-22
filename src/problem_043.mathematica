(* Project Euler Problem 43: Sub-string Divisibility

   Problem Description:
   We consider the set of 0 to 9 pandigital numbers (permutations of digits 0-9). 
   Let $d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 d_9 d_{10}$ be the decimal representation of such a number.
   The objective is to sum all integers in this set that satisfy the following sub-string 
   divisibility property for the sequence of primes $P = \{2, 3, 5, 7, 11, 13, 17\}$:
   
   $d_2 d_3 d_4 \equiv 0 \pmod 2$
   $d_3 d_4 d_5 \equiv 0 \pmod 3$
   $d_4 d_5 d_6 \equiv 0 \pmod 5$
   $d_5 d_6 d_7 \equiv 0 \pmod 7$
   $d_6 d_7 d_8 \equiv 0 \pmod{11}$
   $d_7 d_8 d_9 \equiv 0 \pmod{13}$
   $d_8 d_9 d_{10} \equiv 0 \pmod{17}$

   Mathematical Solution:
   1. Search Space: The domain consists of the symmetric group $S_{10}$ acting on the set of digits 
      $\{0, 1, ..., 9\}$. The cardinality is $10! = 3,628,800$, which is computationally tractable 
      for brute-force verification.
   2. Verification: For each permutation vector $\mathbf{v}$, we extract the specified triplets 
      and verify modular congruence against the corresponding prime.
   3. Aggregation: The valid permutation vectors are converted back to integers and summed.
*)

SubStringDivisibleQ[digits_List] := 
  AllTrue[Range[7], Function[i, 
    Divisible[FromDigits[digits[[i + 1 ;; i + 3]]], {2, 3, 5, 7, 11, 13, 17}[[i]]]
  ]]

candidates = Permutations[Range[0, 9]];

matches = Select[candidates, SubStringDivisibleQ];

Total[FromDigits /@ matches]