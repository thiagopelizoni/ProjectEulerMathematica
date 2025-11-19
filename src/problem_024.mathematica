(* Problem: https://projecteuler.net/problem=24 *)
(* Lexicographic permutations *)

(*
   Problem: A permutation is an ordered arrangement of objects. For example, 3124
   is one possible permutation of the digits 0, 1, 2 and 3. If all of the
   permutations are listed numerically or alphabetically, we call it lexicographic
   order. The lexicographic permutations of 0, 1 and 2 are:
   
   012   021   102   120   201   210
   
   What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
   5, 6, 7, 8 and 9?
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: This problem asks for the 1,000,000th permutation when
   all permutations of {0,1,2,3,4,5,6,7,8,9} are arranged in lexicographic
   (dictionary) order.
   
   There are 10! = 3,628,800 total permutations of 10 distinct elements.
   
   Direct approach: Generate all permutations, sort them lexicographically, and
   extract the millionth one. While conceptually simple, this creates millions
   of permutations in memory.
   
   The solution:
   1. Generate all permutations of the digits 0-9
   2. Sort them in lexicographic order (numerically for digit sequences)
   3. Extract the 1,000,000th permutation (index 999,999 in 0-based indexing)
   4. Convert the digit list to a string
   
   Wolfram's Permutations function generates permutations in lexicographic order
   by default, so explicit sorting is unnecessary.
*)

millionth = 1000000;

lexicographicPermutation[] := Module[
  {
    allPermutations, targetPermutation
  },
  
  allPermutations = Permutations[Range[0, 9]];
  targetPermutation = allPermutations[[millionth]];
  StringJoin[ToString /@ targetPermutation]
]

lexicographicPermutation[]
