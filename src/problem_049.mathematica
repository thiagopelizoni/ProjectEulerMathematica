(* Project Euler Problem 49: Prime Permutations

   Problem Description:
   We examine the set of 4-digit prime numbers. We define a subset as a "prime permutation sequence" 
   if it satisfies three conditions:
   1. It contains three distinct prime numbers {p1, p2, p3}.
   2. The terms are permutations of one another (i.e., they share the same multiset of digits).
   3. The terms form an arithmetic progression: p2 - p1 = p3 - p2 = k.
   
   The sequence {1487, 4817, 8147} is known. The goal is to find the other such sequence 
   and return the concatenation of its terms.

   Mathematical Solution:
   1. Equivalence Classes:
      Instead of generating permutations for every prime (which yields many composites), we partition 
      the set of all primes $P \in [1000, 9999]$ into equivalence classes based on their digit signature.
      Two primes $a, b$ are in the same class iff $Sort(Digits(a)) = Sort(Digits(b))$.
      
   2. Filtering:
      We discard any class with cardinality $|C| < 3$.
      
   3. Arithmetic Progression Search:
      For each remaining class, we examine all 3-combinations (subsets of size 3). 
      Let a sorted triplet be $(x, y, z)$. It forms an arithmetic progression if $y - x = z - y$.
      
   4. Selection:
      We select the sequence where the first term is not 1487 and concatenate the integers.
*)

FindArithmeticTriple[group_List] := Module[{subsets},
  subsets = Subsets[Sort[group], {3}];
  SelectFirst[subsets, #[[2]] - #[[1]] == #[[3]] - #[[2]] &]
]

primes = Select[Range[1000, 9999], PrimeQ];

permutationGroups = GatherBy[primes, Sort[IntegerDigits[#]] &];

viableGroups = Select[permutationGroups, Length[#] >= 3 &];

sequences = DeleteMissing[FindArithmeticTriple /@ viableGroups];

result = SelectFirst[sequences, #[[1]] != 1487 &];

StringJoin[ToString /@ result]