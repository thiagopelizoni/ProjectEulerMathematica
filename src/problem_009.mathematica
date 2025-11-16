(* Problem: https://projecteuler.net/problem=9 *)
(* Product abc of the Pythagorean triplet (a, b, c) such that a + b + c = given sum. *)

(*
   Problem statement (Project Euler 9, paraphrased):
   A Pythagorean triplet is a set of three natural numbers a < b < c such that
     a^2 + b^2 = c^2.
   For example, (3, 4, 5) is a Pythagorean triplet, since 3^2 + 4^2 = 5^2.
   
   The problem states:
     There exists exactly one Pythagorean triplet for which
       a + b + c = 1000.
     Find the product abc. :contentReference[oaicite:0]{index=0}

   The known solution for a + b + c = 1000 is:
     (a, b, c) = (200, 375, 425)
   with product:
     abc = 200 * 375 * 425 = 31875000. :contentReference[oaicite:1]{index=1}

   ---------------------------------------------------------------------------
   Mathematical background
   ---------------------------------------------------------------------------

   1) Pythagorean triplet:
      A triple of natural numbers (a, b, c) is called a Pythagorean triplet if:
        a^2 + b^2 = c^2,
      which corresponds to the sides of a right triangle with integer lengths.

   2) Additional condition in this problem:
      We are not interested in all Pythagorean triplets. We want those that also
      satisfy a linear constraint on the sum:
        a + b + c = S
      where S = 1000 in the original Project Euler problem.

      We are told there is exactly one triplet of natural numbers (a, b, c)
      satisfying both:
        a^2 + b^2 = c^2
        a + b + c  = S

   3) Search space reduction using the sum constraint:
      From the condition a + b + c = S, we can express c in terms of a and b:
        c = S - a - b.

      Substituting into the Pythagorean equation:
        a^2 + b^2 = c^2
                  = (S - a - b)^2.

      The problem could be approached algebraically by solving this Diophantine
      equation, but the given Python solution uses a direct search.

   4) Direct search (brute force) strategy:
      We search over all possible integer values of a and b, derive c from the sum,
      and then check whether (a, b, c) forms a Pythagorean triplet.

      Concretely, for a given S = sumOfTriplet:

      - a ranges from 1 up to S - 1 (in practice, we could reduce this further,
        but this upper bound is simple and safe).
      - For each fixed a, b ranges from a up to (S - a - 1). We start b at a to
        favor the condition a <= b (which is consistent with a < b < c).
      - For each pair (a, b), set:
            c = S - a - b.
        This ensures:
            a + b + c = S.
      - Then we test the Pythagorean condition:
            a^2 + b^2 == c^2.
        If it is satisfied, we have found a Pythagorean triplet whose sum is S.

      Because the problem statement guarantees that there is exactly one such
      triplet for S = 1000, the algorithm can return immediately once it finds
      a valid (a, b, c).

   5) Complexity:
      The algorithm runs in O(S^2) time, since a and b can each be up to about S.
      For S = 1000 this is completely feasible.

   ---------------------------------------------------------------------------
   Wolfram Language translation of the Python algorithm
   ---------------------------------------------------------------------------

   The Python code structure:

     def pythagorean_triplet_product(sum_of_triplet):
         for a in range(1, sum_of_triplet):
             for b in range(a, sum_of_triplet - a):
                 c = sum_of_triplet - a - b
                 if a * a + b * b == c * c:
                     return a * b * c
         return None

   We implement the same logic in Wolfram Language using nested Do loops
   and early Return when we find the correct triplet.
*)

pythagoreanTripletProduct[sumOfTriplet_Integer?Positive] := Module[
  {
    a, b, c
  },
  
  (* Outer loop: try all possible values of a from 1 to sumOfTriplet - 1. *)
  Do[
    (* Inner loop: for each a, try b from a up to sumOfTriplet - a - 1.
       Starting b at a is consistent with the condition a <= b < c. *)
    Do[
      (* c is determined uniquely from the sum constraint a + b + c = sumOfTriplet. *)
      c = sumOfTriplet - a - b;
      
      (* Check Pythagorean condition a^2 + b^2 = c^2. *)
      If[a^2 + b^2 == c^2,
        (* Once a valid triplet is found, return the product abc. *)
        Return[a*b*c]
      ],
      {b, a, sumOfTriplet - a - 1}
    ],
    {a, 1, sumOfTriplet - 1}
  ];
  
  (* If no triplet was found (which does not happen for sumOfTriplet = 1000),
     we return $Failed as a symbolic indication of failure. *)
  $Failed
]

pythagoreanTripletProduct[1000]
