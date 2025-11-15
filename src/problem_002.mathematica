(* Problem: https://projecteuler.net/problem=2 *)
(* Sum of even-valued terms in the Fibonacci sequence up to a given limit. *)

(* 
   Fibonacci sequence used in the problem:
   F_1 = 1, F_2 = 2 and F_{n+1} = F_n + F_{n-1}.

   First terms:
   1, 2, 3, 5, 8, 13, 21, 34, ...

   Parity pattern:
   odd, even, odd, odd, even, odd, odd, even, ...

   That is:
   - in every block of 3 terms there is exactly one even term;
   - the even terms are F_{2}, F_{5}, F_{8}, F_{11}, ...,
     i.e., F_{3k+2} for k >= 0.

   If we define E_n as the n-th even term of the Fibonacci sequence,
   we have for example:
     E_1 = 2   (F_2)
     E_2 = 8   (F_5)
     E_3 = 34  (F_8)
     E_4 = 144 (F_11)
     E_5 = 610 (F_14)
     ...

   These E_n satisfy a second-order linear recurrence:
     E_{n+1} = 4*E_n + E_{n-1}, for n >= 2,

   with initial conditions:
     E_1 = 2
     E_2 = 8.

   Intuition for the recurrence:
   - it can be derived from identities of the Fibonacci sequence,
     using that E_n = F_{3n+2} and the relation F_{n+1} = F_n + F_{n-1};
   - numerically, we see that:
       34  = 4*8  + 2
       144 = 4*34 + 8
       610 = 4*144 + 34
     which supports the validity of the relation E_{n+1} = 4*E_n + E_{n-1}.

   Idea of the algorithm:
   - instead of generating ALL Fibonacci numbers and filtering the even ones,
     we generate only the even terms E_n directly through this recurrence;
   - we accumulate the sum of E_n while they remain <= limit.
*)

sumEvenFibonacci[limit_] := Module[
  {
    (* a = E_n   (current even Fibonacci term)
       b = E_{n+1} (next even Fibonacci term)
       total = accumulated sum of even terms <= limit
    *)
    a = 2,  (* E_1 *)
    b = 8,  (* E_2 *)
    total = 0
  },
  
  (* While the current even term has not exceeded the limit,
     we add this term to the sum and advance in the recurrence:
       (E_n, E_{n+1}) -> (E_{n+1}, E_{n+2})
     where E_{n+2} = 4*E_{n+1} + E_n.
  *)
  While[a <= limit,
    total += a;
    {a, b} = {b, 4*b + a};
  ];
  
  (* When E_n exceeds the limit, we stop and return the accumulated sum. *)
  total
]

(* Example: sum of even Fibonacci terms <= 4,000,000.
   The expected value for Project Euler problem 2 is 4613732.
*)
sumEvenFibonacci[4000000]
