(* Problem: https://projecteuler.net/problem=4 *)
(* Largest palindrome made from the product of two 3-digit numbers. *)

(*
   Problem statement (Project Euler 4):
   A palindromic number reads the same forwards and backwards.
   The largest palindrome made from the product of two 2-digit numbers
   is 9009 = 91 × 99.

   Task:
   Find the largest palindrome that can be written as the product
   of two 3-digit numbers.

   Mathematical background:

   1) Palindromic number
      A palindromic number n in base 10 is a number whose decimal
      digit sequence is invariant under reversal.

      Example:
        9009 has digits [9, 0, 0, 9].
        Reversed digits: [9, 0, 0, 9].
        Since the two sequences match, 9009 is a palindrome.

      Formally:
        Let d_0, d_1, ..., d_{k-1} be the digits of n.
        Then n is palindromic if for all i:
          d_i = d_{k-1-i}.

   2) Range of search
      We are asked to consider products of two 3-digit numbers.
      A 3-digit number ranges from 100 to 999.

      Therefore, we need to consider:
        i * j
        with i, j ∈ {100, 101, ..., 999}.

      The smallest possible product is:
        100 * 100 = 10,000  (5-digit number).

      The largest possible product is:
        999 * 999 = 998,001 (6-digit number).

      So the largest palindrome we are looking for is a 5- or 6-digit
      palindromic number in that range.

   3) Brute-force search strategy
      We can systematically check each product i * j for 100 <= i, j <= 999:

      - For each pair (i, j), compute product = i * j.
      - Test if product is a palindrome.
      - Maintain a variable "largestPalindrome" which stores the largest
        palindromic product found so far.
      - If product is palindromic and product > largestPalindrome,
        update largestPalindrome := product.
      - After scanning all pairs, largestPalindrome holds the desired answer.

      Complexity:
      - Number of (i, j) pairs = 900 * 900 = 810,000.
      - For each product, checking palindromicity involves converting
        to digits and comparing with the reversed list of digits.
      - This brute-force approach is computationally feasible for this scope.

   4) Palindrome test in Wolfram Language
      In Python, the original solution uses:
        str(n) == str(n)[::-1]

      In Wolfram Language, we can use integer digit operations:

        IntegerDigits[n]
          returns the list of base-10 digits of n.

        Reverse[IntegerDigits[n]]
          returns the reversed list of digits.

      So:
        n is a palindrome  <=>  IntegerDigits[n] === Reverse[IntegerDigits[n]].

   5) Expected result (from known solutions of Project Euler 4):
      The largest palindrome made from the product of two 3-digit numbers is:
        906609 = 913 × 993.
*)

(* Predicate that checks whether an integer n is a decimal palindrome. *)
isPalindrome[n_Integer] := Module[{digits},
  digits = IntegerDigits[n];
  digits === Reverse[digits]
]

(* Compute the largest palindromic product of two 3-digit numbers. *)
largestPalindromeProduct[] := Module[
  {
    largestPalindrome = 0,
    product
  },
  
  (* Iterate over all pairs (i, j) with 100 <= i, j <= 999. *)
  Do[
    Do[
      product = i * j;
      
      (* If product is palindromic and greater than the current maximum,
         update largestPalindrome. *)
      If[isPalindrome[product] && product > largestPalindrome,
        largestPalindrome = product
      ],
      {j, 100, 999}
    ],
    {i, 100, 999}
  ];
  
  (* After scanning all pairs, largestPalindrome holds the result. *)
  largestPalindrome
]

(* Example: compute the largest palindrome from the product of two 3-digit numbers.
   The expected answer for Project Euler problem 4 is 906609. *)
largestPalindromeProduct[]
