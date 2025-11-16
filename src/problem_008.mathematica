(* Problem: https://projecteuler.net/problem=8 *)
(* Largest product of k adjacent digits in a given large digit series. *)

(*
   Problem statement (Project Euler 8, paraphrased):
   We are given a 1000-digit number written as a continuous sequence of digits.
   Example (shortened here):
     73167176531330624919225119674426574742355349194934
     96983520312774506326239578318016984801869478851843
     ...
     71636269561882670428252483600823257530420752963450

   The four adjacent digits in this 1000-digit number with the greatest product
   are known to be 9 × 9 × 8 × 9 = 5832 (a test/example in the statement).

   The actual task:
     Find the thirteen adjacent digits in the 1000-digit number that have the
     greatest product. What is the value of this product? :contentReference[oaicite:0]{index=0}


   ---------------------------------------------------------------------------
   Mathematical interpretation
   ---------------------------------------------------------------------------

   Let the 1000-digit number be viewed as a sequence of digits:

     d_0, d_1, d_2, ..., d_{999},

   where each d_i ∈ {0, 1, 2, ..., 9}.

   For a fixed window size k (here k = 13), we consider all contiguous
   subsequences of length k:

     (d_i, d_{i+1}, ..., d_{i+k-1})

   for i = 0, 1, ..., (1000 - k).

   For each such subsequence, define the product:

     P_i = d_i * d_{i+1} * ... * d_{i+k-1}.

   Our goal is to compute:

     max_{0 <= i <= 1000-k} P_i,

   i.e., the maximum product of any k consecutive digits in the sequence.

   Properties/observations:
   - If any digit in the k-length window is 0, then P_i = 0.
   - Nonzero products arise from windows containing only digits 1..9.
   - The brute-force algorithm is still efficient enough because
     the sequence length is only 1000, and k = 13, so there are
     (1000 - 13 + 1) = 988 windows to check, and each product
     involves at most 13 multiplications. The overall complexity
     is O(N * k) with N = length of the series, which is practical.

   The given Python solution implements exactly this sliding-window
   brute-force approach:
   - series: the 1000-digit string.
   - Loop i over all starting indices of windows of length k.
   - For each window, compute the digit product.
   - Track the maximum product seen.

   ---------------------------------------------------------------------------
   Translating the algorithm to Wolfram Language
   ---------------------------------------------------------------------------

   We will:

   1) Represent the 1000-digit number as a long string "series".
   2) Convert this string into a list of integers (digits) using ToCharacterCode
      and digit conversion, or simply use CharacterRange and ToExpression.
   3) For each starting position i from 1 to (len - k + 1):
        - Extract the sublist of length k.
        - Compute the product of those k digits.
        - Update a running maximum.
   4) Return the maximum product.

   Note about indexing:
   - Python uses 0-based indexing; Wolfram Language uses 1-based.
   - In Python:
       for i in range(len(series) - k + 1):
         digits: series[i : i + k]
     In Wolfram Language:
       positions i go from 1 to (length - k + 1)
       digits: Take[digitList, {i, i + k - 1}]

   For the specific Project Euler problem:
   - N = 1000 (length of the digit string);
   - k = 13 (window size);
   - the known correct answer is 23514624000. :contentReference[oaicite:1]{index=1}
*)

(* Convert the digit series string into a list of integer digits. *)
digitSeries = StringJoin[
  "73167176531330624919225119674426574742355349194934",
  "96983520312774506326239578318016984801869478851843",
  "85861560789112949495459501737958331952853208805511",
  "12540698747158523863050715693290963295227443043557",
  "66896648950445244523161731856403098711121722383113",
  "62229893423380308135336276614282806444486645238749",
  "30358907296290491560440772390713810515859307960866",
  "70172427121883998797908792274921901699720888093776",
  "65727333001053367881220235421809751254540594752243",
  "52584907711670556013604839586446706324415722155397",
  "53697817977846174064955149290862569321978468622482",
  "83972241375657056057490261407972968652414535100474",
  "82166370484403199890008895243450658541227588666881",
  "16427171479924442928230863465674813919123162824586",
  "17866458359124566529476545682848912883142607690042",
  "24219022671055626321111109370544217506941658960408",
  "07198403850962455444362981230987879927244284909188",
  "84580156166097919133875499200524063689912560717606",
  "05886116467109405077541002256983155200055935729725",
  "71636269561882670428252483600823257530420752963450"
];

(* Convert each character '0'..'9' to its integer value 0..9. *)
digitList = ToExpression[Characters[digitSeries]];

(* Main function: largest product of k adjacent digits in the series. *)
largestProductInSeries[k_Integer?Positive] := Module[
  {
    maxProduct = 0,
    product,
    len = Length[digitList]
  },
  
  (* Slide a window of length k along the digit list. *)
  Do[
    (* Extract k consecutive digits: positions i through i + k - 1. *)
    product = Times @@ Take[digitList, {i, i + k - 1}];
    
    (* Update maximum if this product is larger. *)
    If[product > maxProduct,
      maxProduct = product
    ],
    {i, 1, len - k + 1}
  ];
  
  (* After scanning all windows, maxProduct is the largest product. *)
  maxProduct
]

largestProductInSeries[13]
