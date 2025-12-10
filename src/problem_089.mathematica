(*
  Project Euler Problem 89: Roman Numerals
  URL: https://projecteuler.net/problem=089

  Problem Statement:
  The problem provides a file 'roman.txt' containing 1000 Roman numerals written in various forms. While Roman numerals
  often follow the standard subtractive notation (e.g., IV for 4, IX for 9), they can also be written additively
  (e.g., IIII for 4). The goal is to replace each numeral in the file with its minimal standard form (using subtractive
  pairs like IV, IX, XL, XC, CD, CM where appropriate) and calculate the total number of characters saved.

  Mathematical Analysis:
  A Roman numeral represents a specific integer value. The "minimal form" is unique and corresponds to the standard
  rules of Roman numeral construction:
  1. Values are I=1, V=5, X=10, L=50, C=100, D=500, M=1000.
  2. Numerals are generally written in descending order of value.
  3. Subtractive notation is used for 4 (IV), 9 (IX), 40 (XL), 90 (XC), 400 (CD), and 900 (CM).
  To solve the problem, we must:
  1. Parse each string from the file and convert it to its integer value.
  2. Convert that integer back to its minimal Roman numeral string representation.
  3. Compute the difference in length between the original string and the minimal string.
  4. Sum these differences.
  Alternatively, one can perform direct string replacement optimization without full integer conversion, but converting
  to integer and back is rigorously correct and less prone to edge-case errors in regex logic.
  Specifically, "saving characters" comes from:
  - IIII (4 chars) -> IV (2 chars) : saves 2.
  - VIIII (5 chars) -> IX (2 chars) : saves 3.
  - XXXX (4 chars) -> XL (2 chars) : saves 2.
  - LXXXX (5 chars) -> XC (2 chars) : saves 3.
  - CCCC (4 chars) -> CD (2 chars) : saves 2.
  - DCCCC (5 chars) -> CM (2 chars) : saves 3.

  Computational Complexity:
  The input consists of 1000 strings of short length. Converting Roman to Integer and Integer to Roman takes time
  proportional to the value or string length, which is negligible (O(1) relative to total input size).
  Processing 1000 items is effectively instantaneous.

  Parallelization Strategy:
  The processing of each line is independent. We can use `ParallelMap` to apply the `optimizeRoman` function to the
  list of input strings. The results (characters saved per line) are then summed.

  Wolfram Language Implementation:
  - `Import` the file.
  - Define `fromRoman` to parse the string. (Using `RomanNumeral` built-in function is possible, but a custom parser
    ensures strict adherence to problem logic if standard libraries behave differently on non-standard inputs; however,
    Mathematica's `FromRomanNumeral` handles additive variations robustly).
  - Define `toRoman` to generate the minimal string (using `RomanNumeral`).
  - Calculate length difference.
  - Sum the results.
*)

solve[] := Module[{nCores, url, data, processNumeral, savedChars},
  nCores = $ProcessorCount;
  url = "https://projecteuler.net/project/resources/p089_roman.txt";

  (* Import the data as a list of strings *)
  data = Import[url, "List"];

  (* Function to calculate characters saved for a single Roman numeral string *)
  processNumeral = Function[{originalStr},
    Module[{val, minimalStr},
      (* Convert original Roman string to Integer value *)
      (* FromRomanNumeral handles non-minimal forms correctly e.g., IIII -> 4 *)
      val = FromRomanNumeral[originalStr];
      
      (* Convert Integer value back to minimal Roman string *)
      minimalStr = RomanNumeral[val];
      
      (* Return the difference in string lengths *)
      StringLength[originalStr] - StringLength[minimalStr]
    ]
  ];

  (* Execute in parallel across the dataset *)
  savedChars = ParallelMap[processNumeral, data];

  (* Aggregate the total savings *)
  Total[savedChars]
]

solve[]