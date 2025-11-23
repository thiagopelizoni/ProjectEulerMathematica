(* Project Euler Problem 48: Self Powers

   Problem Description:
   The series is defined as $S_N = \sum_{n=1}^{N} n^n$.
   The objective is to determine the last 10 digits of the sum for $N = 1000$.
   This corresponds to finding the value of $S_{1000} \pmod{10^{10}}$.

   Mathematical Solution:
   1. Modular Arithmetic Properties:
      To find the last $d$ digits of an integer, we compute the residue modulo $10^d$.
      The property of linearity of congruences states:
      $\left( \sum_{i=1}^k a_i \right) \pmod m \equiv \left( \sum_{i=1}^k (a_i \pmod m) \right) \pmod m$.

   2. Computational Strategy (Modular Exponentiation):
      Instead of computing the exact integer value of $S_{1000}$ (which contains roughly 3000 digits) 
      and then truncating, we compute each term $n^n \pmod{10^{10}}$ individually using the 
      binary exponentiation algorithm (square-and-multiply). This ensures all intermediate 
      calculations remain within the bounds of 64-bit or standard arbitrary-precision integers, 
      drastically reducing memory usage and CPU cycles.
*)

limit = 1000;
digits = 10;
modulus = 10^digits;

Mod[Sum[PowerMod[n, n, modulus], {n, 1, limit}], modulus]