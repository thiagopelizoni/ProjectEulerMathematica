(* Problem: https://projecteuler.net/problem=1 *)
(* Soma de todos os múltiplos de 3 ou 5 abaixo de um limite *)

sumOfMultiples[m_, limit_] := Module[{k},
  k = Floor[(limit - 1)/m];
  m * k * (k + 1) / 2
]

compute[limit_] := Module[{total},
  total = sumOfMultiples[3, limit] + sumOfMultiples[5, limit] - sumOfMultiples[15, limit];
  total
]

(* Exemplo de execução *)
compute[1000]