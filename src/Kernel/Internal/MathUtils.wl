(* ::Package:: *)

BeginPackage["Integreat`Internal`MathUtils`"];


BlockDiag
ZeroQ
SafePow
SeriesVDM
CompanionMatrix
CountZeros


Begin["`Private`"];


(* TODO: replace with BlockDiagonalMatrix when min version >= 13.1 *)
BlockDiag[m1_, m2_] := ArrayFlatten[{{m1, 0}, {0, m2}}];


ZeroQ[expr_] := PossibleZeroQ[expr, Method -> "ExactAlgebraics"];


SetAttributes[SafePow, Listable];
SafePow[x_, _?ZeroQ] := 1;
SafePow[x_, y_] := x^y;


(* TODO: use VandermondeMatrix when min version >= 13.2 *)
SeriesVDM[x_List, n_Integer?NonPositive] := ConstantArray[n + 1, Length[x]];
SeriesVDM[x_List, n_Integer] := x^n / n!;
SeriesVDM[x_List, s_Integer, e_Integer] := Transpose[Table[SeriesVDM[x, i], {i, s, e}]];


CompanionMatrix[{x_}] := {{x}};
CompanionMatrix[x_List] := Append[ArrayFlatten[{{0, IdentityMatrix[Length[x] - 1]}}], x];


CountZeros[test_, min_:1, max_:Infinity] := NestWhile[# + 1 &, min, And @@ ZeroQ[test[#]] &, 1, max];


End[];


EndPackage[];
