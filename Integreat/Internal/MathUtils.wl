(* ::Package:: *)

BeginPackage["Integreat`Internal`MathUtils`"];


Integreat`Internal`MathUtils::usage = "Package containing math utilities";
BlockDiag::usage = "Constructs a block diagonal matrix";
SeriesVander::usage = "Constructs scaled Vandermonde matrix ";
CompanionMatrix::usage = "Creats a companion matrix";
CountZeros::usage = "";


Begin["`Private`"];


BlockDiag[m1_, m2_] := ArrayFlatten[{{m1, 0}, {0, m2}}];

SeriesVander[x_List, n_Integer?NonPositive] := ConstantArray[n + 1, Length[x]];
SeriesVander[x_List, n_Integer] := x^n / n!;
SeriesVander[x_List, s_Integer, e_Integer] := Transpose[Table[SeriesVander[x, i], {i, s, e}]];

CompanionMatrix[{x_}] := {{x}};
CompanionMatrix[x_List] := Append[ArrayFlatten[{{0, IdentityMatrix[Length[x] - 1]}}], x];

CountZeros[test_, min_:1, max_:Infinity] := NestWhile[# + 1 &, 0, And @@ PossibleZeroQ[test[#]] &, 1, max];


End[];
EndPackage[];
