(* ::Package:: *)

BeginPackage["Integreat`Internal`MathUtils`"];


Integreat`Internal`MathUtils::usage = "Package containing math utilities";
BlockDiag::usage = "Constructs a block diagonal matrix";
SeriesVander::usage = "Constructs scaled Vandermonde matrix ";


Begin["`Private`"];


BlockDiag[m1_, m2_] := ArrayFlatten[{{m1, 0}, {0, m2}}];

SeriesVander[x_List, n_Integer?NonPositive] := ConstantArray[n + 1, Length[x]];
SeriesVander[x_List, n_Integer] := x^n / n!;
SeriesVander[x_List, s_Integer, e_Integer] := Transpose[Table[SeriesVander[x, i],{i, s, e}]];


End[];
EndPackage[];