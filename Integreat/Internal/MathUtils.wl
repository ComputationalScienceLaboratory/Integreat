(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Internal`MathUtils`"];
Integreat`Internal`MathUtils::usage = "This internal package contains math utilities.";

BlockDiag::usage = "BlockDiag[m1, m2] creates a matrix with m1 and m2 as diagonal blocks.";
SafePow::usage = "SafePow[x, y] computes x^y and defines 0^0 = 1."
SeriesVander::usage = "SeriesVander[x, s, e] creates a scaled Vandermonde matrix using x and powers s to e.";
CompanionMatrix::usage = "CompanionMatrix[x] creats a companion matrix with x along the bottom row.";
CountZeros::usage = "CountZeros[test] returns the smallest integer for which test is nonzero.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


(* ::Section:: *)
(*Package Definitions*)


BlockDiag[m1_, m2_] := ArrayFlatten[{{m1, 0}, {0, m2}}];

SetAttributes[SafePow, Listable];
SafePow[x_, _?PossibleZeroQ] := 1;
SafePow[x_, y_] := x^y;

SeriesVander[x_List, n_Integer?NonPositive] := ConstantArray[n + 1, Length[x]];
SeriesVander[x_List, n_Integer] := x^n / n!;
SeriesVander[x_List, s_Integer, e_Integer] := Transpose[Table[SeriesVander[x, i], {i, s, e}]];

CompanionMatrix[{x_}] := {{x}};
CompanionMatrix[x_List] := Append[ArrayFlatten[{{0, IdentityMatrix[Length[x] - 1]}}], x];

CountZeros[test_, min_:1, max_:Infinity] := NestWhile[# + 1 &, min, And @@ PossibleZeroQ[test[#]] &, 1, max];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
