(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Internal`MathUtils`"];

BlockDiag::usage = "BlockDiag[m1, m2] creates a matrix with m1 and m2 as diagonal blocks.";
ZeroQ::usage = "ZeroQ[expr] uses PossibleZeroQ to test if expr is zero";
SafePow::usage = "SafePow[x, y] computes x^y and defines 0^0 = 1."
SeriesVDM::usage = "SeriesVDM[x, s, e] creates a scaled Vandermonde matrix using x and powers s to e.";
CompanionMatrix::usage = "CompanionMatrix[x] creats a companion matrix with x along the bottom row.";
CountZeros::usage = "CountZeros[test] returns the smallest integer for which test is nonzero.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


(* ::Section:: *)
(*Package Definitions*)


(* TODO: replace with BlockDiagonalMatrix when min version >= 13.1 *)
BlockDiag[m1_, m2_] := ArrayFlatten[{{m1, 0}, {0, m2}}];


ZeroQ[expr_] := PossibleZeroQ[expr, Method -> "ExactAlgebraics"];


SetAttributes[SafePow, Listable];
SafePow[x_, _?ZeroQ] := 1;
SafePow[x_, y_] := x^y;


SeriesVDM[x_List, n_Integer?NonPositive] := ConstantArray[n + 1, Length[x]];
SeriesVDM[x_List, n_Integer] := x^n / n!;
SeriesVDM[x_List, s_Integer, e_Integer] := Transpose[Table[SeriesVDM[x, i], {i, s, e}]];


CompanionMatrix[{x_}] := {{x}};
CompanionMatrix[x_List] := Append[ArrayFlatten[{{0, IdentityMatrix[Length[x] - 1]}}], x];


CountZeros[test_, min_:1, max_:Infinity] := NestWhile[# + 1 &, min, And @@ ZeroQ[test[#]] &, 1, max];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];