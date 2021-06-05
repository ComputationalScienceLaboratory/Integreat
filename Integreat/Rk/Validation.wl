(* ::Package:: *)

BeginPackage["Integreat`Rk`Validation`"];
Integreat`Rk`Validation::usage = "Package containing functions for validating Runge-Kutta methods";


Begin["`Private`"];
Needs["Integreat`Rk`Methods`"];


Rk::args = "Rk called with `1` arguments; must have an A, b, c, and possibly \!\(\*OverscriptBox[\(b\), \(^\)]\).";
Rk[args___] /; Not[3 <= Length[{args}] <= 4] := (Message[Rk::args, Length[{args}]]; $Failed);
Rk::squarea = "Rk A coefficients must be a square matrix.";
Rk[A_, __] /; !SquareMatrixQ[A] := (Message[Rk::squarea]; $Failed);
Rk::vector = "Rk `1` coefficients must be a vector.";
Rk[_, b_, __] /; !VectorQ[b] := (Message[Rk::vector, "b"]; $Failed);
Rk[_, _, c_, ___] /; !VectorQ[c] := (Message[Rk::vector, "c"]; $Failed);
Rk[_, _, _, bHat_] /; !VectorQ[bHat] := (Message[Rk::vector, "\!\(\*OverscriptBox[\(b\), \(^\)]\)"]; $Failed);
Rk::length = "Rk coefficients must have compatible lengths.";
Rk[args___] /; ArrayDepth[{args}] != 2 := (Message[Rk::length]; $Failed);


End[];
EndPackage[];
