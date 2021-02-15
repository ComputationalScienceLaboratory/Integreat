(* ::Package:: *)

BeginPackage["Integreat`RungeKutta`Validation`"];
Integreat`RungeKutta`Validation::usage = "Package containing functions for validating Runge-Kutta methods";


Begin["`Private`"];


RungeKutta::args = "RungeKutta called with `1` arguments; must have an A, b, c, and possibly \!\(\*OverscriptBox[\(b\), \(^\)]\).";
RungeKutta[args___] /; Not[3 <= Length[{args}] <= 4] := (Message[RungeKutta::args, Length[{args}]]; $Failed);
RungeKutta::squarea = "RungeKutta A coefficients must be a square matrix.";
RungeKutta[A_, __] /; !SquareMatrixQ[A] := (Message[RungeKutta::squarea]; $Failed);
RungeKutta::vector = "RungeKutta `1` coefficients must be a vector.";
RungeKutta[_, b_, __] /; !VectorQ[b] := (Message[RungeKutta::vector, "b"]; $Failed);
RungeKutta[_, _, c_, ___] /; !VectorQ[c] := (Message[RungeKutta::vector, "c"]; $Failed);
RungeKutta[_, _, _, bHat_] /; !VectorQ[bHat] := (Message[RungeKutta::vector, "\!\(\*OverscriptBox[\(b\), \(^\)]\)"]; $Failed);
RungeKutta::length = "RungeKutta coefficients must have compatible lengths.";
RungeKutta[args___] /; ArrayDepth[{args}] != 2 := (Message[RungeKutta::length]; $Failed);


End[];
EndPackage[];
