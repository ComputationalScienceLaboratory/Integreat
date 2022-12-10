(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


RK::squarea = "RK A coefficients must be a square matrix.";
RK[A_, ___] /; !SquareMatrixQ[A] := (Message[RK::squarea]; $Failed);
RK::vector = "RK `1` coefficients must be a vector.";
RK[_, b_, ___] /; !VectorQ[b] := (Message[RK::vector, "b"]; $Failed);
RK[_, _, c_, ___] /; !VectorQ[c] := (Message[RK::vector, "c"]; $Failed);
RK[_, _, _, bHat_, ___] /; !VectorQ[bHat] := (Message[RK::vector, "\!\(\*OverscriptBox[\(b\), \(^\)]\)"]; $Failed);
RK::args = "RK called with `1` arguments; must have an A, b, c, and possibly \!\(\*OverscriptBox[\(b\), \(^\)]\).";
RK[args___] /; Not[3 <= Length[{args}] <= 4] := (Message[RK::args, Length[{args}]]; $Failed);
RK::length = "RK coefficients must have compatible lengths.";
RK[args___] /; ArrayDepth[{args}] != 2 := (Message[RK::length]; $Failed);
