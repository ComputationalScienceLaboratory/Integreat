(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Glm`Validation`"];
Integreat`Glm`Validation::usage = "Package containing functions for validating general linear methods";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Glm`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


Glm::square = "Glm `1` coefficients must be a square matrix.";
Glm[A_, ___] /; !SquareMatrixQ[A] := (Message[Glm::square, "A"]; $Failed);
Glm::matrix = "Glm `1` coefficients must be a matrix.";
Glm[_, B_, ___] /; !MatrixQ[B] := (Message[Glm::matrix, "B"]; $Failed);
Glm[_, _, U_, ___] /; !MatrixQ[U] := (Message[Glm::matrix, "U"]; $Failed);
Glm[_, _, _, V_, ___] /; !SquareMatrixQ[V] := (Message[Glm::square, "V"]; $Failed);
Glm[_, _, _, _, Q_, ___] /; !MatrixQ[Q] := (Message[Glm::matrix, "Q"]; $Failed);
Glm::vectorc = "Glm c coefficients must be a vector.";
Glm[_, _, _, _, _, c_, ___] /; !VectorQ[c] := (Message[Glm::vector]; $Failed);
Glm::args = "Glm called with `1` arguments; must have an A, B, U, V, Q, and c.";
Glm[args___] /; Length[{args}] =!= 6 := (Message[Glm::args, Length[{args}]]; $Failed);
Glm::length = "Glm coefficients must have compatible lengths.";
Glm[A_, B_, U_, V_, Q_, c_] /; !With[{
	s = Length[c],
	r = Length[V]
}, s === Length[A] && Dimensions[B] === {r, s} && Dimensions[U] === {s, r} && Length[Q] === r] := (Message[Glm::length]; $Failed);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
