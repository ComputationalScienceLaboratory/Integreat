(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


GLM::square = "GLM `1` coefficients must be a square matrix.";
GLM[A_, ___] /; !SquareMatrixQ[A] := (Message[GLM::square, "A"]; $Failed);
GLM::matrix = "GLM `1` coefficients must be a matrix.";
GLM[_, B_, ___] /; !MatrixQ[B] := (Message[GLM::matrix, "B"]; $Failed);
GLM[_, _, U_, ___] /; !MatrixQ[U] := (Message[GLM::matrix, "U"]; $Failed);
GLM[_, _, _, V_, ___] /; !SquareMatrixQ[V] := (Message[GLM::square, "V"]; $Failed);
GLM[_, _, _, _, Q_, ___] /; !MatrixQ[Q] := (Message[GLM::matrix, "Q"]; $Failed);
GLM::vectorc = "GLM c coefficients must be a vector.";
GLM[_, _, _, _, _, c_, ___] /; !VectorQ[c] := (Message[GLM::vector]; $Failed);
GLM::args = "GLM called with `1` arguments; must have an A, B, U, V, Q, and c.";
GLM[args___] /; Length[{args}] =!= 6 := (Message[GLM::args, Length[{args}]]; $Failed);
GLM::length = "GLM coefficients must have compatible lengths.";
GLM[A_, B_, U_, V_, Q_, c_] /; !With[{
	s = Length[c],
	r = Length[V]
}, s === Length[A] && Dimensions[B] === {r, s} && Dimensions[U] === {s, r} && Length[Q] === r] := (Message[GLM::length]; $Failed);
