(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


LMM::vector = "LMM `1` coefficients must be a vector.";
LMM[a_, _] /; !VectorQ[a] := (Message[LMM::vector, "\[Alpha]"]; $Failed);
LMM[_, b_] /; !VectorQ[b] := (Message[LMM::vector, "\[Beta]"]; $Failed);
LMM::args = "LMM called with `1` arguments; must have an \[Alpha] and \[Beta].";
LMM[args___] /; Length[{args}] =!= 2 := (Message[LMM::args, Length[{args}]]; $Failed);
LMM::length = "LMM coefficients must have the same lengths.";
LMM[a_, b_] /; Length[a] =!= Length[b] := (Message[LMM::length]; $Failed);
LMM::solvable = "The last \[Alpha] coefficient must be nonzero.";
LMM[a_, _] /; ZeroQ[Last[a]] := (Message[LMM::solvable]; $Failed);
