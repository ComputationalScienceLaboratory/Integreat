(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Validation`"];
Integreat`Lmm`Validation::usage = "Package containing functions for validating linear multistep methods";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Lmm`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


Lmm::vector = "Lmm `1` coefficients must be a vector.";
Lmm[a_, _] /; !VectorQ[a] := (Message[Lmm::vector, "\[Alpha]"]; $Failed);
Lmm[_, b_] /; !VectorQ[b] := (Message[Lmm::vector, "\[Beta]"]; $Failed);
Lmm::args = "Lmm called with `1` arguments; must have an \[Alpha] and \[Beta].";
Lmm[args___] /; Length[{args}] =!= 2 := (Message[Lmm::args, Length[{args}]]; $Failed);
Lmm::length = "Lmm coefficients must have the same lengths.";
Lmm[a_, b_] /; Length[a] =!= Length[b] := (Message[Lmm::length]; $Failed);
Lmm::solvable = "The last \[Alpha] coefficient must be nonzero.";
Lmm[a_, _] /; PossibleZeroQ[Last[a]] := (Message[Lmm::solvable]; $Failed);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
