(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Simplify`"];
Integreat`Lmm`Simplify::usage = "This package contains functions for simplifying and reducing linear multistep methods.";

LmmReducibleQ::usage = "LmmReducibleQ[lmm] yields True if lmm has a simpler equivalent formulation, and yields False otherwise.";
LmmReduce::usage = "LmmReduce[lmm] returns the simplest equivalent formulation of lmm.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Lmm`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


LmmReducibleQ[lmm_Lmm] := Exponent[PolynomialGCD[LmmAlphaGeneratingPolynomial[lmm, x], LmmBetaGeneratingPolynomial[lmm, x]], x] =!= 0;

LmmReduce[lmm_Lmm] := With[{
		aPoly = LmmAlphaGeneratingPolynomial[lmm, x],
		bPoly = LmmBetaGeneratingPolynomial[lmm, x]
	},
	Lmm @@ PadRight[CoefficientList[{aPoly, bPoly} / PolynomialGCD[aPoly, bPoly], x]]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
