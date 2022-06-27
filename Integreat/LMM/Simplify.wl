(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`LMM`Simplify`"];

LMMReducibleQ::usage = "LMMReducibleQ[lmm] yields True if lmm has a simpler equivalent formulation, and yields False otherwise.";
LMMReduce::usage = "LMMReduce[lmm] returns the simplest equivalent formulation of lmm.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`LMM`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


LMMReducibleQ[lmm_LMM] := Exponent[PolynomialGCD[LMMAlphaGeneratingPolynomial[lmm, x], LMMBetaGeneratingPolynomial[lmm, x]], x] =!= 0;

LMMReduce[lmm_LMM] := With[{
		aPoly = LMMAlphaGeneratingPolynomial[lmm, x],
		bPoly = LMMBetaGeneratingPolynomial[lmm, x]
	},
	LMM @@ PadRight[CoefficientList[{aPoly, bPoly} / PolynomialGCD[aPoly, bPoly], x]]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
