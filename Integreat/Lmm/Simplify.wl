(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Simplify`"];
Integreat`Lmm`Simplify::usage = "Package containing functions for simplifying and reducing linear multistep methods";

LmmReducibleQ::usage = "Returns True if the linear multistep method has a simpler equivalent formulation and False otherwise";
LmmReduce::usage = "Returns the simplest equivalent formulation of a linear multistep method";
LmmRescale::usage = "Scales both the \[Alpha] and \[Beta] coefficients yielding an equivalent formulation of the linear multistep method";


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

LmmRescale[lmm_Lmm, s_] := Lmm[s * LmmAlpha[lmm], s * LmmBeta[lmm]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
