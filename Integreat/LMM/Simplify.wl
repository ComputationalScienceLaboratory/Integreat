(* ::Package:: *)

BeginPackage["Integreat`LMM`Simplify`", {"Integreat`LMM`Methods`"}];


Integreat`LMM`Simplify::usage = "Package containing functions for simplifying and reducing linear multistep methods";

LmmReducibleQ::usage = "Returns True if the linear multistep method has a simpler equivalent formulation and False otherwise";
LmmReduce::usage = "Returns the simplest equivalent formulation of a linear multistep method";
LmmRescale::usage = "Scales both the \[Alpha] and \[Beta] coefficients yielding an equivalent formulation of the linear multistep method";


Begin["`Private`"];


LmmReducibleQ[lmm_Lmm] := Exponent[PolynomialGCD[LmmAlphaGeneratingPolynomial[lmm, x], LmmBetaGeneratingPolynomial[lmm, x]], x] =!= 0;

LmmReduce[lmm_Lmm] := With[{
		aPoly = LmmAlphaGeneratingPolynomial[lmm, x],
		bPoly = LmmBetaGeneratingPolynomial[lmm, x],
		len = Length[LmmAlpha[lmm]]
	},
	Lmm @@ Map[PadRight[CoefficientList[#, x], len] &, {aPoly, bPoly} / PolynomialGCD[aPoly, bPoly]]
];

LmmRescale[lmm_Lmm, s_] := Lmm[s * LmmAlpha[lmm], s * LmmBeta[lmm]];


End[];
EndPackage[];
