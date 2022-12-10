(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


LMMReducibleQ[lmm_LMM] := Exponent[PolynomialGCD[LMMAlphaGeneratingPolynomial[lmm, x], LMMBetaGeneratingPolynomial[lmm, x]], x] =!= 0;


LMMReduce[lmm_LMM] := With[{
		aPoly = LMMAlphaGeneratingPolynomial[lmm, x],
		bPoly = LMMBetaGeneratingPolynomial[lmm, x]
	},
	LMM @@ PadRight[CoefficientList[{aPoly, bPoly} / PolynomialGCD[aPoly, bPoly], x]]
];
