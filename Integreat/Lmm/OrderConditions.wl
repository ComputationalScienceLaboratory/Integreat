(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`OrderConditions`"];
Integreat`Lmm`OrderConditions::usage = "This package contains functions for determining the convergence order of linear multistep methods.";

LmmOrderConditions::usage = "LmmOrderConditions[lmm, p] returns a list of order condition residuals for lmm from order 0 to p.  If a residual is zero, the corresponding order condition is satisfied.";
LmmOrder::usage = "LmmOrder[lmm] determines the order of lmm by evaluating order conditions.";
LmmErrorConstant::usage =
	"LmmErrorConstant[lmm] computes the leading error constant of lmm.\n" <>
	"LmmErrorConstant[lmm, p] computes the error constant of lmm assuming it is order p";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`Lmm`Methods`"}];


(* ::Section:: *)
(*Package Definitions*)


LmmOrderConditions[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, LmmSteps[lmm]]
	},
	(LmmAlpha[lmm].SeriesVander[i, 0, p] - LmmBeta[lmm].SeriesVander[i, -1, p - 1]) / Last[LmmAlpha[lmm]]
];

LmmOrder[lmm_Lmm] := With[{
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		i = Range[0, LmmSteps[lmm]]
	},
	CountZeros[a.SeriesVander[i, #] - b.SeriesVander[i, # - 1] &, 0] - 1
];

LmmErrorConstant[lmm_Lmm] := LmmErrorConstant[lmm, LmmOrder[lmm] + 1];
LmmErrorConstant[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, LmmSteps[lmm]]
	},
	(LmmAlpha[lmm].SeriesVander[i, p] - LmmBeta[lmm].SeriesVander[i, p - 1]) / LmmBetaGeneratingPolynomial[lmm, 1]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
