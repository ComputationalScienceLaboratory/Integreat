(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Glm`OrderConditions`"];
Integreat`Glm`OrderConditions::usage = "Package containing functions for determining the order of general linear methods";

GlmPreconsistencyConditions::usage = "?";
GlmOrderConditions::usage = "?";
GlmOrder::usage = "?";
GlmStageOrder::usage = "?";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`Glm`Methods`"}];

glmLteOrder[glm_Glm] := With[{
		p = GlmP[glm],
		c = GlmC[glm],
		Q = GlmQ[glm]
	},
	{
		CountZeros[SeriesVander[c, #] - GlmA[glm].SeriesVander[c, # - 1] - GlmU[glm].Q[[All, # + 1]] &, 0, p + 1],
		CountZeros[Q.Table[If[# < i, 0, 1 / Factorial[# - i]], {i, 0, p}] - GlmB[glm].SeriesVander[c, # - 1] - GlmV[glm].Q[[All, # + 1]] &, 0, p + 1]
	}
];


(* ::Section:: *)
(*Package Definitions*)


GlmPreconsistencyConditions[glm_Glm] := With[{
		q0 = GlmQ[glm][[All, 1]]
	},
	{
		GlmU[glm].q0 - 1,
		GlmV[glm].q0 - q0
	}
];

GlmOrderConditions[glm_Glm] := GlmOrderConditions[glm, GlmP[glm]];
GlmOrderConditions[glm_Glm, q_Integer] := GlmOrderConditions[glm, {GlmP[glm], q}];
GlmOrderConditions[glm_Glm, {p_Integer, q_Integer}] /; p <= GlmP[glm] && (p == q || p == q + 1) := With[{
		C = SeriesVander[GlmC[glm], -1, p],
		Q = GlmQ[glm]
	},
	Flatten[{
		C[[All, 2;;q+2]] - GlmA[glm].C[[All, 1;;q+1]] - GlmU[glm].Q[[All, 1;;q+1]],
		Q.Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, p}, {j, 0, p}] - GlmB[glm].C[[All, 1;;p+1]] - GlmV[glm].Q
	}, {{3}, {1}, {2}}]
];

GlmStageOrder[glm_Glm] := Min[glmLteOrder[glm] - 1];

GlmOrder[glm_Glm] := Min[glmLteOrder[glm] - {0, 1}];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
