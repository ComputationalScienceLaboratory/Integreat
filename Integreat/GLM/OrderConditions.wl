(* ::Package:: *)

BeginPackage["Integreat`GLM`OrderConditions`", {"Integreat`Internal`MathUtils`", "Integreat`GLM`Methods`"}];


Integreat`GLM`OrderConditions::usage = "Package containing functions for determining the order of general linear methods";

GlmPreconsistencyCondition::usage = "?";
GlmOrderCondition::usage = "?";


Begin["`Private`"];


GlmPreconsistencyCondition[glm_Glm] := With[{
		q0 = GlmQ[glm][[All, 1]]
	},
	Map[# == 0 &, {
		GlmU[glm].q0 - 1,
		GlmV[glm].q0 - q0
	}, {2}]
];

GlmOrderCondition[glm_Glm, q_Integer, \[Omega]_:1] /; (GlmOrder[glm] - 1 <= q <= GlmOrder[glm] <= q + 1) := With[{
		C = SeriesVander[GlmC[glm], -1, GlmOrder[glm]],
		\[CapitalOmega] = DiagonalMatrix[\[Omega]^Range[0, GlmOrder[glm]]],
		p = GlmOrder[glm]
	},
	Map[# == 0 &, {
		C[[All, 2;;q+2]] - GlmA[glm].C[[All, 1;;q+1]] - GlmU[glm].GlmQ[glm][[All, 1;;q+1]].\[CapitalOmega][[1;;q+1, 1;;q+1]],
		GlmQ[glm].Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, p}, {j, 0, p}] - GlmB[glm].C[[All, 1;;p+1]] - GlmV[glm].GlmQ[glm].\[CapitalOmega]
	}, {3}]
];

(*GlmErrorA[glm_Glm, q_Integer] /; (GlmOrder[glm] - 1 <= q <= GlmOrder[glm] <= q + 1) := With[{
		p = GlmOrder[glm]
	},
	Norm[Join[
		GlmQ[] - GlmB[glm].GlmC[glm]^p / p!,
		GlmB[glm].(GlmC[glm]^p / p! - GlmA[glm].GlmC[glm]^(p - 1) / Factorial[p - 1] - GlmU[glm].GlmQ[glm][[All, p + 1]]
	]]
];*)


End[];
EndPackage[];
