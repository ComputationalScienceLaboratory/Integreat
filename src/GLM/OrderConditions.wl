(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GLM`OrderConditions`"];


CSL`OdeUtils`GLM`OrderConditions::usage = "Package containing functions for determining the order of general linear methods";

GlmPreconsistencyCondition::usage = "?";
GlmOrderCondition::usage = "?";


Begin["`Private`"];
Needs["CSL`OdeUtils`GLM`Methods`"];


GlmPreconsistencyCondition[glm_Glm] := With[{
		q0 = GlmQ[glm][[All, 1]]
	},
	Thread[Flatten[{
		GlmU[glm].q0 - 1,
		GlmV[glm].q0 - q0
	}] == 0]
];

GlmOrderCondition[glm_Glm, q_Integer] /; (GlmOrder[glm] - 1 <= q <= GlmOrder[glm] <= q + 1) := With[{
		C = Table[Switch[j, -1, 0, 0, 1, _, GlmC[glm][[i]]^j / j!], {i, GlmInternalStages[glm]}, {j, -1, GlmOrder[glm]}],
		p = GlmOrder[glm]
	},
	Thread[Flatten[{
		C[[All, 2;;q+2]] - GlmA[glm].C[[All, 1;;q+1]] - GlmU[glm].GlmQ[glm][[All, 1;;q+1]],
		GlmQ[glm].Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, p}, {j, 0, p}] - GlmB[glm].C[[All, 1;;p+1]] - GlmV[glm].GlmQ[glm]
	}] == 0]
];


End[];


EndPackage[];
