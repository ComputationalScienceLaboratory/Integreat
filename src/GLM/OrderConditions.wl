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
		C = Transpose[Prepend[Table[GlmC[glm]^i / i!, {i, GlmOrder[glm]}], ConstantArray[1, GlmInternalStages[glm]]]],
		mu = Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, GlmOrder[glm]}, {j, GlmOrder[glm]}],
		p = GlmOrder[glm]
	},
	Thread[Flatten[{
		C[[All, 2;;q+1]] - GlmA[glm].C[[All, 1;;q]] - GlmU[glm].GlmQ[glm][[All, 2;;q+1]],
		GlmQ[glm][[All, 1;;p+1]].mu - GlmB[glm].C[[All, 1;;p]] - GlmV[glm].GlmQ[glm][[All, 2;;p+1]]
	}] == 0]
];


End[];


EndPackage[];
