(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GLM`OrderConditions`"];


CSL`OdeUtils`GLM`OrderConditions::usage = "Package containing functions for determining the order of general linear methods";

GlmPreconsistencyCondition::usage = "?";
GlmOrderCondition::usage = "?";


Begin["`Private`"];
Needs["CSL`OdeUtils`GLM`Methods`"];


GlmPreconsistencyCondition[glm_Glm] := With[{
	w0 = Table[Subscript[\[FormalW], i, 0], {i, GlmExternalStages[glm]}]
},
	Thread[Flatten[{
		GlmU[glm].w0 - 1,
		GlmV[glm].w0 - w0
	}] == 0]
];

GlmOrderCondition[glm_Glm, p_Integer, q_Integer] := With[{
		W = Table[Subscript[\[FormalW], i, j], {i, GlmExternalStages[glm]}, {j, 0, p}],
		C = Transpose[Prepend[Table[GlmC[glm]^i / Factorial[i], {i, p}], ConstantArray[1, GlmInternalStages[glm]]]],
		mu = Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, p}, {j, p}]
	},
	Thread[Flatten[{
		C[[All, 2;;q+1]] - GlmA[glm].C[[All, 1;;q]] - GlmU[glm].W[[All, 2;;q+1]],
		W[[All, 1;;p+1]].mu - GlmB[glm].C[[All, 1;;p]] - GlmV[glm].W[[All, 2;;p+1]]
	}] == 0]
];


End[];


EndPackage[];
