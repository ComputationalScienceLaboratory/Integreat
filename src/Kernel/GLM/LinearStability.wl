(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


glmStabilityPlot[glm_, ref_, opts___] := With[{
		stab = Norm[Eigenvalues[GLMLinearStability[glm, z]], Infinity] <= ref
	},
	ComplexRegionPlot[stab, opts, FrameLabel -> {"Re", "Im"}]
];


(* ::Section:: *)
(*Package Definitions*)


GLMLinearStability[glm_GLM, lim_DirectedInfinity] := Limit[GLMLinearStability[glm, z], z -> lim];
GLMLinearStability[glm_GLM, z_] := GLMV[glm] + z * GLMB[glm] . Inverse[IdentityMatrix[GLMInternalStages[glm]] - z * GLMA[glm]] . GLMU[glm];


GLMLinearStabilityPolynomial[glm_GLM, w_, z_] := Det[w * IdentityMatrix[GLMExternalStages[glm]] - GLMLinearStability[glm, z]];


GLMOrderStarPlot[
	glm_GLM,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, 4],
	opts:OptionsPattern[ComplexRegionPlot]
] := glmStabilityPlot[glm, Exp[Re[z]], {z, zMin, zMax}, opts];


GLMLinearStabilityPlot[
	glm_GLM,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, {-6 - 4I, 2 + 4I}],
	opts:OptionsPattern[ComplexRegionPlot]
] := glmStabilityPlot[glm, 1, {z, zMin, zMax}, opts];
