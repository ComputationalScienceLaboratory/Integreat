(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`GLM`LinearStability`"];

GLMLinearStability::usage = "GLMLinearStability[rk, z] evaluates the linear stability matrix of glm at z=h*\[Lambda]. If z is a DirectedInfinity, then the value in the limit is returned.";
GLMLinearStabilityPolynomial::usage = "GLMLinearStabilityPolynomial[glm, w, z] creates a polynomial in w, parameterized by z=h*\[Lambda], that determines the linear stability of glm.";
GLMLinearStabilityPlot::usage =
	"GLMLinearStabilityPlot[glm] plots the linear stability region of glm.\n" <>
	"GLMLinearStabilityPlot[glm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"GLMLinearStabilityPlot[glm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";
GLMOrderStarPlot::usage =
	"GLMOrderStarPlot[glm] plots the order star of glm.\n" <>
	"GLMOrderStarPlot[glm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"GLMOrderStarPlot[glm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`GLM`Core`"];

stabilityPlot[glm_, ref_, opts___] := With[{
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
] := stabilityPlot[glm, Exp[Re[z]], {z, zMin, zMax}, opts];

GLMLinearStabilityPlot[
	glm_GLM,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, {-6 - 4I, 2 + 4I}],
	opts:OptionsPattern[ComplexRegionPlot]
] := stabilityPlot[glm, 1, {z, zMin, zMax}, opts];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
