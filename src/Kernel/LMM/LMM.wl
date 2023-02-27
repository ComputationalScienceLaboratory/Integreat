(* ::Package:: *)

BeginPackage["Integreat`LMM`"];


LMM
LMMAdamsBashforth
LMMAdamsMoulton
LMMNystrom
LMMMilneSimpson
LMMBackwardDifferentiationFormula
LMMAlpha
LMMBeta
LMMSteps
LMMAlphaGeneratingPolynomial
LMMBetaGeneratingPolynomial


LMMOrderConditions
LMMOrder
LMMErrorConstant


LMMLinearStabilityPolynomial
LMMLinearStabilityPlot
LMMOrderStarPlot


LMMReduce
LMMReducibleQ


Begin["`Private`"];

Needs["Integreat`Internal`MathUtils`"];

<<Integreat`LMM`Core`;
<<Integreat`LMM`Validation`;
<<Integreat`LMM`LinearStability`;
<<Integreat`LMM`OrderConditions`;
<<Integreat`LMM`Simplify`;

End[];


EndPackage[];
