(* ::Package:: *)

BeginPackage["Integreat`LMM`"];


LMM::usage =
	"LMM[k] creates a generic, k-step linear multistep method\n" <>
	"LMM[{\[Alpha]0, \[Alpha]1, \[Ellipsis], \[Alpha]k}, {\[Beta]0, \[Beta]1, \[Ellipsis], \[Beta]k}] creates a k-step linear multistep method where the \[Alpha] coefficients multiply y's and the \[Beta] coefficients multiply f(y)'s. Coefficients should be ordered from oldest to newest.";
LMMAdamsBashforth::usage = "LMMAdamsBashforth[k] creates a k-step Adams-Bashforth method.";
LMMAdamsMoulton::usage = "LMMAdamsMoulton[k] creates a k-step Adams-Moulton method.";
LMMNystrom::usage = "LMMNystrom[k] creates a k-step Nystr\[ODoubleDot]m method.";
LMMMilneSimpson::usage = "LMMMilneSimpson[k] creates a k-step Milne-Simpson method.";
LMMBackwardDifferentiationFormula::usage = "LMMBackwardDifferentiationFormula[k] creates a k-step backward differentiation formula method.";
LMMAlpha::usage = "LMMAlpha[lmm] gets the list of coefficients multiplying y's from lmm. They are ordered from oldest to newest.";
LMMBeta::usage = "LMMBeta[lmm] gets the list of coefficients multiplying f(y)'s from lmm. They are ordered from oldest to newest.";
LMMSteps::usage = "LMMSteps[lmm] returns the number of previous steps required to compute the next step for lmm.";
LMMAlphaGeneratingPolynomial::usage = "LMMAlphaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Alpha] coefficients of lmm.";
LMMBetaGeneratingPolynomial::usage = "LMMBetaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Beta] coefficients of lmm.";


LMMOrderConditions::usage = "LMMOrderConditions[lmm, p] returns a list of order condition residuals for lmm from order 0 to p.  If a residual is zero, the corresponding order condition is satisfied.";
LMMOrder::usage = "LMMOrder[lmm] determines the order of lmm by evaluating order conditions.";
LMMErrorConstant::usage =
	"LMMErrorConstant[lmm] computes the leading error constant of lmm.\n" <>
	"LMMErrorConstant[lmm, p] computes the error constant of lmm assuming it is order p";


LMMLinearStabilityPolynomial::usage = "LMMLinearStabilityPolynomial[lmm, \[Zeta], \[Mu]] creates a polynomial in \[Zeta], parameterized by \[Mu]=h*\[Lambda], that determines the linear stability of lmm.";
LMMLinearStabilityPlot::usage =
	"LMMLinearStabilityPlot[lmm] plots the linear stability region of lmm.\n" <>
	"LMMLinearStabilityPlot[lmm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"LMMLinearStabilityPlot[lmm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";
LMMOrderStarPlot::usage =
	"LMMOrderStarPlot[lmm] plots the order star of lmm.\n" <>
	"LMMOrderStarPlot[lmm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"LMMOrderStarPlot[lmm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";


LMMReduce::usage = "LMMReduce[lmm] returns the simplest equivalent formulation of lmm.";
LMMReducibleQ::usage = "LMMReducibleQ[lmm] yields True if lmm has a simpler equivalent formulation, and yields False otherwise.";


Begin["`Private`"];

Needs["Integreat`Internal`MathUtils`"];

<<Integreat`LMM`Core`;
<<Integreat`LMM`Validation`;
<<Integreat`LMM`LinearStability`;
<<Integreat`LMM`OrderConditions`;
<<Integreat`LMM`Simplify`;

End[];


EndPackage[];
