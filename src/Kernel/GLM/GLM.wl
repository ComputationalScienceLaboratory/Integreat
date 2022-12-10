(* ::Package:: *)

BeginPackage["Integreat`GLM`"];


GLM::usage =
	"GLM[s, r, p] constructs a generic general linear method with s internal stages, r external stages, and order p.\n" <>
	"GLM[rk, p] converts a Runge-Kutta method rk into a general linear method of order p.\n" <>
	"GLM[rk] converts a Runge-Kutta method rk into a general linear method.\n" <>
	"GLM[lmm] converts a linear multistep method lmm into a general linear method.";
GLMCompose::usage =
	"GLMCompose[glm1, \[Ellipsis], glmm] creates a general linear method from a step of glm1, \[Ellipsis], glmm in sequence using a step size h/m.\n" <>
	"GLMCompose[{glm1, w1}, \[Ellipsis], {glmm, wm}] composes glm1, \[Ellipsis], glmm using step sizes w1*h, \[Ellipsis], wm*h, respectively.\n" <>
	"glm1[glm2] composes a half step of glm1 with a half step of glm2.\n" <>
	"glm^p composes p steps of glm."
GLMDIMSIM::usage =
	"GLMDIMSIM[A, B, v, Q, c] constructs a diagonally implicit multistage integration method from coefficients A, B, v (repeated row of V), Q, and abscissae c.\n" <>
	"GLMDIMSIM[A, v, c] uses A, v, and c then deduces other coefficients from the assumption p=q=r=s.";
GLMPeer::usage = "GLMPeer[B, A, R, c, p] constructs an order p peer method from coefficients B, A, R, and abscissae c.";
GLMOneLeg::usage = "GLMOneLeg[{\[Alpha]0, \[Alpha]1, \[Ellipsis], \[Alpha]k}, {\[Beta]0, \[Beta]1, \[Ellipsis], \[Beta]k}, p] creates a k-step one-leg method.";
GLMParallelEnsemble::usage =
	"GLMParallelEnsemble[c, \[Lambda]] constructs a parallel ensemble method with abscissae c and A=\[Lambda]*I.\n" <>
	"GLMParallelEnsemble[c] constructs an explicit parallel ensemble method.";
GLMA::usage = "GLMA[glm] returns the A coefficient matrix of glm.";
GLMB::usage = "GLMB[glm] returns the B coefficient matrix of glm.";
GLMU::usage = "GLMU[glm] returns the U coefficient matrix of glm.";
GLMV::usage = "GLMV[glm] returns the V coefficient matrix of glm.";
GLMQ::usage = "GLMQ[glm] returns the Q coefficient matrix of glm.";
GLMC::usage = "GLMC[glm] returns the abscissae of glm.";
GLMInternalStages::usage = "GLMInternalStages[glm] returns the number of internal stages in glm.";
GLMExternalStages::usage = "GLMExternalStages[glm] returns the number of external stages in glm.";
GLMP::usage = "GLMP[glm] returns the order to which external stages are expanded for glm.";
GLMType::usage = "GLMType[glm] returns the type number of glm.";
GLMTransform::usage = "GLMTransform[glm, T] Transforms glm into an equivalent form with the matrix T.";


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


GLMPreconsistencyConditions::usage = "GLMPreconsistencyConditions[glm] returns the residuals of the preconsistency conditions for glm.";
GLMOrderConditions::usage =
	"GLMOrderConditions[glm, {p, q}] generates order conditions residuals of glm up to order p and stage order q.\n" <>
	"GLMOrderConditions[glm, q] uses stage order q and p=GLMP[glm].\n" <>
	"GLMOrderConditions[glm] uses p=q=GLMP[glm].";
GLMOrder::usage = "GLMOrder[glm] computes the order of accuracy of glm assumping high stage order.";
GLMStageOrder::usage = "GLMStageOrder[glm] computes the order of accuracy of the internal stages of glm assumping high stage order.";


Begin["`Private`"];

Needs["Integreat`Internal`MathUtils`"];
Needs["Integreat`Internal`Catalog`"];
Needs["Integreat`Internal`Composition`"];
DeclarePackage["Integreat`RK`", {"RK", "RKA", "RKB", "RKC", "RKStages", "RKOrder"}];
DeclarePackage["Integreat`LMM`", {"LMM", "LMMAlpha", "LMMBeta", "LMMSteps"}];

<<Integreat`GLM`Core`;
<<Integreat`GLM`Catalog`;
<<Integreat`GLM`Validation`;
<<Integreat`GLM`LinearStability`;
<<Integreat`GLM`OrderConditions`;

End[];


EndPackage[];
