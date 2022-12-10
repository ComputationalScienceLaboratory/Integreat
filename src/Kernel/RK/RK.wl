(* ::Package:: *)

BeginPackage["Integreat`RK`"];


RK::usage =
	"RK[] returns a Dataset containing a catalog of Runge-Kutta methods.\n" <>
	"RK[\"name\"] retreives the method named name from the catalog.\n" <>
	"RK[s] constructs a generic s-stage Runge-Kutta method.\n" <>
	"RK[A] constructs a Runge-Kutta method with b coefficients taken to be the last row of A and c coefficients that are the row sum of A.\n" <>
	"RK[A, b] constructs a Runge-Kutta method with c coefficients that are the row sum of A.\n" <>
	"RK[A, b, c] constructions a Runge-Kutta method with coefficients A, b, and c.\n" <>
	"RK[A, b, c, bHat] constructs an embedded Runge-Kutta pair.\n" <>
	"RK[rk, bHat] adds embedded coefficients bHat to rk."
RKRescale::usage = "RKRescale[rk, x] scales all coefficients of rk by x.";
RKType::usage = "RKType[rk] returns a string representation of the tableau type of rk.";
RKPrimary::usage = "RKPrimary[rk] creates a copy of rk without an embedded method.";
RKEmbedded::usage = "RKEmbedded[rk] returns the embedded method for the embedded pair rk.";
RKPairQ::usage = "RKPairQ[rk] returns True if rk is an embedded Runge-Kutta pair and False, otherwise.";
RKCollocation::usage = "RKCollocation[c] constructs a collocation method with abscissae c.";
RKCompose::usage =
	"RKCompose[rk1, \[Ellipsis], rkm] creates a Runge-Kutta method from a step of rk1, \[Ellipsis], rkm in sequence using a step size h/m.\n" <>
	"RKCompose[{rk1, w1}, \[Ellipsis], {rkm, wm}] composes rk1, \[Ellipsis], rkm using step sizes w1*h, \[Ellipsis], wm*h, respectively.\n" <>
	"rk1[rk2] composes a half step of rk1 with a half step of rk2.\n" <>
	"rk^p composes p steps of rk."
RKA::usage = "RKA[rk] returns the A coefficient matrix of rk.";
RKDenseOutput::usage = "RKDenseOutput[rk] returns the dense output coefficients of rk paramtereized by \[FormalTheta].";
RKB::usage = "RKB[rk] returns the b weights of rk.";
RKC::usage = "RKC[rk] returns the abscissae of rk.";
RKBHat::usage = "RKBHat[rk] returns the embedded coefficients of an embedded pair rk.";
RKStages::usage = "RKStages[rk] returns the number of stages of rk.";


RKLinearStability::usage =
	"RKLinearStability[rk, z] evaluates the linear stability function of rk at z=h*\[Lambda]. If z is a DirectedInfinity, then the value in the limit is returned.\n" <>
	"RKLinearStability[rk, z, Stage \[Rule] All] evaluates the internal stability function of rk at z.\n";
RKLinearStabilityPlot::usage =
	"RKLinearStabilityPlot[rk] plots the linear stability region of rk.\n" <>
	"RKLinearStabilityPlot[rk, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"RKLinearStabilityPlot[rk, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";
RKOrderStarPlot::usage =
	"RKOrderStarPlot[rk] plots the order star of rk.\n" <>
	"RKOrderStarPlot[rk, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"RKOrderStarPlot[rk, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";
RKLinearStabilityP::usage = "RKLinearStabilityP[rk, z] evaluates the numerator of the linear stability function of rk at z.";
RKLinearStabilityQ::usage = "RKLinearStabilityQ[rk, z] evaluates the denominator of the linear stability function of rk at z.";
RKEPolynomial::usage = "RKEPolynomial[rk, y] computes |Q(y*I)|^2-|P(y*I)|^2 where Q and P are the denominator and numerator, respectively, of the linear stability function of rk.";
RKIStableCondition::usage = "RKIStableCondition[rk] returns an algebraic condition equivalent to rk being stable on the imaginary axis.";
RKAStableCondition::usage = "RKAStableCondition[rk] returns an algebraic condition equivalent to rk being stable in the left half-plane.";
RKStifflyAccurateQ::usage = "RKStifflyAccurateQ[rk] returns True if, for the coefficients of rk, the last row of A equals b. It returns False, otherwise.";


RKAlgebraicStabilityMatrix::usage = "RKAlgebraicStabilityMatrix[rk] computes the algebraic stability matrix of rk.";
RKAlgebraicallyStableQ::usage = "RKAlgebraicallyStableQ[rk] returns True if rk is algebraically stable and False, otherwise.";
RKSymplecticQ::usage = "RKSymplecticQ[rk] returns True if rk is symplectic and False, otherwise.";
RKAbsoluteMonotonicityRadius::usage = "RKAbsoluteMonotonicityRadius[rk] computes the radius of absolute monotonicty (SSP coefficient) of rk.";


RKOrderConditions::usage =
	"RKOrderConditions[rk, p] generates the order condition residuals of rk up to order p grouped by order.\n" <>
	"RKOrderConditions[rk, {p}] generates a list of p-th order residuals of rk.";
RKSimplifyingAssumptionB::usage =
	"RKSimplifyingAssumptionB[rk, p] generates a list of B simplifying assumption residuals up to order p for rk.\n" <>
	"RKSimplifyingAssumptionB[rk, {p}] generates only the residual of order p."
RKSimplifyingAssumptionC::usage =
	"RKSimplifyingAssumptionC[rk, p] generates a list of C simplifying assumption residuals up to order p for rk.\n" <>
	"RKSimplifyingAssumptionC[rk, {p}] generates only the residual of order p.";
RKSimplifyingAssumptionD::usage =
	"RKSimplifyingAssumptionD[rk, p] generates a list of D simplifying assumption residuals up to order p for rk.\n" <>
	"RKSimplifyingAssumptionD[rk, {p}] generates only the residual of order p.";
RKOrder::usage = "RKOrder[rk] computes the order of accuracy of rk.";
RKExtrapolation::usage =
	"RKExtrapolation[rk, steps] creates a new Runge-Kutta method which is rk extrapolated using the step sequence steps.\n" <>
	"RKExtrapolation[rk, steps, j] extrapolates assuming rk has an asymptotic error expansion involving only powers of h^j."
RKErrorA::usage =
	"RKErrorA[rk] computes the 2-norm of the leading error residuals.\n" <>
	"RKErrorA[rk, p] computes the 2-norm of the order p residuals."
RKErrorB::usage =
	"RKErrorB[rk] computes the B metric for the quality of the embedded pair rk.\n" <>
	"RKErrorB[rk, p] assumes the order of rk is p."
RKErrorC::usage =
	"RKErrorC[rk] computes the C metric for the quality of the embedded pair rk.\n" <>
	"RKErrorC[rk, p] assumes the order of rk is p."
RKErrorD::usage = "RKErrorD[rk] computes the maximum method coefficient of rk by absolute value";
RKErrorE::usage =
	"RKErrorE[rk] computes the E metric for the quality of the embedded pair rk.\n" <>
	"RKErrorE[rk, p] assumes the order of rk is p."
RKDispersionError::usage = "RKDispersionError[rk, y] returns the phases error of rk applied to y'=I*\[Omega]*y.";
RKDispersionOrder::usage = "RKDispersionOrder[rk] returns the phase error order of rk applied to y'=I*\[Omega]*y.";
RKDissipationError::usage = "RKDissipationError[rk, y] returns the amplification error of rk applied to y'=I*\[Omega]*y.";
RKDissipationOrder::usage = "RKDissipationOrder[rk] returns the amplification error order of rk applied to y'=I*\[Omega]*y.";


RKReflection::usage = "RKReflection[rk] returns a new Runge-Kutta method which is the reflection of rk. ";
RKTranspose::usage = "RKTranspose[rk] returns a new Runge-Kutta method which is the transpose of rk. ";


RKDJReduce::usage = "RKDJReduce[rk] returns a new Runge-Kutta method with the unused stages of rk removed.";
RKDJReducibleQ::usage = "RKDJReducibleQ[rk] returns True if rk has unused stages and False, otherwise";


Begin["`Private`"];

Needs["Integreat`BTrees`"];
Needs["Integreat`Tableaus`"];
Needs["Integreat`Internal`MathUtils`"];
Needs["Integreat`Internal`Catalog`"];
Needs["Integreat`Internal`Composition`"];

<<Integreat`RK`Core`;
<<Integreat`RK`Catalog`;
<<Integreat`RK`Validation`;
<<Integreat`RK`LinearStability`;
<<Integreat`RK`NonlinearStability`;
<<Integreat`RK`OrderConditions`;
<<Integreat`RK`Symmetry`;
<<Integreat`RK`Simplify`;

End[];


EndPackage[];
