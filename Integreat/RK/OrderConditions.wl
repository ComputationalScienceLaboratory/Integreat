(* ::Package:: *)

BeginPackage["Integreat`RK`OrderConditions`"];


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


Begin["`Private`"];
Scan[Needs, {
	"Integreat`RK`Methods`",
	"Integreat`RK`LinearStability`",
	"Integreat`Internal`MathUtils`",
	"Integreat`BTrees`"
}];

SetAttributes[rkOrderConditions, Listable];
rkOrderConditions[rk_, t_, opts___] := (phi[First[t], RKA[rk], RKB[rk, opts], RKC[rk]] - one[BTreeOrder[t], rk, opts] / BTreeGamma[t]) / BTreeSigma[t];

SetAttributes[one, HoldFirst];
one[p_, rk_, OptionsPattern[RKB]] := one[p, rk, OptionValue[Embedded], OptionValue[Stage], OptionValue[DenseOutput]];
one[_, _, True, _, _] := 1;
one[p_, rk_ , False, i_Integer, _] := SafePow[RKC[rk][[i]], p];
one[p_, _, False, None, True] := SafePow[\[FormalTheta], p];
one[p_, _, False, None, False] := 1;
one[p_, _, False, None, do_] := SafePow[do, p];

phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];

phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];

errOrder[err_?ZeroQ, _] := Infinity;
errOrder[err_, y_] := CountZeros[SeriesCoefficient[err, {y, 0, #}] &] - 1;


RKOrderConditions[rk_RK, p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RKB]] := rkOrderConditions[rk, BTree[p], opts];
RKOrderConditions[rk_RK, t_BTree, opts:OptionsPattern[RKB]] := rkOrderConditions[rk, t, opts];

RKSimplifyingAssumptionB[rk_RK, {p_Integer?Positive}, opts:OptionsPattern[RKB]] := RKB[rk, opts] . SafePow[RKC[rk], p - 1] - one[p, rk, opts] / p;
RKSimplifyingAssumptionB[rk_RK, p_Integer?Positive, opts:OptionsPattern[RKB]] := Table[RKSimplifyingAssumptionB[rk, {k}, opts], {k, p}];

Options[RKSimplifyingAssumptionC] = {Stage -> All};
RKSimplifyingAssumptionC[rk_RK, {eta_Integer?Positive}, OptionsPattern[]] := With[{
		stages = OptionValue[Stage],
		c = RKC[rk]
	},
	RKA[rk][[stages]] . SafePow[c, eta - 1] - SafePow[c[[stages]], eta] / eta
];
RKSimplifyingAssumptionC[rk_RK, eta_Integer?Positive, opts:OptionsPattern[]] := Table[RKSimplifyingAssumptionC[rk, {k}, opts], {k, eta}];

RKSimplifyingAssumptionD[rk_RK, {zeta_Integer?Positive}, opts:OptionsPattern[RKB]] := With[{
		b = RKB[rk, opts],
		c = RKC[rk]
	},
	(b * SafePow[c, zeta - 1]) . RKA[rk] - b * (one[zeta, rk, opts] - SafePow[c, zeta]) / zeta
];
RKSimplifyingAssumptionD[rk_RK, zeta_Integer?Positive, opts:OptionsPattern[RKB]] := Table[RKSimplifyingAssumptionD[rk, {k}, opts], {k, zeta}];

RKOrder[rk_RK, opts:OptionsPattern[RKB]] := CountZeros[RKOrderConditions[rk, {#}, opts] &] - 1;

RKExtrapolation[m_RK, steps_/;VectorQ[steps, Positive] && DuplicateFreeQ[steps], j:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RKOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(j * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];

RKErrorA[rk_RK, p_Integer?NonNegative, opts:OptionsPattern[RKB]] := Norm[RKOrderConditions[rk, {p}, opts]];
RKErrorA[rk_RK, opts:OptionsPattern[RKB]] := RKErrorA[rk, RKOrder[rk] + 1, opts];

RKErrorB[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat, Embedded -> True] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorB[rk_?RKPairQ] := RKErrorB[rk, RKOrder[rk] + 1];

RKErrorC[rk_?RKPairQ, pHat_Integer?Positive] := Norm[
		RKOrderConditions[rk, {pHat}] - RKOrderConditions[RKEmbedded[rk], {pHat}]
	] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorC[rk_?RKPairQ] := RKErrorC[rk, RKOrder[rk] + 1];

RKErrorD[rk_RK] := Max[Abs[List @@ rk]];

RKErrorE[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorE[rk_?RKPairQ] := RKErrorE[rk, RKOrder[rk] + 1];

RKDispersionError[rk_RK, y_, opts:OptionsPattern[RKB]] := one[1, rk, opts] * y - ComplexExpand[Arg[RKLinearStability[rk, y * I, opts]], TargetFunctions -> {Re, Im}];

RKDispersionOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDispersionError[rk, y, opts], y];

RKDissipationError[rk_RK, y_, opts:OptionsPattern[RKB]] := 1 - ComplexExpand[Abs[RKLinearStability[rk, y * I, opts]]];

RKDissipationOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDissipationError[rk, y, opts], y];


End[];
EndPackage[];
