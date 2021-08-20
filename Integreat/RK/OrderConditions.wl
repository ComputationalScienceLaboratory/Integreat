(* ::Package:: *)

BeginPackage["Integreat`RK`OrderConditions`"];


Integreat`RK`OrderConditions::usage = "Package containing functions for determining the order of Runge-Kutta methods";

RKOrderConditions::usage = "?";
RKDaeOrderConditions::usage = "?";
RKTreeOrderConditions::usage = "";
RKSimplifyingAssumptionB::usage = "The Runge-Kutta simplifying assumption B";
RKSimplifyingAssumptionC::usage = "The Runge-Kutta simplifying assumption C";
RKSimplifyingAssumptionD::usage = "The Runge-Kutta simplifying assumption D";
RKOrder::usage = "?";
RKExtrapolation::usage = "?";
RKErrorA::usage = "The 2-norm of the principal error";
RKErrorB::usage = "The ratio of the embedded second error terms' norm to leading error terms' norm";
RKErrorC::usage = "The ratio of the norm of the difference in second error terms to the norm of embedded leading error terms";
RKErrorD::usage = "The maximum entry in the Butcher tableau by absolute value";
RKErrorE::usage = "The ratio of the second error terms' norm to embedded leading error terms' norm";
RKDispersionError::usage = "Dispersion error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RKDispersionOrder::usage = "Dispersion order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RKDissipationError::usage = "Dissipation error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RKDissipationOrder::usage = "Dissipation order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";


Begin["`Private`"];
Scan[Needs, {
	"Integreat`RK`Methods`",
	"Integreat`RK`LinearStability`",
	"Integreat`Internal`MathUtils`",
	"Integreat`BTrees`"
}];

SetAttributes[denseOne, HoldFirst];
denseOne[p_, OptionsPattern[RKB]] := If[TrueQ[OptionValue[DenseOutput]], \[FormalTheta]^p, 1]

orderConditions[r:_[t_], a_, b_, c_, opts:OptionsPattern[RKB]] := (phi[t, a, b, c] - denseOne[BTreeOrder[r], opts] / BTreeGamma[r]) / BTreeSigma[r];

phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];

phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];

errOrder[err_?PossibleZeroQ, _] := Infinity;
errOrder[err_, y_] := CountZeros[SeriesCoefficient[err, {y, 0, #}] &] - 1;


RKOrderConditions[rk: Repeated[_RK, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RKB]] := RKTreeOrderConditions[rk, BTree[p], opts];

RKDaeOrderConditions[rk: Repeated[_RK, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RKB]] := RKTreeOrderConditions[rk, BTreeDiffAlg[p], opts];

SetAttributes[RKTreeOrderConditions, Listable];
RKTreeOrderConditions[rk_RK, t:(_BTree | _BTreeDiffAlg), opts:OptionsPattern[RKB]] := orderConditions[t, RKA[rk], RKB[rk, opts], RKC[rk], opts];
RKTreeOrderConditions[t:(_BTree | _BTreeDiffAlg), opts:OptionsPattern[RKB]] := orderConditions[t, \[FormalA], If[TrueQ[OptionValue[Embedded]], OverHat[\[FormalB]], \[FormalB]], \[FormalC], opts];

RKSimplifyingAssumptionB[rk_RK, {1}, opts:OptionsPattern[RKB]] := Total[RKB[rk, opts]] - denseOne[1, opts];
RKSimplifyingAssumptionB[rk_RK, {p_Integer?Positive}, opts:OptionsPattern[RKB]] := RKB[rk, opts] . RKC[rk]^(p - 1) - denseOne[p, opts] / p;
RKSimplifyingAssumptionB[rk_RK, p_Integer?Positive, opts:OptionsPattern[RKB]] := Table[RKSimplifyingAssumptionB[rk, {k}, opts], {k, p}];

RKSimplifyingAssumptionC[rk_RK, {1}, stages_:All] := Total[RKA[rk][[stages]], {2}] - RKC[rk][[stages]];
RKSimplifyingAssumptionC[rk_RK, {eta_Integer?Positive}, stages_:All] := RKA[rk][[stages]] . RKC[rk]^(eta - 1) - RKC[rk][[stages]]^eta / eta;
RKSimplifyingAssumptionC[rk_RK, eta_Integer?Positive, stages_:All] := Table[RKSimplifyingAssumptionC[rk, {k}, stages], {k, eta}];

RKSimplifyingAssumptionD[rk_RK, {1}, opts:OptionsPattern[RKB]] := RKB[rk, opts] . RKA[rk] - RKB[rk, opts] * (denseOne[1, opts] - RKC[rk]);
RKSimplifyingAssumptionD[rk_RK, {zeta_Integer?Positive}, opts:OptionsPattern[RKB]] := With[{
		b = RKB[rk, opts],
		c = RKC[rk]
	},
	(b * c^(zeta - 1)) . RKA[rk] - b * (denseOne[zeta, opts] - c^zeta) / zeta
];
RKSimplifyingAssumptionD[rk_RK, zeta_Integer, opts:OptionsPattern[RKB]] := Table[RKSimplifyingAssumptionD[rk, {k}, opts], {k, zeta}];

RKOrder[rk_RK, opts:OptionsPattern[RKB]] := CountZeros[RKOrderConditions[rk, {#}, opts] &] - 1;

RKExtrapolation[m_RK, steps_/;VectorQ[steps, Positive] && DuplicateFreeQ[steps], jump:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RKOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(jump * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];

RKErrorA[rk_RK, p_Integer?NonNegative, opts:OptionsPattern[RKB]] := Norm[RKOrderConditions[rk, {p}, opts]];
RKErrorA[rk_RK, opts:OptionsPattern[RKB]] := RKErrorA[rk, RKOrder[rk] + 1, opts];

RKErrorB[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat, Embedded -> True] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorB[rk_?RKPairQ] := RKErrorB[rk, RKOrder[rk] + 1];

RKErrorC[rk_?RKPairQ, pHat_Integer?Positive] := Norm[
		RKOrderConditions[rk, {pHat}] - RKOrderConditions[RKEmbedded[rk], {pHat}]
	] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorC[rk_?RKPairQ] := RKErrorC[rk, RKOrder[rk] + 1];

RKErrorD[HoldPattern[RK[args__]]] := Max[Abs[{args}]];

RKErrorE[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorE[rk_?RKPairQ] := RKErrorE[rk, RKOrder[rk] + 1];

RKDispersionOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDispersionError[rk, y, opts], y];

RKDispersionError[rk_RK, y_, opts:OptionsPattern[RKB]] := y - ComplexExpand[Arg[RKLinearStability[rk, y * I, opts]], TargetFunctions -> {Re, Im}];

RKDissipationOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDissipationError[rk, y, opts], y];

RKDissipationError[rk_RK, y_, opts:OptionsPattern[RKB]] := 1 - ComplexExpand[Abs[RKLinearStability[rk, y * I, opts]]];


End[];
EndPackage[];
