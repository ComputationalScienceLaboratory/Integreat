(* ::Package:: *)

BeginPackage["Integreat`Rk`OrderConditions`"];


Integreat`Rk`OrderConditions::usage = "Package containing functions for determining the order of Runge-Kutta methods";

RkOrderConditions::usage = "?";
RkDaeOrderConditions::usage = "?";
RkTreeOrderConditions::usage = "";
RkSimplifyingAssumptionB::usage = "The Runge-Kutta simplifying assumption B";
RkSimplifyingAssumptionC::usage = "The Runge-Kutta simplifying assumption C";
RkSimplifyingAssumptionD::usage = "The Runge-Kutta simplifying assumption D";
RkOrder::usage = "?";
RkExtrapolation::usage = "?";
RkErrorA::usage = "The 2-norm of the principal error";
RkErrorB::usage = "The ratio of the embedded second error terms' norm to leading error terms' norm";
RkErrorC::usage = "The ratio of the norm of the difference in second error terms to the norm of embedded leading error terms";
RkErrorD::usage = "The maximum entry in the Butcher tableau by absolute value";
RkErrorE::usage = "The ratio of the second error terms' norm to embedded leading error terms' norm";
RkDispersionError::usage = "Dispersion error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RkDispersionOrder::usage = "Dispersion order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RkDissipationError::usage = "Dissipation error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RkDissipationOrder::usage = "Dissipation order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Rk`Methods`",
	"Integreat`Rk`LinearStability`",
	"Integreat`Internal`MathUtils`",
	"Integreat`BTrees`"
}];

SetAttributes[denseOne, HoldFirst];
denseOne[p_, OptionsPattern[RkB]] := If[TrueQ[OptionValue[DenseOutput]], \[FormalTheta]^p, 1]

orderConditions[r:_[t_], a_, b_, c_, opts:OptionsPattern[RkB]] := (phi[t, a, b, c] - denseOne[BTreeOrder[r], opts] / BTreeGamma[r]) / BTreeSigma[r];

phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];

phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];

errOrder[err_?PossibleZeroQ, _] := Infinity;
errOrder[err_, y_] := CountZeros[SeriesCoefficient[err, {y, 0, #}] &, 1] - 1;


RkOrderConditions[rk: Repeated[_Rk, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RkB]] := RkTreeOrderConditions[rk, BTree[p], opts];

RkDaeOrderConditions[rk: Repeated[_Rk, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RkB]] := RkTreeOrderConditions[rk, BTreeDiffAlg[p], opts];

SetAttributes[RkTreeOrderConditions, Listable];
RkTreeOrderConditions[rk_Rk, t:(_BTree | _BTreeDiffAlg), opts:OptionsPattern[RkB]] := orderConditions[t, RkA[rk], RkB[rk, opts], RkC[rk], opts];
RkTreeOrderConditions[t:(_BTree | _BTreeDiffAlg), opts:OptionsPattern[RkB]] := orderConditions[t, \[FormalA], If[TrueQ[OptionValue[Embedded]], OverHat[\[FormalB]], \[FormalB]], \[FormalC], opts];

RkSimplifyingAssumptionB[rk_Rk, {1}, opts:OptionsPattern[RkB]] := Total[RkB[rk, opts]] - denseOne[1, opts];
RkSimplifyingAssumptionB[rk_Rk, {p_Integer?Positive}, opts:OptionsPattern[RkB]] := RkB[rk, opts].RkC[rk]^(p - 1) - denseOne[p, opts] / p;
RkSimplifyingAssumptionB[rk_Rk, p_Integer?Positive, opts:OptionsPattern[RkB]] := Table[RkSimplifyingAssumptionB[rk, {k}, opts], {k, p}];

RkSimplifyingAssumptionC[rk_Rk, {1}, stages_:All] := Total[RkA[rk][[stages]], {2}] - RkC[rk][[stages]];
RkSimplifyingAssumptionC[rk_Rk, {eta_Integer?Positive}, stages_:All] := RkA[rk][[stages]].RkC[rk]^(eta - 1) - RkC[rk][[stages]]^eta / eta;
RkSimplifyingAssumptionC[rk_Rk, eta_Integer?Positive, stages_:All] := Table[RkSimplifyingAssumptionC[rk, {k}, stages], {k, eta}];

RkSimplifyingAssumptionD[rk_Rk, {1}, opts:OptionsPattern[RkB]] := RkB[rk, opts].RkA[rk] - RkB[rk, opts] * (denseOne[1, opts] - RkC[rk]);
RkSimplifyingAssumptionD[rk_Rk, {zeta_Integer?Positive}, opts:OptionsPattern[RkB]] := With[{
		b = RkB[rk, opts],
		c = RkC[rk]
	},
	(b * c^(zeta - 1)).RkA[rk] - b * (denseOne[zeta, opts] - c^zeta) / zeta
];
RkSimplifyingAssumptionD[rk_Rk, zeta_Integer, opts:OptionsPattern[RkB]] := Table[RkSimplifyingAssumptionD[rk, {k}, opts], {k, zeta}];

RkOrder[rk_Rk, opts:OptionsPattern[RkB]] := CountZeros[RkOrderConditions[rk, {#}, opts] &, 1] - 1;

RkExtrapolation[m_Rk, steps_/;VectorQ[steps, Positive] && DuplicateFreeQ[steps], jump:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RkOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(jump * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];

RkErrorA[rk_Rk, p_Integer?NonNegative, opts:OptionsPattern[RkB]] := Norm[RkOrderConditions[rk, {p}, opts]];
RkErrorA[rk_Rk, opts:OptionsPattern[RkB]] := RkErrorA[rk, RkOrder[rk] + 1, opts];

RkErrorB[rk_?RkPairQ, pHat_Integer?Positive] := RkErrorA[rk, pHat, Embedded -> True] / RkErrorA[rk, pHat - 1, Embedded -> True];
RkErrorB[rk_?RkPairQ] := RkErrorB[rk, RkOrder[rk] + 1];

RkErrorC[rk_?RkPairQ, pHat_Integer?Positive] := Norm[
		RkOrderConditions[rk, {pHat}] - RkOrderConditions[RkEmbedded[rk], {pHat}]
	] / RkErrorA[rk, pHat - 1, Embedded -> True];
RkErrorC[rk_?RkPairQ] := RkErrorC[rk, RkOrder[rk] + 1];

RkErrorD[HoldPattern[Rk[args__]]] := Max[Abs[{args}]];

RkErrorE[rk_?RkPairQ, pHat_Integer?Positive] := RkErrorA[rk, pHat] / RkErrorA[rk, pHat - 1, Embedded -> True];
RkErrorE[rk_?RkPairQ] := RkErrorE[rk, RkOrder[rk] + 1];

RkDispersionOrder[rk_Rk, opts:OptionsPattern[RkB]] := errOrder[RkDispersionError[rk, y, opts], y];

RkDispersionError[rk_Rk, y_, opts:OptionsPattern[RkB]] := y - ComplexExpand[Arg[RkLinearStability[rk, y * I, opts]], TargetFunctions -> {Re, Im}];

RkDissipationOrder[rk_Rk, opts:OptionsPattern[RkB]] := errOrder[RkDissipationError[rk, y, opts], y];

RkDissipationError[rk_Rk, y_, opts:OptionsPattern[RkB]] := 1 - ComplexExpand[Abs[RkLinearStability[rk, y * I, opts]]];


End[];
EndPackage[];
