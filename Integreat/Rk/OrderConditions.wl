(* ::Package:: *)

BeginPackage["Integreat`Rk`OrderConditions`"];


Integreat`Rk`OrderConditions::usage = "Package containing functions for determining the order of Runge-Kutta methods";

RkOrderConditions::usage = "?";
RkDaeOrderConditions::usage = "?";
RkSimplifyingAssumptionB::usage = "The Runge-Kutta simplifying assumption B";
RkSimplifyingAssumptionC::usage = "The Runge-Kutta simplifying assumption C";
RkSimplifyingAssumptionD::usage = "The Runge-Kutta simplifying assumption D";
RkOrder::usage = "?";
RkExtrapolation::usage = "?";
RkErrorA::usage = "The 2-norm of the principal error";
RkErrorAHat::usage = "The 2-norm of the embedded principal error";
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

orderConditions[r:_[t_], theta_, a_, b_, c_] := (phi[t, a, b, c] - theta^BTreeOrder[r] / BTreeGamma[r]) / BTreeSigma[r];

phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];

phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];

errOrder[err_?PossibleZeroQ, _] := Infinity;
errOrder[err_, y_] := NestWhile[# + 1 &, 0, PossibleZeroQ[SeriesCoefficient[err, {y, 0, #}]] &] - 1;


RkOrderConditions[rk: Repeated[_Rk, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}] := RkTreeOrderConditions[rk, BTree[p]];

RkDaeOrderConditions[rk: Repeated[_Rk, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}] := RkTreeOrderConditions[rk, BTreeDiffAlg[p]];

RkTreeOrderConditions[rk_Rk, t:(_BTree | _BTreeDiffAlg)] := orderConditions[t, 1, RkA[rk], RkB[rk], RkC[rk]];
RkTreeOrderConditions[t:(_BTree | _BTreeDiffAlg)] := orderConditions[t, 1, \[FormalA], \[FormalB], \[FormalC]];
SetAttributes[RkTreeOrderConditions, Listable];

RkSimplifyingAssumptionB[rk_Rk, {1}] := Total[RkB[rk]] - 1;
RkSimplifyingAssumptionB[rk_Rk, {p_Integer?Positive}] := RkB[rk].RkC[rk]^(p - 1) - 1 / p;
RkSimplifyingAssumptionB[rk_Rk, p_Integer?Positive] := Table[RkSimplifyingAssumptionB[rk, {k}], {k, p}];

RkSimplifyingAssumptionC[rk_Rk, {1}, stages_:All] := Total[RkA[rk][[stages]], {2}] - RkC[rk][[stages]];
RkSimplifyingAssumptionC[rk_Rk, {eta_Integer?Positive}, stages_:All] := RkA[rk][[stages]].RkC[rk]^(eta - 1) - RkC[rk][[stages]]^eta / eta;
RkSimplifyingAssumptionC[rk_Rk, eta_Integer?Positive, stages_:All] := Table[RkSimplifyingAssumptionC[rk, {k}, stages], {k, eta}];

RkSimplifyingAssumptionD[rk_Rk, {1}] := RkB[rk].RkA[rk] - RkB[rk] * (1 - RkC[rk]);
RkSimplifyingAssumptionD[rk_Rk, {zeta_Integer?Positive}] := With[{
		b = RkB[rk],
		c = RkC[rk]
	},
	(b * c^(zeta - 1)).RkA[rk] - b * (1 - c^zeta) / zeta
];
RkSimplifyingAssumptionD[rk_Rk, zeta_Integer] := Table[RkSimplifyingAssumptionD[rk, {k}], {k, zeta}];

RkOrder[rk_Rk] := NestWhile[# + 1 &, 0, VectorQ[RkOrderConditions[rk, {# + 1}], PossibleZeroQ] &];

RkExtrapolation[m_Rk, steps_/;VectorQ[steps, Positive] && DuplicateFreeQ[steps], jump:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RkOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(jump * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];

RkErrorA[rk_Rk, p_Integer?NonNegative] := Norm[RkOrderConditions[rk, {p}]];
RkErrorA[rk_Rk] := RkErrorA[rk, RkOrder[rk] + 1];

RkErrorAHat[rk_?RkPairQ, pHat: Repeated[_Integer?NonNegative, {0, 1}]] := RkErrorA[RkEmbedded[rk], pHat];

RkErrorB[rk_?RkPairQ, pHat_Integer?Positive] := RkErrorAHat[rk, pHat] / RkErrorAHat[rk, pHat - 1];
RkErrorB[rk_?RkPairQ] := RkErrorB[rk, RkOrder[rk] + 1];

RkErrorC[rk_?RkPairQ, pHat_Integer?Positive] := Norm[
		RkOrderConditions[rk, {pHat}] - RkOrderConditions[RkEmbedded[rk], {pHat}]
	] / RkErrorAHat[rk, pHat - 1];
RkErrorC[rk_?RkPairQ] := RkErrorC[rk, RkOrder[rk] + 1];

RkErrorD[HoldPattern[Rk[args__]]] := Max[Abs[{args}]];

RkErrorE[rk_?RkPairQ, pHat_Integer?Positive] := RkErrorA[rk, pHat] / RkErrorAHat[rk, pHat - 1];
RkErrorE[rk_?RkPairQ] := RkErrorE[rk, RkOrder[rk] + 1];

RkDispersionError[rk_Rk, y_] := y - ComplexExpand[Arg[RkLinearStability[rk, y * I]], TargetFunctions -> {Re, Im}];

RkDispersionOrder[rk_Rk] := errOrder[RkDispersionError[rk, y], y];

RkDissipationError[rk_Rk, y_] := 1 - ComplexExpand[Abs[RkLinearStability[rk, y * I]]];

RkDissipationOrder[rk_Rk] := errOrder[RkDissipationError[rk, y], y];


End[];
EndPackage[];
