(* ::Package:: *)

BeginPackage["Integreat`Rk`OrderConditions`"];


Integreat`Rk`OrderConditions::usage = "Package containing functions for determining the order of Runge-Kutta methods";

RungeKuttaOrderCondition::usage = "?";
RungeKuttaDaeOrderCondition::usage = "?";
RungeKuttaSimplifyingAssumptionB::usage = "The Runge-Kutta simplifying assumption B";
RungeKuttaSimplifyingAssumptionC::usage = "The Runge-Kutta simplifying assumption C";
RungeKuttaSimplifyingAssumptionD::usage = "The Runge-Kutta simplifying assumption D";
RungeKuttaOrder::usage = "?";
RungeKuttaExtrapolation::usage = "?";
RungeKuttaErrorA::usage = "The 2-norm of the principal error";
RungeKuttaErrorAHat::usage = "The 2-norm of the embedded principal error";
RungeKuttaErrorB::usage = "The ratio of the embedded second error terms' norm to leading error terms' norm";
RungeKuttaErrorC::usage = "The ratio of the norm of the difference in second error terms to the norm of embedded leading error terms";
RungeKuttaErrorD::usage = "The maximum entry in the Butcher tableau by absolute value";
RungeKuttaErrorE::usage = "The ratio of the second error terms' norm to embedded leading error terms' norm";
RungeKuttaDispersionError::usage = "Dispersion error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RungeKuttaDispersionOrder::usage = "Dispersion order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RungeKuttaDissipationError::usage = "Dissipation error of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";
RungeKuttaDissipationOrder::usage = "Dissipation order of Runge-Kutta method applied to y'=\[ImaginaryI]\[Omega]y";


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Rk`Methods`",
	"Integreat`Rk`LinearStability`",
	"Integreat`Internal`MathUtils`",
	"Integreat`BTrees`"
}];

orderCondition[r:_[t_], theta_, a_, b_, c_] := (phi[t, a, b, c] - theta^BTreeOrder[r] / BTreeGamma[r]) / BTreeSigma[r];

phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];

phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];

errOrder[err_?PossibleZeroQ, _] := Infinity;
errOrder[err_, y_] := NestWhile[# + 1 &, 0, PossibleZeroQ[SeriesCoefficient[err, {y, 0, #}]] &] - 1;


RungeKuttaOrderCondition[rk: Repeated[_RungeKutta, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}] := RungeKuttaTreeOrderCondition[rk, BTree[p]];

RungeKuttaDaeOrderCondition[rk: Repeated[_RungeKutta, {0, 1}], p:_Integer?Positive | {_Integer?NonNegative}] := RungeKuttaTreeOrderCondition[rk, BTreeDiffAlg[p]];

RungeKuttaTreeOrderCondition[rk_RungeKutta, t:(_BTree | _BTreeDiffAlg)] := orderCondition[t, 1, RungeKuttaA[rk], RungeKuttaB[rk], RungeKuttaC[rk]];
RungeKuttaTreeOrderCondition[t:(_BTree | _BTreeDiffAlg)] := orderCondition[t, 1, \[FormalA], \[FormalB], \[FormalC]];
SetAttributes[RungeKuttaTreeOrderCondition, Listable];

RungeKuttaSimplifyingAssumptionB[rk_RungeKutta, {1}] := Total[RungeKuttaB[rk]] - 1;
RungeKuttaSimplifyingAssumptionB[rk_RungeKutta, {p_Integer?Positive}] := RungeKuttaB[rk].RungeKuttaC[rk]^(p - 1) - 1 / p;
RungeKuttaSimplifyingAssumptionB[rk_RungeKutta, p_Integer?Positive] := Table[RungeKuttaSimplifyingAssumptionB[rk, {k}], {k, p}];

RungeKuttaSimplifyingAssumptionC[rk_RungeKutta, {1}, stages_:All] := Total[RungeKuttaA[rk][[stages]], {2}] - RungeKuttaC[rk][[stages]];
RungeKuttaSimplifyingAssumptionC[rk_RungeKutta, {eta_Integer?Positive}, stages_:All] := RungeKuttaA[rk][[stages]].RungeKuttaC[rk]^(eta - 1) - RungeKuttaC[rk][[stages]]^eta / eta;
RungeKuttaSimplifyingAssumptionC[rk_RungeKutta, eta_Integer?Positive, stages_:All] := Table[RungeKuttaSimplifyingAssumptionC[rk, {k}, stages], {k, eta}];

RungeKuttaSimplifyingAssumptionD[rk_RungeKutta, {1}] := RungeKuttaB[rk].RungeKuttaA[rk] - RungeKuttaB[rk] * (1 - RungeKuttaC[rk]);
RungeKuttaSimplifyingAssumptionD[rk_RungeKutta, {zeta_Integer?Positive}] := With[{
		b = RungeKuttaB[rk],
		c = RungeKuttaC[rk]
	},
	(b * c^(zeta - 1)).RungeKuttaA[rk] - b * (1 - c^zeta) / zeta
];
RungeKuttaSimplifyingAssumptionD[rk_RungeKutta, zeta_Integer] := Table[RungeKuttaSimplifyingAssumptionD[rk, {k}], {k, zeta}];

RungeKuttaOrder[rk_RungeKutta] := NestWhile[# + 1 &, 0, And @@ PossibleZeroQ[RungeKuttaOrderCondition[rk, {# + 1}]] &];

RungeKuttaExtrapolation[m_RungeKutta, steps_/;VectorQ[steps, Positive] && DuplicateFreeQ[steps], jump:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RungeKuttaOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(jump * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];

RungeKuttaErrorA[rk_RungeKutta, p_Integer?NonNegative] := Norm[RungeKuttaOrderCondition[rk, {p}]];
RungeKuttaErrorA[rk_RungeKutta] := RungeKuttaErrorA[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorAHat[rk_?RungeKuttaPairQ, pHat: Repeated[_Integer?NonNegative, {0, 1}]] := RungeKuttaErrorA[RungeKuttaEmbedded[rk], pHat];

RungeKuttaErrorB[rk_?RungeKuttaPairQ, pHat_Integer?Positive] := RungeKuttaErrorAHat[rk, pHat] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorB[rk_?RungeKuttaPairQ] := RungeKuttaErrorB[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorC[rk_?RungeKuttaPairQ, pHat_Integer?Positive] := Norm[
		RungeKuttaOrderCondition[rk, {pHat}] - RungeKuttaOrderCondition[RungeKuttaEmbedded[rk], {pHat}]
	] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorC[rk_?RungeKuttaPairQ] := RungeKuttaErrorC[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorD[HoldPattern[RungeKutta[args__]]] := Max[Abs[{args}]];

RungeKuttaErrorE[rk_?RungeKuttaPairQ, pHat_Integer?Positive] := RungeKuttaErrorA[rk, pHat] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorE[rk_?RungeKuttaPairQ] := RungeKuttaErrorE[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaDispersionError[rk_RungeKutta, y_] := y - ComplexExpand[Arg[RungeKuttaLinearStability[rk, y * I]], TargetFunctions -> {Re, Im}];

RungeKuttaDispersionOrder[rk_RungeKutta] := errOrder[RungeKuttaDispersionError[rk, y], y];

RungeKuttaDissipationError[rk_RungeKutta, y_] := 1 - ComplexExpand[Abs[RungeKuttaLinearStability[rk, y * I]]];

RungeKuttaDissipationOrder[rk_RungeKutta] := errOrder[RungeKuttaDissipationError[rk, y], y];


End[];
EndPackage[];
