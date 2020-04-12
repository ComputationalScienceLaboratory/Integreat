(* ::Package:: *)

BeginPackage["Integreat`RungeKutta`OrderConditions`", {"Integreat`RungeKutta`Methods`", "Integreat`RungeKutta`LinearStability`", "NumericalDifferentialEquationAnalysis`"}];


Integreat`RungeKutta`OrderConditions::usage = "Package containing functions for determining the order of Runge-Kutta methods";

RungeKuttaOrderCondition::usage = "?";
RungeKuttaSimplifyingAssumptionB::usage = "The Runge-Kutta simplifying assumption B";
RungeKuttaSimplifyingAssumptionC::usage = "The Runge-Kutta simplifying assumption C";
RungeKuttaSimplifyingAssumptionD::usage = "The Runge-Kutta simplifying assumption D";
RungeKuttaPrincipalError::usage = "?";
RungeKuttaOrder::usage = "?";
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

RungeKuttaReplace[expr_, rk_] := With[{
		Asubs = MapIndexed[Subscript[\[FormalA], First[#2], Last[#2]]->#1 &, RungeKuttaA[rk], {2}],
		bsubs = MapIndexed[Subscript[\[FormalB], First[#2]]->#1 &, RungeKuttaB[rk]],
		csubs = MapIndexed[Subscript[\[FormalC], First[#2]]->#1 &, RungeKuttaC[rk]]
	},
	ReplaceAll[expr, Flatten[{Asubs, bsubs, csubs}]]
];

ErrOrder[err_, y_] := If[PossibleZeroQ[err], \[Infinity], (
	For[p = 0, PossibleZeroQ[SeriesCoefficient[err, {y, 0, p}]], p++];
	p - 1
)];


RungeKuttaOrderCondition[rk_RungeKutta, p_Integer] := RungeKuttaReplace[RungeKuttaOrderConditions[p, RungeKuttaStages[rk]], rk];

RungeKuttaSimplifyingAssumptionB[rk_RungeKutta, p_Integer] := With[{
		b = RungeKuttaB[rk],
		c = RungeKuttaC[rk]
	},
	Table[If[k == 1, Total[b], b.c^(k-1)] == 1 / k, {k, p}]
];

RungeKuttaSimplifyingAssumptionC[rk_RungeKutta, eta_Integer] := With[{
		A = RungeKuttaA[rk],
		c = RungeKuttaC[rk]
	},
	Table[Thread[If[k == 1, Total[A, {2}], A.c^(k-1)] == c^k / k], {k, eta}]
];

RungeKuttaSimplifyingAssumptionD[rk_RungeKutta, zeta_Integer] := With[{
		A = RungeKuttaA[rk],
		b = RungeKuttaB[rk],
		c = RungeKuttaC[rk]
	},
	Table[Thread[If[k == 1, b, b * c^(k-1)].A == b * (1 - c^k) / k], {k, zeta}]
];

RungeKuttaPrincipalError[rk_RungeKutta, p_Integer] := RungeKuttaReplace[ButcherPrincipalError[p, RungeKuttaStages[rk]], rk];
RungeKuttaPrincipalError[rk_RungeKutta] := RungeKuttaPrincipalError[rk, RungeKuttaOrder[rk]];

RungeKuttaOrder[rk_RungeKutta] := (
	For[p = 1, PossibleZeroQ[FullSimplify[Norm[RungeKuttaPrincipalError[rk, p]]]], p++];
	p
);

RungeKuttaErrorA[rk_RungeKutta, p_Integer] := Norm[FullSimplify[RungeKuttaPrincipalError[rk, p - 1]]];
RungeKuttaErrorA[rk_RungeKutta] := RungeKuttaErrorA[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorAHat[rk_?RungeKuttaPairQ, pHat_Integer] := RungeKuttaErrorA[RungeKuttaEmbedded[rk], pHat];
RungeKuttaErrorAHat[rk_?RungeKuttaPairQ] := RungeKuttaErrorA[RungeKuttaEmbedded[rk]];

RungeKuttaErrorB[rk_?RungeKuttaPairQ, pHat_Integer] := RungeKuttaErrorAHat[rk, pHat] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorB[rk_?RungeKuttaPairQ] := RungeKuttaErrorB[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorC[rk_?RungeKuttaPairQ, pHat_Integer] := Norm[
		RungeKuttaPrincipalError[rk, pHat - 1] - RungeKuttaPrincipalError[RungeKuttaEmbedded[rk], pHat - 1]
	] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorC[rk_?RungeKuttaPairQ] := RungeKuttaErrorC[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaErrorD[rk_RungeKutta] := Max[Abs[Level[rk, 1]]];

RungeKuttaErrorE[rk_?RungeKuttaPairQ, pHat_Integer] := RungeKuttaErrorA[rk, pHat] / RungeKuttaErrorAHat[rk, pHat - 1];
RungeKuttaErrorE[rk_?RungeKuttaPairQ] := RungeKuttaErrorE[rk, RungeKuttaOrder[rk] + 1];

RungeKuttaDispersionError[rk_RungeKutta, y_] := y - ComplexExpand[Arg[RungeKuttaLinearStability[rk, y * I]], TargetFunctions -> {Re, Im}];

RungeKuttaDispersionOrder[rk_RungeKutta] := ErrOrder[RungeKuttaDispersionError[rk, y], y];

RungeKuttaDissipationError[rk_RungeKutta, y_] := 1 - ComplexExpand[Abs[RungeKuttaLinearStability[rk, y * I]]];

RungeKuttaDissipationOrder[rk_RungeKutta] := ErrOrder[RungeKuttaDissipationError[rk, y], y];


End[];


EndPackage[];
