(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`Error`"];


CSL`OdeUtils`RungeKutta`Error::usage = "Package containing functions for analyzing the error Runge-Kutta methods";

RungeKuttaA::usage = "The 2-norm of the principal error";
RungeKuttaB::usage = "The ratio of the embedded second error terms' norm to leading error terms' norm";
RungeKuttaC::usage = "The ratio of the norm of the difference in second error terms to the norm of embedded leading error terms";
RungeKuttaD::usage = "The maximum entry in the Butcher tableau by absolute value";
RungeKuttaE::usage = "The ratio of the second error terms' norm to leading error terms' norm";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["NumericalDifferentialEquationAnalysis`"];

RungeKuttaReplace[expr_, method_] := With[{
		Asubs = MapIndexed[Subscript[\[FormalA], First[#2], Last[#2]]->#1 &, method[\[FormalCapitalA]], {2}],
		bsubs = MapIndexed[Subscript[\[FormalB], First[#2]]->#1 &, method[\[FormalB]]],
		csubs = MapIndexed[Subscript[\[FormalC], First[#2]]->#1 &, method[\[FormalC]]]
	},
	ReplaceAll[expr, Flatten[{Asubs, bsubs, csubs}]]
];


RungeKuttaA[method_/;RungeKuttaQ[method], p_Integer] := Norm[RungeKuttaReplace[ButcherPrincipalError[p - 1, RungeKuttaStages[method]], method]];

RungeKuttaB[method_/;RungeKuttaPairQ[method], pHat_Integer] := With[{
	eMethod = RungeKuttaEmbedded[method]
},
	RungeKuttaA[eMethod, pHat] / RungeKuttaA[eMethod, pHat - 1]
];

RungeKuttaC[method_/;RungeKuttaPairQ[method], pHat_Integer] := With[{
	s = RungeKuttaStages[method],
	eMethod = RungeKuttaEmbedded[method]
},
	Norm[
		RungeKuttaReplace[ButcherPrincipalError[pHat - 1, s], method] - RungeKuttaReplace[ButcherPrincipalError[pHat - 1, s], eMethod]
	] / RungeKuttaA[eMethod, pHat - 1]
];

RungeKuttaD[method_/;RungeKuttaQ[method]] := Max[Abs[{
	method[\[FormalCapitalA]], method[\[FormalB]], method[\[FormalC]], If[KeyExistsQ[method, \[FormalD]], method[\[FormalD]], {}]
}]];

RungeKuttaE[method_/;RungeKuttaPairQ[method], pHat_Integer] := RungeKuttaA[method, pHat] / RungeKuttaA[RungeKuttaEmbedded[method], pHat - 1];


End[];


EndPackage[];
