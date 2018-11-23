(* ::Package:: *)

BeginPackage["CSL`OdeUtils`MrGARK`NonlinearStability`"];


CSL`OdeUtils`MrGARK`NonlinearStability::usage = "Package containing functions for analyzing the linear stability Runge-Kutta methods";

MrGarkAlgebraicStabilityMatrix::usage = "";


Begin["`Private`"];
Needs["CSL`OdeUtils`MrGARK`Methods`"];

P[A1_, b1_, A2_, b2_] := DiagonalMatrix[b1] . A1 + Transpose[A2] . DiagonalMatrix[b2] - Outer[Times, b1, b2];


MrGarkAlgebraicStabilityMatrix[method_?MrGarkQ, M_Integer] := With[{
	Aff = MrGarkFullAff[method, M],
	Afs = MrGarkFullAfs[method, M],
	Asf = MrGarkFullAsf[method, M],
	Ass = MrGarkFullAss[method],
	bf = MrGarkFullBf[method, M],
	bs = MrGarkFullBs[method]
},
	ArrayFlatten[{{P[Aff, bf, Aff, bf], P[Afs, bf, Asf, bs]}, {P[Asf, bs, Afs, bf], P[Ass, bs, Ass, bs]}}]
]


End[];


EndPackage[];
