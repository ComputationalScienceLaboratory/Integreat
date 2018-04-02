(* ::Package:: *)

BeginPackage["CSL`OdeUtils`MrGARK`LinearStability`"];


CSL`OdeUtils`MrGARK`LinearStability::usage = "Package containing functions for analyzing the linear stability Runge-Kutta methods";

MrGarkLinearStability::usage = "The linear stability function for a Runge-Kutta method";


Begin["`Private`"];
Needs["CSL`OdeUtils`MrGARK`Methods`"];


MrGarkLinearStability[method_/;MrGarkQ[method], M_Integer, zf_, zs_] := With[{
	Z = ArrayFlatten[{{zf * IdentityMatrix[M * MrGarkFastStages[method]], 0}, {0, zs * IdentityMatrix[MrGarkSlowStages[method]]}}],
	s = M * MrGarkFastStages[method] + MrGarkSlowStages[method]
},
	1 + MrGarkFullB[method, M].Z.LinearSolve[IdentityMatrix[s] - MrGarkFullA[method, M].Z, ConstantArray[1, s]]
];


End[];


EndPackage[];
