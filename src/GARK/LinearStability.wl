(* ::Package:: *)

BeginPackage["CSL`OdeUtils`MrGARK`LinearStability`"];


CSL`OdeUtils`MrGARK`LinearStability::usage = "Package containing functions for analyzing the linear stability Runge-Kutta methods";

MrGarkLinearStability::usage = "The scalar linear stability function for a Runge-Kutta method";
MrGarkLinearStabilityP::usage = "The numerator of the scalar linear stability function for a Runge-Kutta method";
MrGarkLinearStabilityQ::usage = "The denominator of the scalar linear stability function for a Runge-Kutta method";
MrGarkLinearStabilityE::usage = "The denominator of the scalar linear stability function for a Runge-Kutta method";
MrGark2DLinearStability::usage = "The 2x2 linear stability function for a Runge-Kutta method";


Begin["`Private`"];
Needs["CSL`OdeUtils`MrGARK`Methods`"];


(*MrGarkLinearStability[method_?MrGarkQ, M_Integer, zf_, zs_] := With[{
	Z = ArrayFlatten[{{zf * IdentityMatrix[M * MrGarkFastStages[method]], 0}, {0, zs * IdentityMatrix[MrGarkSlowStages[method]]}}],
	s = M * MrGarkFastStages[method] + MrGarkSlowStages[method]
},
	1 + MrGarkFullB[method, M].Z.LinearSolve[IdentityMatrix[s] - MrGarkFullA[method, M].Z, ConstantArray[1, s]]
];*)

MrGarkLinearStability[method_?MrGarkQ, M_Integer, zf_, zs_] := With[{
	Z = ArrayFlatten[{{zf * IdentityMatrix[M * MrGarkFastStages[method]], 0}, {0, zs * IdentityMatrix[MrGarkSlowStages[method]]}}],
	s = M * MrGarkFastStages[method] + MrGarkSlowStages[method]
},
	1 + MrGarkFullB[method, M].Z.Inverse[IdentityMatrix[s] - MrGarkFullA[method, M].Z].ConstantArray[1, s]
];

MrGarkLinearStabilityP[method_?MrGarkQ, M_Integer, zf_, zs_] := With[{
	Z = ArrayFlatten[{{zf * IdentityMatrix[M * MrGarkFastStages[method]], 0}, {0, zs * IdentityMatrix[MrGarkSlowStages[method]]}}],
	s = M * MrGarkFastStages[method] + MrGarkSlowStages[method]
},
	Det[IdentityMatrix[s] + (ConstantArray[MrGarkFullB[method, M], s] - MrGarkFullA[method, M]).Z]
];

MrGarkLinearStabilityQ[method_?MrGarkQ, M_Integer, zf_, zs_] := With[{
	Z = ArrayFlatten[{{zf * IdentityMatrix[M * MrGarkFastStages[method]], 0}, {0, zs * IdentityMatrix[MrGarkSlowStages[method]]}}],
	s = M * MrGarkFastStages[method] + MrGarkSlowStages[method]
},
	Det[IdentityMatrix[s] - MrGarkFullA[method, M].Z]
];

MrGarkLinearStabilityE[method_?MrGarkQ, M_Integer, zf_, zs_] := Abs[MrGarkLinearStabilityQ[method, M, zf, zs]]^2 - Abs[MrGarkLinearStabilityP[method, M, zf, zs]]^2;

MrGark2DLinearStability[method_?MrGarkQ, M_Integer, A_/;Dimensions[A]==={2,2}] := With[{
	sf = M * MrGarkFastStages[method],
	ss = MrGarkSlowStages[method]
},
	IdentityMatrix[2] + ArrayFlatten[{
		{{MrGarkFullBf[method, M]}, 0},
		{0, {MrGarkFullBs[method]}}
	}] . Inverse[ArrayFlatten[{
		{IdentityMatrix[sf] - A[[1, 1]] * MrGarkFullAff[method, M], -A[[1, 2]] * MrGarkFullAfs[method, M]},
		{-A[[2, 1]] * MrGarkFullAsf[method, M], IdentityMatrix[ss] - A[[2, 2]] * MrGarkFullAss[method]}
	}]] . ArrayFlatten[{
		{ConstantArray[1, {sf, 1}], 0},
		{0, ConstantArray[1, {ss, 1}]}
	}] . A
];


End[];


EndPackage[];
