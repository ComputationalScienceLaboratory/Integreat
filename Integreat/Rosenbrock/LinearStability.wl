(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rosenbrock`LinearStability`"];
Integreat`Rosenbrock`LinearStability::usage = "Package containing functions for analyzing the linear stability of Rosenbrock methods";

RosenbrockLinearStability::usage = "The linear stability function for a Rosenbrock method applied to y'=\[Lambda]y";
RosenbrockOrderStarPlot::usage = "Plots the order star";
RosenbrockLinearStabilityPlot::usage = "Plots the region of linear stability";
RosenbrockLinearStabilityP::usage = "The numerator of the linear stability function";
RosenbrockLinearStabilityQ::usage = "The denominator of the linear stability function";
RosenbrockEPolynomial::usage = "The E-polynomial to test for I-stability";
RosenbrockAStableCondition::usage = "Returns True if the Rosenbrock method is A-stable, and False otherwise";
RosenbrockStifflyAccurateCondition::usage = "Determines if a Rosenbrock method is stiffly-accurate";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Rosenbrock`Methods`", "Integreat`Internal`LinearStability`"}];


(* ::Section:: *)
(*Package Definitions*)


RosenbrockLinearStability[ros_Rosenbrock, lim_DirectedInfinity, p:Repeated[_, {0, 1}]] := Limit[RosenbrockLinearStability[ros, z, p], z -> lim];
RosenbrockLinearStability[ros_Rosenbrock, z_, p_] := Total[Inverse[IdentityMatrix[Length[ros]] - z * RosenbrockBeta[ros]], {2}][[p]];
RosenbrockLinearStability[ros_Rosenbrock, z_] := 1 + z * RosenbrockB[ros].RosenbrockLinearStability[ros, z, All];

RosenbrockOrderStarPlot[ros_Rosenbrock, args___] := OrderStarPlot[Evaluate[Abs[RosenbrockLinearStability[ros, #]]] &, args];

RosenbrockLinearStabilityPlot[ros_Rosenbrock, args___] := LinearStabilityPlot[Evaluate[Abs[RosenbrockLinearStability[ros, #]]] &, args];

RosenbrockLinearStabilityP[ros_Rosenbrock, z_, p_] := With[{
		B = RosenbrockBeta[ros],
		s = Length[ros]
	},
	(*Computes P for all stages.  More efficient implementation?*)
	Map[Det[IdentityMatrix[s] + z * (ConstantArray[#, s] - B)] &, B][[p]]
];

RosenbrockLinearStabilityP[ros_Rosenbrock, z_] := With[{
		s = Length[ros]
	},
	Det[IdentityMatrix[s] + z * (ConstantArray[RosenbrockB[ros], s] - RosenbrockBeta[ros])]
];

RosenbrockLinearStabilityQ[ros_Rosenbrock, z_] := Det[IdentityMatrix[Length[ros]] - z * RosenbrockBeta[ros]];

RosenbrockEPolynomial[ros_Rosenbrock, y_] := ComplexExpand[
	RosenbrockLinearStabilityQ[ros, y * I] * RosenbrockLinearStabilityQ[ros, -y * I]
	- RosenbrockLinearStabilityP[ros, y * I] * RosenbrockLinearStabilityP[ros, -y * I]
];

RosenbrockAStableCondition[ros_Rosenbrock, p:Repeated[_, {0, 1}]] := Resolve[ForAll[y, RosenbrockEPolynomial[ros, y, p] >= 0], Reals]

RosenbrockStifflyAccurateCondition[ros_Rosenbrock] := And @@ Thread[Last[RosenbrockBeta[ros]] == RosenbrockB[ros]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
