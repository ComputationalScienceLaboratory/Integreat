(* ::Package:: *)

BeginPackage["Integreat`Internal`LinearStability`"];


Integreat`Internal`LinearStability::usage = "An internal package for analyzing linear stability";

LinearStabilityPlot::usage = "Plots a stability region on the complex plane";
OrderStarPlot::usage = "Plots an order star on the complex plane";


Begin["`Private`"];


LinearStabilityPlot[stab_, re_:{-6, 2}, im_:{-4, 4}, opts:OptionsPattern[RegionPlot]] := RegionPlot[
	stab[a + b * I] <= 1,
	Prepend[re, a],
	Prepend[im, b],
	opts,
	FrameLabel -> {"Re", "Im"}
];

OrderStarPlot[stab_, re_:{-4, 4}, im_:{-4, 4}, opts:OptionsPattern[RegionPlot]] := RegionPlot[
	stab[a + b * I] > Exp[a],
	Prepend[re, a],
	Prepend[im, b],
	opts,
	FrameLabel -> {"Re", "Im"}
];


End[];
EndPackage[];
