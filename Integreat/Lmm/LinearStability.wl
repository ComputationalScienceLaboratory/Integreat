(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`LinearStability`"];
Integreat`Lmm`LinearStability::usage = "Package containing functions for analyzing the linear stability of linear multistep methods";

LmmLinearStabilityPlot::usage = "Plots the region of linear stability";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Lmm`Methods`",
	"Integreat`Glm`Methods`",
	"Integreat`Glm`LinearStability`"
}];


(* ::Section:: *)
(*Package Definitions*)


LmmLinearStabilityPlot[lmm_Lmm, args___] := GlmLinearStabilityPlot[Glm[lmm], args];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
