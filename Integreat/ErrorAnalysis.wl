(* ::Package:: *)

BeginPackage["Integreat`ErrorAnalysis`"];


Integreat`ErrorAnalysis::usage = "Functions analyzing the error of time integration methods";

FitErrors::usage = "Fits a function to errors versus stepsizes";
ApproximateOrder::usage = "Approximates the order exponent given step sizes and error";


Begin["`Private`"];


FitErrors[h_?VectorQ, errors_?VectorQ] := NonlinearModelFit[Transpose[{h, errors}], \[FormalC] \[FormalH]^\[FormalE], {\[FormalC], \[FormalE]}, \[FormalH]];

ApproximateOrder[h_?VectorQ, err_?VectorQ] := \[FormalE] /. FitErrors[h, err]["BestFitParameters"]


End[];


EndPackage[];
