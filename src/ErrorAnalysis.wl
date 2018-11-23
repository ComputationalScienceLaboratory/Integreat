(* ::Package:: *)

BeginPackage["CSL`OdeUtils`ErrorAnalysis`"];


CSL`OdeUtils`ErrorAnalysis::usage = "Package containing functions analyzing the error of time integration methods";

FitErrors::usage = "Fits a function to error versus step size";
ApproximateOrder::usage = "Approximates the order exponent given step sizes and error";


Begin["`Private`"];


FitErrors[h_?VectorQ, err_?VectorQ] := NonlinearModelFit[Transpose[{h, err}], \[FormalC] \[FormalH]^\[FormalE], {\[FormalC], \[FormalE]}, \[FormalH]];

ApproximateOrder[h_?VectorQ, err_?VectorQ] := \[FormalE] /. FitErrors[h, err]["BestFitParameters"]


End[];


EndPackage[];
