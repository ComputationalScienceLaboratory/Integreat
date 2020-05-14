(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Methods`"];
Integreat`Lmm`Methods::usage = "Package containing functions for creating linear multistep methods";

Lmm::usage = "Constructs a linear multistep method";
LmmAdamsBashforth::usage = "Constructs an Adams-Bashforth method";
LmmAdamsMoulton::usage = "Constructs an Adams-Moulton method";
LmmBdf::usage = "Constructs a backward differentiation formula method";
LmmAlpha::usage = "Gets the alpha coefficients of a linear multistep method";
LmmBeta::usage = "Gets the beta coefficients of a linear multistep method";
LmmAlphaGeneratingPolynomial::usage = "";
LmmBetaGeneratingPolynomial::usage = "";
LmmSteps::usage = "Returns the number of previous steps required to compute the next";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


(* ::Section:: *)
(*Package Definitions*)


Lmm[k_Integer?Positive] := Lmm[Table[Subscript[\[FormalAlpha], i], {i, 0, k}], Table[Subscript[\[FormalBeta], i], {i, 0, k}]];

LmmAdamsBashforth[k_Integer?Positive] := Lmm[
	Join[ConstantArray[0, k - 1], {-1, 1}],
	Append[LinearSolve[Table[If[i == 0, 1, j^i], {i, 0, k - 1}, {j, k - 1, 0, -1}], Table[(-1)^(i - 1) / i, {i, k}]], 0]
];

LmmAdamsMoulton[0] := Lmm[{-1, 1}, {0, 1}];
LmmAdamsMoulton[k_Integer?Positive] /; k > 0 := Lmm[
	Join[ConstantArray[0, k - 1], {-1, 1}],
	LinearSolve[Table[If[i == 0, 1, j^i], {i, 0, k}, {j, k - 1, -1, -1}], Table[(-1)^i / (i + 1), {i, 0, k}]]
];

(*LmmNystrom[k_Integer] /; k > 0 := Lmm[
	
];*)

LmmBdf[k_Integer?Positive] /; k > 0 := With[{
		b0 = 1 / HarmonicNumber[k],
		i = Range[k, 1, -1]
	},
	Lmm[Append[(-1)^i * b0 * Binomial[k, i] / i, 1], Append[ConstantArray[0, k], b0]]
];

LmmAlpha[HoldPattern[Lmm[a_, _]]] := a;

LmmBeta[HoldPattern[Lmm[_, b_]]] := b;

LmmAlphaGeneratingPolynomial[HoldPattern[Lmm[a_, _]], xi_] := Sum[a[[k]] * xi^(k - 1), {k, Length[a]}];

LmmBetaGeneratingPolynomial[HoldPattern[Lmm[_, b_]], xi_] := Sum[b[[k]] * xi^(k - 1), {k, Length[b]}];

LmmSteps[HoldPattern[Lmm[a_, _]]] := Length[a] - 1;

Lmm /: Length[HoldPattern[Lmm[a_, _]]] := Length[a] - 1;

Lmm /: Variables[HoldPattern[Lmm[a, b]]] := Variables[{a, b}];

Lmm /: MakeBoxes[HoldPattern[Lmm[a_List, b_List]], format_] := With[{
		m = Sum[a[[i]] * Subscript[\[FormalY], \[FormalN] + i - 1], {i, Length[a]}] == \[FormalH] * Sum[b[[i]] * Subscript[\[FormalF], \[FormalN] + i - 1], {i, Length[a]}]
	}, MakeBoxes[m, format]];


(* ::Section:: *)
(*Error Handling*)


Lmm::args = "Lmm called with `1` arguments; must have \[Alpha] and \[Beta]";
Lmm[args___] /; Length[{args}] != 2 := (Message[Lmm::args, Length[{args}]]; $Failed);
Lmm::vector = "Lmm `1` coefficients must be a vector";
Lmm[a_, _] /; Not[VectorQ[a]] := (Message[Lmm::vector, "\[Alpha]"]; $Failed);
Lmm[_, b_] /; Not[VectorQ[b]] := (Message[Lmm::vector, "\[Beta]"]; $Failed);
Lmm::length = "Lmm \[Alpha] and \[Beta] coefficients must have the same length";
Lmm[a_, b_] /; Length[a] != Length[b] := (Message[Lmm::length]; $Failed);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
