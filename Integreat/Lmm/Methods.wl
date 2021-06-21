(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Methods`"];
Integreat`Lmm`Methods::usage = "Package containing functions for creating linear multistep methods";

Lmm::usage = "Constructs a linear multistep method";
LmmAdamsBashforth::usage = "Constructs an Adams-Bashforth method";
LmmAdamsMoulton::usage = "Constructs an Adams-Moulton method";
LmmNystrom::usage = "";
LmmMilneSimpson::usage = "";
LmmBdf::usage = "Constructs a backward differentiation formula method";
LmmAlpha::usage = "Gets the alpha coefficients of a linear multistep method";
LmmBeta::usage = "Gets the beta coefficients of a linear multistep method";
LmmAlphaGeneratingPolynomial::usage = "";
LmmBetaGeneratingPolynomial::usage = "";
LmmSteps::usage = "Returns the number of previous steps required to compute the next";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

vanderSolve[k_, rhs_] := LinearSolve[Table[If[i == 0, 1, j^i], {i, 0, Length[rhs] - 1}, {j, 1 - k, Length[rhs] - k}], rhs];


(* ::Section:: *)
(*Package Definitions*)


Lmm[k_Integer?Positive] := Lmm[Table[Subscript[\[FormalAlpha], i], {i, 0, k}], Table[Subscript[\[FormalBeta], i], {i, 0, k}]];

LmmAdamsBashforth[k_Integer?Positive] := Lmm[PadLeft[{-1, 1}, k + 1], Append[vanderSolve[k, 1 / Range[k]], 0]];

LmmAdamsMoulton[k_Integer?NonNegative] := Lmm @@ PadLeft[{{-1, 1}, vanderSolve[k, 1 / Range[k + 1]]}];

LmmNystrom[k_Integer?Positive] := Lmm @@ PadLeft[{{-1, 0, 1}, Append[vanderSolve[k, Table[If[OddQ[i], 2 / i, 0], {i, k}]], 0]}];

LmmMilneSimpson[k_Integer?NonNegative] := Lmm @@ PadLeft[{{-1, 0, 1}, vanderSolve[k, Table[If[OddQ[i], 2 / i, 0], {i, k + 1}]]}];

LmmBdf[k_Integer?Positive] := With[{
		i = Range[k, 1, -1]
	},
	Lmm[Append[(-1)^i * Binomial[k, i] / i, HarmonicNumber[k]], UnitVector[k + 1, k + 1]]
];

LmmAlpha[HoldPattern[Lmm[a_, _]]] := a;

LmmBeta[HoldPattern[Lmm[_, b_]]] := b;

LmmAlphaGeneratingPolynomial[HoldPattern[Lmm[a_, _]], xi_] := FromDigits[Reverse[a], xi];

LmmBetaGeneratingPolynomial[HoldPattern[Lmm[_, b_]], xi_] := FromDigits[Reverse[b], xi];

LmmSteps[HoldPattern[Lmm[a_, _]]] := Length[a] - 1;

Lmm /: Variables[HoldPattern[Lmm[args__]]] := Variables[{args}];

Lmm /: MakeBoxes[HoldPattern[Lmm[a_List, b_List]], format_] := With[{
		m = Sum[a[[i]] * Subscript[\[FormalY], \[FormalN] + i - 1], {i, Length[a]}] == \[FormalH] * Sum[b[[i]] * Subscript[\[FormalF], \[FormalN] + i - 1], {i, Length[a]}]
	}, MakeBoxes[m, format]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
