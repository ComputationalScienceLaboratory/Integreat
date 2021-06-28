(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`Methods`"];
Integreat`Lmm`Methods::usage = "This package containing functions for creating and accessing basic properties of linear multistep methods.";

Lmm::usage =
	"Lmm[k] creates a generic, k-step linear multistep method\n" <>
	"Lmm[{\[Alpha]0, \[Alpha]1, \[Ellipsis], \[Alpha]k}, {\[Beta]0, \[Beta]1, \[Ellipsis], \[Beta]k}] create a k-step linear multistep method where the \[Alpha] coefficients multiply y's and the \[Beta] coefficients multiply f(y)'s.  Coefficients should be ordered from oldest to newest.";
LmmAdamsBashforth::usage = "LmmAdamsBashforth[k] creates a k-step Adams-Bashforth method.";
LmmAdamsMoulton::usage = "LmmAdamsMoulton[k] creates a k-step Adams-Moulton method.";
LmmNystrom::usage = "LmmNystrom[k] creates a k-step Nystr\[ODoubleDot]m method.";
LmmMilneSimpson::usage = "LmmMilneSimpson[k] creates a k-step Milne-Simpson method.";
LmmBdf::usage = "LmmBdf[k] creates a k-step backward differentiation formula method.";
LmmAlpha::usage = "LmmAlpha[lmm] gets the list of coefficients multiplying y's from lmm.  They are ordered from oldest to newest.";
LmmBeta::usage = "LmmBeta[lmm] gets the list of coefficients multiplying f(y)'s from lmm.  They are ordered from oldest to newest.";
LmmAlphaGeneratingPolynomial::usage = "LmmAlphaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Alpha] coefficients of lmm.";
LmmBetaGeneratingPolynomial::usage = "LmmBetaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Beta] coefficients of lmm.";
LmmSteps::usage = "LmmSteps[lmm] returns the number of previous steps required to compute the next step for lmm.";


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

LmmAlphaGeneratingPolynomial[HoldPattern[Lmm[a_, _]], zeta_] := FromDigits[Reverse[a], zeta];

LmmBetaGeneratingPolynomial[HoldPattern[Lmm[_, b_]], zeta_] := FromDigits[Reverse[b], zeta];

LmmSteps[HoldPattern[Lmm[a_, _]]] := Length[a] - 1;

Lmm /: Variables[HoldPattern[Lmm[args__]]] := Variables[{args}];

Lmm /: MakeBoxes[HoldPattern[Lmm[a_List, b_List]], format_] := With[{
		m = Sum[a[[i]] * Subscript[\[FormalY], \[FormalN] + i - 1], {i, Length[a]}] == \[FormalH] * Sum[b[[i]] * Subscript[\[FormalF], \[FormalN] + i - 1], {i, Length[a]}]
	}, MakeBoxes[m, format]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
