(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`LMM`Methods`"];

LMM::usage =
	"LMM[k] creates a generic, k-step linear multistep method\n" <>
	"LMM[{\[Alpha]0, \[Alpha]1, \[Ellipsis], \[Alpha]k}, {\[Beta]0, \[Beta]1, \[Ellipsis], \[Beta]k}] creates a k-step linear multistep method where the \[Alpha] coefficients multiply y's and the \[Beta] coefficients multiply f(y)'s. Coefficients should be ordered from oldest to newest.";
LMMAdamsBashforth::usage = "LMMAdamsBashforth[k] creates a k-step Adams-Bashforth method.";
LMMAdamsMoulton::usage = "LMMAdamsMoulton[k] creates a k-step Adams-Moulton method.";
LMMNystrom::usage = "LMMNystrom[k] creates a k-step Nystr\[ODoubleDot]m method.";
LMMMilneSimpson::usage = "LMMMilneSimpson[k] creates a k-step Milne-Simpson method.";
LMMBDF::usage = "LMMBDF[k] creates a k-step backward differentiation formula method.";
LMMAlpha::usage = "LMMAlpha[lmm] gets the list of coefficients multiplying y's from lmm. They are ordered from oldest to newest.";
LMMBeta::usage = "LMMBeta[lmm] gets the list of coefficients multiplying f(y)'s from lmm. They are ordered from oldest to newest.";
LMMAlphaGeneratingPolynomial::usage = "LMMAlphaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Alpha] coefficients of lmm.";
LMMBetaGeneratingPolynomial::usage = "LMMBetaGeneratingPolynomial[lmm, \[Zeta]] creates a polynomial in \[Zeta] using the \[Beta] coefficients of lmm.";
LMMSteps::usage = "LMMSteps[lmm] returns the number of previous steps required to compute the next step for lmm.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Internal`MathUtils`"];

vanderSolve[k_, rhs_] := LinearSolve[Table[SafePow[j, i], {i, 0, Length[rhs] - 1}, {j, 1 - k, Length[rhs] - k}], rhs];


(* ::Section:: *)
(*Package Definitions*)


LMM[k_Integer?Positive] := LMM[Table[Subscript[\[FormalAlpha], i], {i, 0, k}], Table[Subscript[\[FormalBeta], i], {i, 0, k}]];

LMMAdamsBashforth[k_Integer?Positive] := LMM[PadLeft[{-1, 1}, k + 1], Append[vanderSolve[k, 1 / Range[k]], 0]];

LMMAdamsMoulton[k_Integer?NonNegative] := LMM @@ PadLeft[{{-1, 1}, vanderSolve[k, 1 / Range[k + 1]]}];

LMMNystrom[k_Integer?Positive] := LMM @@ PadLeft[{{-1, 0, 1}, Append[vanderSolve[k, Table[If[OddQ[i], 2 / i, 0], {i, k}]], 0]}];

LMMMilneSimpson[k_Integer?NonNegative] := LMM @@ PadLeft[{{-1, 0, 1}, vanderSolve[k, Table[If[OddQ[i], 2 / i, 0], {i, k + 1}]]}];

LMMBDF[k_Integer?Positive] := With[{
		i = Range[k, 1, -1]
	},
	LMM[Append[(-1)^i * Binomial[k, i] / i, HarmonicNumber[k]], UnitVector[k + 1, k + 1]]
];

LMMAlpha[HoldPattern[LMM[a_, _]]] := a;

LMMBeta[HoldPattern[LMM[_, b_]]] := b;

LMMAlphaGeneratingPolynomial[HoldPattern[LMM[a_, _]], zeta_] := FromDigits[Reverse[a], zeta];

LMMBetaGeneratingPolynomial[HoldPattern[LMM[_, b_]], zeta_] := FromDigits[Reverse[b], zeta];

LMMSteps[HoldPattern[LMM[a_, _]]] := Length[a] - 1;

LMM /: Variables[HoldPattern[LMM[args__]]] := Variables[{args}];

LMM /: MakeBoxes[HoldPattern[LMM[a_List, b_List]], format_] := ToBoxes[
	Sum[a[[i]] * Subscript[\[FormalY], \[FormalN] + i - 1], {i, Length[a]}] == \[FormalH] * Sum[b[[i]] * Subscript[\[FormalF], \[FormalN] + i - 1], {i, Length[a]}],
	format
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
