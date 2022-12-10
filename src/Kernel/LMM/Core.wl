(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


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
