(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


treeOrder[\[FormalY] | Subscript[\[FormalY], _]] := 0;
treeOrder[_Symbol | _Subscript] := 1;
treeOrder[Power[t_, p_]] := p * treeOrder[t];
treeOrder[t_Times] := Map[treeOrder, Plus @@ t];
treeOrder[r:(\[FormalF] | Subscript[\[FormalF], _])[t_]] := treeOrder[r] = treeOrder[t] + 1;
treeOrder[r:Subscript[\[FormalG], idx_][t_]] := treeOrder[r] = treeOrder[t] + 1 - idx;


treeAlpha[_Symbol | _Subscript] := 1;
treeAlpha[Power[t_, p_]] := With[{o = treeOrder[t]},
	Quotient[Pochhammer[p + 1,  p * (o - 1)], (o!)^p] * treeAlpha[t]^p
];
treeAlpha[t_Times] := Apply[Multinomial, Map[treeOrder, List @@ t]] * Map[treeAlpha, t];
treeAlpha[r:_[t_]] := treeAlpha[r] = treeAlpha[t];


treeGamma[_Symbol | _Subscript] := 1;
treeGamma[Power[t_, p_]] := treeGamma[t]^p;
treeGamma[t_Times] := Map[treeGamma, t];
treeGamma[r:(\[FormalF] | _Subscript)[t_]] := treeGamma[r] = treeOrder[r] * treeGamma[t];
treeGamma[r:Subscript[\[FormalG], idx_][t_]] := treeGamma[r] = treeGamma[t] / Pochhammer[treeOrder[r] + 1, idx - 1];


treeSigma[_Symbol | _Subscript] := 1;
treeSigma[Power[t_, p_]] := Factorial[p] * treeSigma[t]^p;
treeSigma[t_Times] := Map[treeSigma, t];
treeSigma[r:_[t_]] := treeSigma[r] = treeSigma[t];


(* ::Section:: *)
(*Package Definitions*)


BTreeOrder[t_?BTreeQ] := treeOrder[First[t]];
SetAttributes[BTreeOrder, Listable];


BTreeAlpha[t_?BTreeQ] := treeAlpha[First[t]];
SetAttributes[BTreeAlpha, Listable];


BTreeGamma[t_?BTreeQ] := treeGamma[First[t]];
SetAttributes[BTreeGamma, Listable];


BTreeSigma[t_?BTreeQ] := treeSigma[First[t]];
SetAttributes[BTreeSigma, Listable];
