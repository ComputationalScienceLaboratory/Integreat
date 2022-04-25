(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`BTrees`"];
Integreat`BTrees::usage = "Package containing functions for ";
BTree::usage = "";
BTreeN::usage = "";
BTreeDAE::usage = "";

BTreeOrder::usage = "";
BTreeAlpha::usage = "";
BTreeGamma::usage = "";
BTreeSigma::usage = "";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

(* Allow KroneckerProduct to accept 1 argument. For lists, this works like TensorProduct but has less List nesting and is faster. *)
kron[x_] := x;
kron[x__] := KroneckerProduct[x];

(* Generates all sets of subtrees with at least minSubtrees elements and p total vertices *)
subtrees[head_, p_, minSubtrees_:1] := DeleteDuplicates[Flatten[kron @@@ head[IntegerPartitions[p, {minSubtrees, p}]]]];


(* ::Subsection:: *)
(*B-Trees*)


tree[0] := {\[FormalY]};
tree[1] := {\[FormalF]};
tree[p_] := tree[p] = Map[\[FormalF], subtrees[tree, p - 1]];
SetAttributes[tree, Listable];


(* ::Subsection:: *)
(*N-Trees*)


treeN[0, n_] := {\[FormalY]};
treeN[1, n_] := Table[Subscript[\[FormalF], i], {i, n}];
treeN[p_, n_] := treeN[p, n] = Flatten[Outer[Construct, treeN[1, n], subtrees[treeN[#, n] &, p - 1]]];
SetAttributes[treeN, Listable];


(* ::Subsection:: *)
(*DAE-Trees*)


treeDiffAlg[0, _] := {\[FormalY]};
treeDiffAlg[p_, index_] := Join[treeDiff[p, index], treeAlg[p, index]];

treeDiff[0, _] := {\[FormalY]};
treeDiff[1, _] := {\[FormalF]};
treeDiff[p_, 1] := treeDiff[p, 1] = Map[\[FormalF], subtrees[treeDiffAlg[#, 1] &, p - 1]];
treeDiff[p_, 2] := treeDiff[p, 2] = Join[treeDiff2[p], Map[\[FormalF], treeAlg[p - 1, 2]]]; 

(* Index-2 differential trees excluding ones with a single algebraic subtree *)
treeDiff2[p_] := treeDiff2[p] = Map[\[FormalF], Join[subtrees[treeDiffAlg[#, 2] &, p - 1, 2], treeDiff[p - 1, 2]]];

treeAlg[0, _] := {\[FormalY]};
treeAlg[p_, 1] := treeAlg[p, 1] = Map[\[FormalG], Join[subtrees[treeDiffAlg[#, 1] &, p, 2], treeDiff[p, 1]]];
treeAlg[p_, 2] := treeAlg[p, 2] = Map[\[FormalH], Join[subtrees[treeDiff[#, 2] &, p + 1, 2], treeDiff2[p + 1]]];
SetAttributes[{treeDiffAlg, treeDiff, treeDiff2, treeAlg}, Listable];

typeToDAETree[type_] := Switch[type, "Differential", treeDiff, "Algebraic", treeAlg, _, treeDiffAlg];


(* ::Subsection:: *)
(*Tree Functions*)


treeOrder[\[FormalY], ___] := 0;
treeOrder[_Symbol] := 1;
treeOrder[Subscript[\[FormalF], m_], n___] := Boole[m == n];
treeOrder[Power[t_, p_], n___] := p * treeOrder[t, n];
treeOrder[t_Times, n___] := Total[Map[treeOrder[#, n] &, List @@ t]];
treeOrder[r:\[FormalF][t_]] := treeOrder[r] = treeOrder[t] + 1;
treeOrder[r:\[FormalG][t_]] := treeOrder[r] = treeOrder[t];
treeOrder[r:\[FormalH][t_]] := treeOrder[r] = treeOrder[t] - 1;
treeOrder[r:Subscript[\[FormalF], m_][t_], n___] := treeOrder[r, n] = Boole[m == n] + treeOrder[t, n];

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
treeGamma[r:\[FormalG][t_]] := treeGamma[r] = treeGamma[t];
treeGamma[r:\[FormalH][t_]] := treeGamma[r] = treeGamma[t] / (treeOrder[r] + 1);

treeSigma[_Symbol | _Subscript] := 1;
treeSigma[Power[t_, p_]] := Factorial[p] * treeSigma[t]^p;
treeSigma[t_Times] := Map[treeSigma, t];
treeSigma[r:_[t_]] := treeSigma[r] = treeSigma[t];


(* ::Subsection:: *)
(*Conversions*)


toTree[t:(_Symbol | _Subscript)] := Tree[t, {}];
toTree[Power[t_, p_]] := Splice[ConstantArray[toTree[t], p]];
toTree[t_Times] := Splice[Map[toTree, List @@ t]];
toTree[r:h_[t_]] := toTree[r] = Tree[h, {toTree[t]}];

toGraph[\[FormalY]] := Graph[{}];
toGraph[t_] := With[{
		tree = toTree[t]
	},
	UndirectedGraph[
		Graph[tree],
		GraphLayout -> {
			"LayeredEmbedding",
			"Orientation" -> Bottom,
			"RootVertex" -> {TreeData[tree], {}}
		},
		EdgeStyle -> Black,
		VertexSize -> Large,
		VertexStyle -> {
			{\[FormalG], _} -> White,
			{\[FormalH], _} -> LightGray,
			{Subscript[_, n_], _} :> ColorData[1, n],
			Black
		},
		VertexLabels -> {{Subscript[_, n_], _} -> n},
		ImageSize -> Tiny
	]
];

If[
	TrueQ[$VersionNumber >= 12.3],
	toBoxes[t_, format_] := ToBoxes[toGraph[t], format],
	toBoxes[t_, format_] := ToBoxes[t, format]
];


(* ::Section:: *)
(*Package Definitions*)


BTree[{p_Integer?NonNegative}] := Map[BTree, tree[p]];
BTree[pStart:_Integer?NonNegative:1, pEnd_Integer?NonNegative] := Map[BTree, tree[Range[pStart, pEnd]], {2}];
BTree /: Tree[HoldPattern[BTree[t_]]] := toTree[t];
BTree /: Graph[HoldPattern[BTree[t_]]] := toGraph[t];
BTree /: MakeBoxes[HoldPattern[BTree[t_]], format_] := toBoxes[t, format];

Options[BTreeN] = {"Partitions" -> 2};
BTreeN[{p_Integer?NonNegative}, OptionsPattern[]] := With[{
		n = OptionValue[Partitions]
	},
	Map[BTreeN[#, n] &, treeN[p, n]] /; IntegerQ[n] && Positive[n]
];
BTreeN[pStart:_Integer?NonNegative:1, pEnd_Integer?NonNegative, OptionsPattern[]] := With[{
		n = OptionValue[Partitions]
	},
	Map[BTreeN[#, n] &, treeN[Range[pStart, pEnd], n], {2}] /; IntegerQ[n] && Positive[n]
];
BTreeN /: Tree[HoldPattern[BTreeN[t_, _Integer]]] := toTree[t];
BTreeN /: Graph[HoldPattern[BTreeN[t_, _Integer]]] := toGraph[t];
BTreeN /: MakeBoxes[HoldPattern[BTreeN[t_, _Integer]], format_] := toBoxes[t, format];

Options[BTreeDAE] = {"Index" -> 1, "Type" -> "All"};
BTreeDAE[{p_Integer?NonNegative}, OptionsPattern[]] := With[{
		index = OptionValue[Index],
		treeFun = typeToDAETree[OptionValue[Type]]
	},
	Map[BTreeDAE[#, index] &, treeFun[p, index]] /; index === 1 || index === 2
];
BTreeDAE[pStart:_Integer?NonNegative:1, pEnd_Integer?NonNegative, OptionsPattern[]] := With[{
		index = OptionValue[Index],
		treeFun = typeToDAETree[OptionValue[Type]]
	},
	Map[BTreeDAE[#, index] &, treeFun[Range[pStart, pEnd], index], {2}] /; index === 1 || index === 2
];
BTreeDAE /: Tree[HoldPattern[BTreeDAE[t_, _Integer]]] := toTree[t];
BTreeDAE /: Graph[HoldPattern[BTreeDAE[t_, _Integer]]] := toGraph[t];
BTreeDAE /: MakeBoxes[HoldPattern[BTreeDAE[t_, _Integer]], format_] := toBoxes[t, format];

BTreeOrder[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeOrder[t];
BTreeOrder[BTreeN[t_, _], n_Integer?Positive] := treeOrder[t, n];
SetAttributes[BTreeOrder, Listable];

BTreeAlpha[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeAlpha[t];
SetAttributes[BTreeAlpha, Listable];

BTreeGamma[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeGamma[t];
SetAttributes[BTreeGamma, Listable];

BTreeSigma[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeSigma[t];
SetAttributes[BTreeSigma, Listable];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
