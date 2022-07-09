(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`BTrees`"];

BTree::usage =
	"BTree[p] generates all B-Trees up to order p grouped by order.\n" <>
	"BTree[{p}] generates a list of B-Trees of order p.";
BTreeN::usage =
	"BTreeN[p] generates all N-Trees up to order p grouped by order.\n" <>
	"BTreeN[{p}] generates a list of N-Trees of order p.";
BTreeDAE::usage =
	"BTreeDAE[p] generates all DAE-Trees up to order p grouped by order.\n" <>
	"BTreeDAE[{p}] generates a list of DAE-Trees of order p.";

BTreeOrder::usage = "BTreeOrder[t] computes the order of the tree t.";
BTreeAlpha::usage = "BTreeAlpha[t] computes the number of monotonic labelings of the tree t.";
BTreeGamma::usage = "BTreeGamma[t] computes the density of the tree t.";
BTreeSigma::usage = "BTreeSigma[t] computes the number of symmetries of the tree t.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

treeMap[head_, treeFun_, {p_}] := Map[head, treeFun[p]];
treeMap[head_, treeFun_, p_] := Map[head, Table[treeFun[i], {i, p}], {2}];

(* Allow KroneckerProduct to accept 1 argument. For lists, this works like TensorProduct but has less List nesting and is faster. *)
kron[x_] := x;
kron[x__] := KroneckerProduct[x];

(* Generates all sets of subtrees with at least minSubtrees elements and p total vertices *)
subtrees[head_, p_, minSubtrees_:1] := DeleteDuplicates[Flatten[kron @@@ Map[head, IntegerPartitions[p, {minSubtrees, p}], {2}]]];


(* ::Subsection:: *)
(*B-Trees*)


tree[0] := {\[FormalY]};
tree[1] := {\[FormalF]};
tree[p_] := tree[p] = Map[\[FormalF], subtrees[tree, p - 1]];


(* ::Subsection:: *)
(*N-Trees*)


treeN[_][0] := {\[FormalY]};
treeN[n_][1] := Table[Subscript[\[FormalF], i], {i, n}];
treeN[n_][p_] := treeN[n][p] = Flatten[Outer[Construct, treeN[n][1], subtrees[treeN[n], p - 1]]];


(* ::Subsection:: *)
(*DAE-Trees*)


treeDiffAlg[idx_][p_] := Join[treeDiff[idx][p], treeAlg[idx][p]];

treeDiff[idx_, start_:1, q_:0][p_] := Catenate[Table[treeDiff[idx, {i}, q][p], {i, start, Max[1, idx - 1]}]];
treeDiff[_, {part_}, _][0] := {Subscript[\[FormalY], part]};
treeDiff[_, {part_}, _][1] := {Subscript[\[FormalF], part]};
treeDiff[idx_, {1}, _][p_] := treeDiff[idx, {1}, _][p] = Map[Subscript[\[FormalF], 1], subtrees[treeDiffAlg[idx], p - 1]];
treeDiff[idx_, {1}, p_][p_] := treeDiff[idx, {1}, p][p] = Map[Subscript[\[FormalF], 1], Join[subtrees[treeDiffAlg[idx], p - 1, 2], treeDiff[idx][p - 1]]];
treeDiff[idx_, {part_}, q_][p_] := treeDiff[idx, {part}, q][p] = Map[Subscript[\[FormalF], part], subtrees[treeDiff[idx, part - 1, q], p - 1]];

treeAlg[idx_][0] := {Subscript[\[FormalY], Max[2, idx]]};
treeAlg[0][1] := {Subscript[\[FormalG], 0]};
treeAlg[0][p_] := treeAlg[0][p] = Map[Subscript[\[FormalG], 0], subtrees[treeDiffAlg[0], p - 1]];
treeAlg[1][p_] := treeAlg[1][p] = Map[Subscript[\[FormalG], 1], Join[subtrees[treeDiffAlg[1], p, 2], treeDiff[1][p]]];
treeAlg[idx_][p_] := treeAlg[idx][p] = Map[Subscript[\[FormalG], idx], subtrees[treeDiff[idx, {idx - 1}, p + 1], p + idx - 1]];


(* ::Subsection:: *)
(*Tree Functions*)


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


(* ::Subsection:: *)
(*Conversions*)


toTree[t:(_Symbol | _Subscript)] := Tree[t, {}];
toTree[Power[t_, p_]] := Splice[ConstantArray[toTree[t], p]];
toTree[t_Times] := Splice[Map[toTree, List @@ t]];
toTree[r:h_[t_]] := toTree[r] = Tree[h, {toTree[t]}];

toGraph[\[FormalY] | Subscript[\[FormalY], _]] := Graph[{}];
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
			{Subscript[\[FormalG], _], _} -> White,
			{Subscript[\[FormalF], n_], _} :> ColorData[3, n],
			Black
		},
		VertexLabels -> {{Subscript[\[FormalF], n_], _} -> n},
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


BTree[p:_Integer?Positive | {_Integer?NonNegative}] := treeMap[BTree, tree, p];
BTree /: Tree[HoldPattern[BTree[t_]]] := toTree[t];
BTree /: Graph[HoldPattern[BTree[t_]]] := toGraph[t];
BTree /: MakeBoxes[HoldPattern[BTree[t_]], format_] := toBoxes[t, format];

Options[BTreeN] = {"Partitions" -> 2};
BTreeN[p:_Integer?Positive | {_Integer?NonNegative}, OptionsPattern[]] := With[{
		n = OptionValue[Partitions]
	},
	treeMap[BTreeN[#, n] &, treeN[n], p] /; IntegerQ[n] && Positive[n]
];
BTreeN /: Tree[HoldPattern[BTreeN[t_, _Integer]]] := toTree[t];
BTreeN /: Graph[HoldPattern[BTreeN[t_, _Integer]]] := toGraph[t];
BTreeN /: MakeBoxes[HoldPattern[BTreeN[t_, _Integer]], format_] := toBoxes[t, format];

Options[BTreeDAE] = {"Index" -> 1, "Partition" -> All};
BTreeDAE[p:_Integer?Positive | {_Integer?NonNegative}, OptionsPattern[]] := With[{
		idx = OptionValue[Index],
		treeFun = Switch[OptionValue[Partition],
			All, treeDiffAlg,
			"Differential", treeDiff,
			"Algebraic", treeAlg,
			_, True
		]
	},
	treeMap[BTreeDAE[#, idx] &, treeFun[idx], p] /; IntegerQ[idx] && NonNegative[idx] && !TrueQ[treeFun] 
];
BTreeDAE /: Tree[HoldPattern[BTreeDAE[t_, _Integer]]] := toTree[t];
BTreeDAE /: Graph[HoldPattern[BTreeDAE[t_, _Integer]]] := toGraph[t];
BTreeDAE /: MakeBoxes[HoldPattern[BTreeDAE[t_, _Integer]], format_] := toBoxes[t, format];

BTreeOrder[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeOrder[t];
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
