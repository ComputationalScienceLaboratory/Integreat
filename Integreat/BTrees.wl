(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`BTrees`"];
Integreat`BTrees::usage = "Package containing functions for ";
BTree::usage = "";
BTreeN::usage = "";
BTreeDiffAlg::usage = "";
BTreeDiff::usage = "";
BTreeAlg::usage = "";

BTreeOrder::usage = "";
BTreeAlpha::usage = "";
BTreeGamma::usage = "";
BTreeSigma::usage = "";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

branches[head_, p__] := DeleteDuplicates[Flatten[Map[Outer[Times, Sequence @@ #]&, head[IntegerPartitions[p]]]]];


(* ::Subsection:: *)
(*B-Trees*)


tree[0] := {\[FormalY]};
tree[1] := {\[FormalF]};
tree[p_] := tree[p] = Map[\[FormalF], branches[tree, p - 1]];
SetAttributes[tree, Listable];


(* ::Subsection:: *)
(*N-Trees*)


treeN[n_, 0] := \[FormalY];
treeN[n_, 1] := Table[Subscript[\[FormalF], i], {i, n}];
treeN[n_, p_] := treeN[n, p] = Flatten[Outer[Construct, treeN[n, 1], branches[treeN[n, #] &, p - 1]]];
SetAttributes[treeN, Listable];


(* ::Subsection:: *)
(*DAE-Trees*)


treeDiffAlg[0] := \[FormalY];
treeDiffAlg[p_] := Join[treeDiff[p], treeAlg[p]];

treeDiff[0] := \[FormalY];
treeDiff[1] := {\[FormalF]};
treeDiff[p_] := treeDiff[p] = Map[\[FormalF], branches[treeDiffAlg, p - 1]];

treeAlg[0] := \[FormalY];
treeAlg[1] := {\[FormalG][\[FormalF]]};
treeAlg[p_] := treeAlg[k] = Map[\[FormalG], Join[branches[treeDiffAlg, p, {2, p}], treeDiff[p]]];
SetAttributes[{treeDiffAlg, treeDiff, treeAlg} , Listable];


(* ::Subsection:: *)
(*Tree Functions*)


treeOrder[\[FormalY]] := 0;
treeOrder[_Symbol | _Subscript] := 1;
treeOrder[Power[t_, p_]] := p * treeOrder[t];
treeOrder[t_Times] := Total[Map[treeOrder, List @@ t]];
treeOrder[r:\[FormalG][t_]] := treeOrder[r] = treeOrder[t];
treeOrder[r:(\[FormalF] | _Subscript)[t_]] := treeOrder[r] = 1 + treeOrder[t];

treeAlpha[_Symbol | _Subscript] := 1;
treeAlpha[Power[t_, p_]] := With[{o = treeOrder[t]},
	Quotient[Pochhammer[p + 1,  p * (o - 1)], (o!)^p] * treeAlpha[t]^p
];
treeAlpha[t_Times] := Apply[Multinomial, Map[treeOrder, List @@ t]] * Map[treeAlpha, t];
treeAlpha[r:(_Symbol | _Subscript)[t_]] := treeAlpha[r] = treeAlpha[t];

treeGamma[_Symbol | _Subscript] := 1;
treeGamma[Power[t_, p_]] := treeGamma[t]^p;
treeGamma[t_Times] := Map[treeGamma, t];
treeGamma[r:(\[FormalF] | _Subscript)[t_]] := treeGamma[r] = treeOrder[r] * treeGamma[t];
treeGamma[r:\[FormalG][t_]] := treeGamma[r] = treeGamma[t];

treeSigma[_Symbol | _Subscript] := 1;
treeSigma[Power[t_, p_]] := Factorial[p] * treeSigma[t]^p;
treeSigma[t_Times] := Map[treeSigma, t];
treeSigma[r:(_Symbol | _Subscript)[t_]] := treeSigma[r] = treeSigma[t];

treeEdges[_Symbol] = {};
treeEdges[r:\[FormalF][t_]] := treeEdges[r] = treeEdges[t, 1, 2];
treeEdges[\[FormalF], root_, counter_] := {UndirectedEdge[root, counter]};
treeEdges[\[FormalF][t_], root_, counter_] := Append[treeEdges[t, counter, counter+1], UndirectedEdge[root, counter]];
treeEdges[Power[t_, p_], root_, counter_] := With[{o = treeOrder[t]}, Join @@ Table[treeEdges[t, root, counter + j * o], {j, 0, p - 1}]];
treeEdges[t_Times, root_, counter_] := With[{t1 = First[t]}, Join[treeEdges[Rest[t], root, counter + treeOrder[t1]], treeEdges[t1, root, counter]]];


(* ::Section:: *)
(*Package Definitions*)


BTree[{p_Integer?NonNegative}] := Map[BTree, tree[p]];
BTree[p_Integer?Positive] := BTree[1, p];
BTree[pStart_Integer?NonNegative, pEnd_Integer?NonNegative] := Map[BTree, tree[Range[pStart, pEnd]], {2}];
BTree /: MakeBoxes[HoldPattern[BTree[t_]], format_] := MakeBoxes[t, format];

BTreeN[n_Integer?Positive, {p_Integer?NonNegative}] := Map[BTreeN[#, n] &, treeN[n, p]];
BTreeN[n_Integer?Positive, p_Integer?Positive] := BTreeN[n, 1, p];
BTreeN[n_Integer?Positive, pStart_Integer?NonNegative, pEnd_Integer?NonNegative] := Map[BTreeN[#, n] &, treeN[n, Range[pStart, pEnd]], {2}];
BTreeN /: MakeBoxes[HoldPattern[BTreeN[t_, _]], format_] := MakeBoxes[t, format];

BTreeDiffAlg[{p_Integer?NonNegative}] := Map[BTreeDiffAlg, treeDiffAlg[p]];
BTreeDiffAlg[p_Integer?Positive] := BTreeDiffAlg[1, p];
BTreeDiffAlg[pStart_Integer?NonNegative, pEnd_Integer?NonNegative] := Map[BTreeDiffAlg, treeDiffAlg[Range[pStart, pEnd]], {2}];
BTreeDiffAlg /: MakeBoxes[HoldPattern[BTreeDiffAlg[t_]], format_] := MakeBoxes[t, format];

BTreeDiff[{p_Integer?NonNegative}] := Map[BTreeDiffAlg, treeDiff[p]];
BTreeDiff[p_Integer?Positive] := BTreeDiff[1, p];
BTreeDiff[pStart_Integer?NonNegative, pEnd_Integer?NonNegative] := Map[BTreeDiffAlg, treeDiff[Range[pStart, pEnd]], {2}];

BTreeAlg[{p_Integer?NonNegative}] := Map[BTreeDiffAlg, treeAlg[p]];
BTreeAlg[p_Integer?Positive] := BTreeAlg[1, p];
BTreeAlg[pStart_Integer?NonNegative, pEnd_Integer?NonNegative] := Map[BTreeDiffAlg, treeAlg[Range[pStart, pEnd]], {2}];

BTreeOrder[(BTree | BTreeN | BTreeDiffAlg)[t_, ___]] := treeOrder[t];
SetAttributes[BTreeOrder, Listable];

BTreeAlpha[(BTree | BTreeN | BTreeDiffAlg)[t_, ___]] := treeAlpha[t];
SetAttributes[BTreeAlpha, Listable];

BTreeGamma[(BTree | BTreeN | BTreeDiffAlg)[t_, ___]] := treeGamma[t];
SetAttributes[BTreeGamma, Listable];

BTreeSigma[(BTree | BTreeN | BTreeDiffAlg)[t_, ___]] := treeSigma[t];
SetAttributes[BTreeSigma, Listable];

BTree /: Graph[HoldPattern[BTree[t_]]] := Graph[Range[treeOrder[t]], treeEdges[t], GraphLayout -> {"LayeredEmbedding", "RootVertex" -> 1, "Orientation" -> Bottom}, ImageSize -> Small];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
