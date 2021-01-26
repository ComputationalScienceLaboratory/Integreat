(* ::Package:: *)

BeginPackage["Integreat`Trees`"];


Integreat`Trees::usage = "Package containing functions for ";
Tree::usage = "";
TreeDiffAlg::usage = "";
TreeDiff::usage = "";
TreeAlg::usage = "";

TreeOrder::usage = "";
TreeAlpha::usage = "";
TreeGamma::usage = "";
TreeSigma::usage = "";


Begin["`Private`"];

treeconstructor[func_, construct_] := treeconstructor[func, construct, func];
treeconstructor[func_, construct_, head_] := (
	func[{k_Integer?NonNegative}] := Map[head, construct[k]];
	func[k_Integer?Positive] := Map[head, construct[Range[k]], {2}];
	func[k1_Integer?NonNegative, k2_Integer?Positive] := Map[head, construct[Range[k1, k2]], {2}];
);

tree[0] := {\[FormalY]};
tree[1] := {\[FormalF]};
tree[p_] := tree[p] = Map[\[FormalF], DeleteDuplicates[Flatten[Map[Outer[Times, Sequence @@ #]&, tree[IntegerPartitions[p - 1]]]]]];
SetAttributes[tree, Listable];

treeorder[\[FormalY]] := 0;
treeorder[\[FormalG]|\[FormalF]] := 1;
treeorder[Power[t_, p_]] := p * treeorder[t];
treeorder[t_Times] := Total[Map[treeorder, List @@ t]];
treeorder[r:\[FormalG][t_]] := treeorder[r] = treeorder[t];
treeorder[r:\[FormalF][t_]] := treeorder[r] = 1 + treeorder[t];

treealpha[_Symbol] := 1;
treealpha[Power[t_, p_]] := With[{o = treeorder[t]},
	Quotient[Pochhammer[p + 1,  p * (o - 1)], (o!)^p] * treealpha[t]^p
];
treealpha[t_Times] := Apply[Multinomial, Map[treeorder, List @@ t]] * Map[treealpha, t];
treealpha[r:_Symbol[t_]] := treealpha[r] = treealpha[t];

treegamma[_Symbol] := 1;
treegamma[Power[t_, p_]] := treegamma[t]^p;
treegamma[t_Times] := Map[treegamma, t];
treegamma[r:\[FormalF][t_]] := treegamma[r] = treeorder[r] * treegamma[t];
treegamma[r:\[FormalG][t_]] := treegamma[r] = treegamma[t];

treesigma[_Symbol] := 1;
treesigma[Power[t_, p_]] := Factorial[p] * treesigma[t]^p;
treesigma[t_Times] := Map[treesigma, t];
treesigma[r:_Symbol[t_]] := treesigma[r] = treesigma[t];

treeda[0] := \[FormalY];
treeda[p_] := Join[treed[p], treea[p]];

treed[0] := \[FormalY];
treed[1] := {\[FormalF]};
treed[p_] := treed[p] = Map[\[FormalF], DeleteDuplicates[Flatten[Map[Outer[Times, Sequence @@ #]&, treeda[IntegerPartitions[p - 1]]]]]];

treea[0] := \[FormalY];
treea[1] := {\[FormalG][\[FormalF]]};
treea[p_] := treea[k] = Map[\[FormalG], Join[
	DeleteDuplicates[Flatten[Map[Outer[Times, Sequence @@ #]&, treeda[IntegerPartitions[p, {2, p}]]]]],
	treed[p]
]];
SetAttributes[{treeda, treed, treea} , Listable];

treeedges[_Symbol] = {};
treeedges[r:\[FormalF][t_]] := treeedges[r] = treeedges[t, 1, 2];
treeedges[\[FormalF], root_, counter_] := {UndirectedEdge[root, counter]};
treeedges[\[FormalF][t_], root_, counter_] := Append[treeedges[t, counter, counter+1], UndirectedEdge[root, counter]];
treeedges[Power[t_, p_], root_, counter_] := With[{o = treeorder[t]}, Join @@ Table[treeedges[t, root, counter + j * o], {j, 0, p - 1}]];
treeedges[t_Times, root_, counter_] := With[{t1 = First[t]}, Join[treeedges[Rest[t], root, counter + treeorder[t1]], treeedges[t1, root, counter]]];


treeconstructor[Tree, tree];

treeconstructor[TreeDiffAlg, treeda];
treeconstructor[TreeDiff, treed, TreeDiffAlg];
treeconstructor[TreeAlg, treea, TreeDiffAlg];

TreeOrder[(Tree | TreeDiffAlg)[t_]] := treeorder[t];
SetAttributes[TreeOrder, Listable];

TreeAlpha[(Tree | TreeDiffAlg)[t_]] := treealpha[t];
SetAttributes[TreeAlpha, Listable];

TreeGamma[(Tree | TreeDiffAlg)[t_]] := treegamma[t];
SetAttributes[TreeGamma, Listable];

TreeSigma[(Tree | TreeDiffAlg)[t_]] := treesigma[t];
SetAttributes[TreeSigma, Listable];

Tree /: Graph[Tree[t_]] := Graph[Range[treeorder[t]], treeedges[t], GraphLayout -> {"LayeredEmbedding", "RootVertex" -> 1, "Orientation" -> Bottom}, ImageSize -> Small];


End[];
EndPackage[];
