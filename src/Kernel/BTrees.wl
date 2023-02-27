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

BTreePrune::usage = "Get all prunings of tree T with tree S and get its Forest Space";
BTreeContract::usage = "Get all cuts of tree T which contract to tree S and get its Forest Space";
BTreeSubTrees::usage = "Get All Subtrees of a Tree as a List";
BTreeRoot::usage = "Get the root node of a tree";
BTreeChildren::usage = "Get children of a tree and return as a Forest Space";

BTreeFormalForm::usage = "Get the Formal Representation of the Tree";


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

(*Inner sub tree function for only formal expansions*)
innerBSubtrees[t_]:=Complement[DeleteDuplicates[butcherSubtrees[t]], {t}];
 
 (*Expand out power terms in a list to individual terms*)
listPowerRemove[x_]:=Flatten[Replace[x,{Power[y_,p_] :>ConstantArray[y,p]},{0,1}]];

(*Flatten a Forest Space into a flat List *)
linearCombToList[x_Plus]:=Flatten[Replace[List@@x,{Times[c_Integer,y_] :>ConstantArray[y,c]},{0,1}]];
linearCombToList[Times[y_Integer,x_]]:=ConstantArray[x,y];
linearCombToList[x_]:={x};

(*Generalized Outer Product on two list with a Cofactor Expansion applied where there are no alternating signs*)
outerACE[op_,t_List /;Length[t] ==2,k_List /;Length[k] ==2]:=op[t[[1]],k[[1]]]*op[t[[2]],k[[2]]] + op[t[[1]],k[[2]]]*op[t[[2]],k[[1]]];
outerACE[op_,t_List,k_List] := Sum[op[First[t],k[[i]]]*outerACE[op,Drop[t,1],Delete[k,i]],{i,1,Length[k]}];

(*Given a list of trees with powers in entries get the factorial of each power and product*)
factorialProd[t_]:=Times@@(#!&/@Replace[Cases[t,_Power,{0,1}],{Power[y_,p_] :>p},{0,1}]);


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

getRoot[t_Symbol]:=t;
getRoot[t_Subscript]:=t;
getRoot[t_Power]:=0;
getRoot[t_Times]:=0;
getRoot[t_Plus]:=0;
getRoot[t_]:=Head[t];

getChildren[_Symbol | _Subscript | _Power | _Times | _Plus]:=0;
getChildren[t_[h_]]:=h;

subTrees[k_]:={k};
subTrees[k_[t_]]:=Append[Map[k,subTrees[t]],k];
subTrees[t_Times]:=Times@@@Most[Tuples[Map[Append[subTrees[#],1]&,List@@t]]];
subTrees[t_^p_]:=Times@@@Most[Tuples[Append[subTrees[t],1],p]];


(* ::Subsection:: *)
(*Tree Functions (Binary)*)


prune[t_,t_]:=1;
prune[t_,k_]:=0;
prune[t_,1]:=t;
prune[t_[u_],t_]:=u;
prune[t_[u_],t_[g_]]:=prune[u,g];
prune[t_Times, k_]:=Sum[prune[t[[i]],k]*(Length[t] - 1)!*Delete[t,i],{i,1,Length[t]}];
prune[t_,k_Times]:=0;
prune[t_Times, k_Times]:=With[{tpad = listPowerRemove[List@@t], kpad =listPowerRemove[List@@k] },If[Length[tpad]>=Length[kpad],(1/factorialProd[List@@k])*outerACE[prune,tpad,PadRight[kpad,Length[tpad],1]],0]];
prune[t_^p_,k_Times]:=With[{kpad = listPowerRemove[List@@k]},If[p>=Length[kpad],(p! /factorialProd[List@@k] )*Product[prune[t,kpad[[i]]],{i,1,Length[kpad]}]*prune[t,1]^(p-Length[kpad]),0]];
prune[t_Times,k_^q_]:=With[{tpad=listPowerRemove[List@@t]},If[Length[tpad]>=q,(1/q!)*outerACE[prune,tpad,PadRight[ConstantArray[k,q],Length[tpad],1]],0]];
prune[t_^p_,k_]:=p*prune[t,k]*prune[t,1]^(p-1);
prune[t_^p_,k_^q_]:=If[p>=q,Binomial[p,q]*prune[t,k]^(q)*prune[t,1]^(p-q),0];

contract[t_,k_]:=If[getRoot[t]===getRoot[k],t,0];
contract[t_,k_[u_]]:=If[getRoot[t]===k,Total[#*innerTreeSub[ Flatten[linearCombFlattenToList[Expand[prune[t,#]]]],u]&/@innerBSubtrees[t],2],0];
innerContract[tl_,u_]:=contract[#,u]&/@tl;
contract[t_Times,k_]:=0;
contract[t_,k_Times]:=0;
contract[t_Times,k_[u_]]:=0;
contract[t_[w_],k_Times]:=0;
contract[t_^p_,k_]:=0;
contract[t_,k_^q_]:=0;
contract[t_^p_,k_[u_]]:=0;
contract[t_[w_],k_^q_]:=0;
contract[t_Times,k_Times]:=With[{tpad = listPowerRemove[List@@t],kpad=listPowerRemove[List@@k]},If[Length[tpad]==Length[kpad],(1/factorialProd[List@@k])*outerACE[contract,tpad,kpad],0]];
contract[t_Times,k_^q_]:=With[{tpad=listPowerRemove[List@@t]},If[Length[tpad]==q,Product[contract[tpad[[i]],k],{i,1,Length[tpad]}],0]];
contract[t_^p_,k_Times]:=With[{kpad = listPowerRemove[List @@k]},If[p==Length[kpad],(p!/factorialProd[List@@k])*Product[contract[t,kpad[[i]]],{i,1,Length[kpad]}],0]];
contract[t_^p_,k_^q_]:=If[p==q,contract[t,k]^p,0];



(* ::Subsection:: *)
(*Conversions*)


(* TODO: add options to display nicely once Tree API stabilizes *)
toTree[\[FormalY] | Subscript[\[FormalY], _]] := Null;
toTree[t:(_Symbol | _Subscript)] := Tree[t, None];
toTree[Power[t_, p_]] := Splice[ConstantArray[toTree[t], p]];
toTree[t_Times] := Splice[Map[toTree, List @@ t]];
toTree[r:h_[t_]] := toTree[r] = Tree[h, {toTree[t]}];

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
			{Subscript[\[FormalF], n_], _} :> ColorData[3, n],
			{Subscript[\[FormalG], _], _} -> White,
			Black
		},
		VertexLabels -> {
			{Subscript[\[FormalF], n_], _} :> n,
			{Subscript[\[FormalG], _], _} -> "A"
		},
		ImageSize -> Tiny
	]
];

toBoxes[\[FormalY], format_] := ToBoxes["\[EmptySet]", format];
toBoxes[Subscript[\[FormalY], i_], format_] := ToBoxes[Subscript["\[EmptySet]", i], format];
If [
	TrueQ[$VersionNumber >= 12.3],
	toBoxes[t_, format_] := ToBoxes[toGraph[t], format];,
	toBoxes[t_, format_] := ToBoxes[t, format];
];


(* ::Section:: *)
(*Package Definitions*)


BTree[p:_Integer?Positive | {_Integer?NonNegative}] := treeMap[BTree, tree, p];
BTree /: Tree[HoldPattern[BTree[t_]]] := toTree[t];
BTree /: MakeBoxes[HoldPattern[BTree[t_]], format_] := toBoxes[t, format];

Options[BTreeN] = {"Partitions" -> 2};
BTreeN[p:_Integer?Positive | {_Integer?NonNegative}, OptionsPattern[]] := With[{
		n = OptionValue[Partitions]
	},
	treeMap[BTreeN[#, n] &, treeN[n], p] /; IntegerQ[n] && Positive[n]
];
BTreeN /: Tree[HoldPattern[BTreeN[t_, _Integer]]] := toTree[t];
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
BTreeDAE /: MakeBoxes[HoldPattern[BTreeDAE[t_, _Integer]], format_] := toBoxes[t, format];

(*Linearization Properties*)
BTree[t_ + s_]:= BTree[t] + BTree[s];
BTree[t_*s_/;NumberQ[t]]:=t*BTree[s];
BTree[t_*s_]:=BTree[t]*BTree[s];
BTree[t_^q_]:=BTree[t]^q;

BTreeN[t_ + s_,p__]:= BTreeN[t,p] + BTreeN[s,p];
BTree[t_*s_/;NumberQ[t],p__]:=t*BTreeN[s,p];
BTreeN[t_*s_,p__]:=BTreeN[t,p]*BTreeN[s,p];
BTreeN[t_^q_/;NumberQ[q],p__]:=BTreeN[t,p]^q;

BTreeDAE[t_ + s_,p__]:= BTreeDAE[t,p] + BTreeDAE[s,p];
BTreeDAE[t_*s_/;NumberQ[t],p__]:=t*BTreeDAE[s,p];
BTreeDAE[t_*s_,p__]:=BTreeDAE[t,p]*BTreeDAE[s,p];
BTreeDAE[t_^q_/;NumberQ[q],p__]:=BTreeDAE[t,p]^q;

BTreeOrder[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeOrder[t];
SetAttributes[BTreeOrder, Listable];

BTreeAlpha[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeAlpha[t];
SetAttributes[BTreeAlpha, Listable];

BTreeGamma[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeGamma[t];
SetAttributes[BTreeGamma, Listable];

BTreeSigma[(BTree | BTreeN | BTreeDAE)[t_, ___]] := treeSigma[t];
SetAttributes[BTreeSigma, Listable];

BTreePrune[h_[t_,p__],h_[t_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=h[\[FormalY],p];
BTreePrune[h_[t_,p__],h_[k_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=h[Expand[prune[t,k]],p];

BTreeContract[h_[t_,p__],h_[k_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=h[Expand[contract[t,k]],p];

BTreeSubTrees[h_[t_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=Map[h[#,p]&,Complement[DeleteDuplicates[subTrees[t]], {t}]];

BTreeRoot[h_[t_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=h[getRoot[t],p];

BTreeChildren[h_[t_,p__]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=h[getChildren[t],p];

BTreeFormalForm[h_[t_,___]]/;(AnyTrue[{BTree,BTreeN,BTreeDAE},h===#&]):=t;


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
