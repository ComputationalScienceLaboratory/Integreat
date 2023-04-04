(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


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
toBoxes[t_, format_] := ToBoxes[toGraph[t], format];


(* ::Section:: *)
(*Package Definitions*)


BTree[p:_Integer?Positive | {_Integer?NonNegative}] := treeMap[BTree, tree, p];
BTree /: MakeBoxes[HoldPattern[r:BTree[t_]], format_] := With[{b = toBoxes[t, format]}, InterpretationBox[b, r]];


Options[BTreeN] = {"Partitions" -> 2};
BTreeN[p:_Integer?Positive | {_Integer?NonNegative}, OptionsPattern[]] := With[{
		n = OptionValue[Partitions]
	},
	treeMap[BTreeN[#, n] &, treeN[n], p] /; IntegerQ[n] && Positive[n]
];
BTreeN /: MakeBoxes[HoldPattern[r:BTreeN[t_, _Integer]], format_] := With[{b = toBoxes[t, format]}, InterpretationBox[b, r]];


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
BTreeDAE /: MakeBoxes[HoldPattern[r:BTreeDAE[t_, _Integer]], format_] := With[{b = toBoxes[t, format]}, InterpretationBox[b, r]];


BTreeQ[BTree[_] | BTreeN[_, _] | BTreeDAE[_, _]] := True;
BTreeQ[BTree | BTreeN | BTreeDAE] := True;
BTreeQ[_] := False;


BTreeFormalForm[h_[t_,___]]/;BTreeQ[h]:=t;
BTreeFormalForm[h_]/;NumericQ[h]:=h;
SetAttributes[BTreeFormalForm, Listable];
