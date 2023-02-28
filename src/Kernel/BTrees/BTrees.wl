(* ::Package:: *)

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
BTreeFormalForm::usage = "Get the Formal Representation of the Tree";
BTreeQ::usage = "Check if the argument is a BTree"


BTreeOrder::usage = "BTreeOrder[t] computes the order of the tree t.";
BTreeAlpha::usage = "BTreeAlpha[t] computes the number of monotonic labelings of the tree t.";
BTreeGamma::usage = "BTreeGamma[t] computes the density of the tree t.";
BTreeSigma::usage = "BTreeSigma[t] computes the number of symmetries of the tree t.";


BTreePrune::usage = "Get all prunings of tree T with tree S and get its Forest Space";
BTreeContract::usage = "Get all cuts of tree T which contract to tree S and get its Forest Space";
BTreeSubTrees::usage = "Get All Subtrees of a Tree as a List";
BTreeRoot::usage = "Get the root node of a tree";
BTreeChildren::usage = "Get children of a tree and return as a Forest Space";


Begin["`Private`"];

<<Integreat`BTrees`Core`;
<<Integreat`BTrees`IntegerProperties`;
<<Integreat`BTrees`Subtrees`;

End[];


EndPackage[];
