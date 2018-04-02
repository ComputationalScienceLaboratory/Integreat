(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Internal`Catalog`"];


CSL`OdeUtils`internal`Catalog::usage = "Package containing functions for using method catalogs";

Catalog::usage = "Constructs a dataset of methods and their names";
CatalogSearch::usage = "Searches a catalog for a method closely matching a name";


Begin["`Private`"];


Catalog[c:{(<|"Names"->{__},"Method"->_|>)...}] := Dataset[c];

CatalogSearch::arg = "The search must be a string";
CatalogSearch[catalog_, search_?StringQ] := With[{
	pattern = StringMatchQ[search, IgnoreCase -> True]
},
	Normal[catalog[SelectFirst[MemberQ[pattern[#Names], True] &], "Method"]]
];
CatalogSearch[___] := (Message[CatalogSearch::arg]; $Failed);


End[];


EndPackage[];
