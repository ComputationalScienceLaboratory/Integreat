(* ::Package:: *)

BeginPackage["Integreat`Internal`Catalog`"];


AddCatalog


Begin["`Private`"];


AddCatalog[type_Symbol, c__List] := (
	type[] := Evaluate[Dataset[Map[<|"Names" -> Most[#], "Method" -> Last[#]|> &, List[c]]]];
	type[name_String] := Normal[type[][SelectFirst[StringMatchQ[name, #Names, IgnoreCase -> True] &], "Method"]];
);


End[];


EndPackage[];
