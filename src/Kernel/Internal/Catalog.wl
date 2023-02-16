(* ::Package:: *)

BeginPackage["Integreat`Internal`Catalog`"];


Integreat`Internal`Catalog::usage = "Package containing functions for using method catalogs";
AddCatalog::usage = "Adds catalog and search functionality to a symbol";


Begin["`Private`"];


AddCatalog[type_Symbol, c__List] := (
	type[] := Evaluate[Dataset[Map[<|"Names" -> Most[#], "Method" -> Last[#]|> &, List[c]]]];
	type[name_String] := Normal[type[][SelectFirst[StringMatchQ[name, #Names, IgnoreCase -> True] &], "Method"]];
);


End[];
EndPackage[];