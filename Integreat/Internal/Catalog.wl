(* ::Package:: *)

BeginPackage["Integreat`Internal`Catalog`"];


Integreat`Internal`Catalog::usage = "Package containing functions for using method catalogs";
AddCatalog::usage = "Adds catalog and search functionality to a symbol";
AddCatalogSearch::usage = "";
CatalogEntry::usage = "";


Begin["`Private`"];


AddCatalog[type_Symbol, c__List] := (
	type[] := Evaluate[Dataset[Map[<|"Names" -> Most[#], "Method" -> Last[#]|> &, List[c]]]];
	type[name_String] := Normal[type[][SelectFirst[StringMatchQ[name, #Names, IgnoreCase -> True] &], "Method"]];
);

AddCatalogSearch[type_Symbol] := (
	type[name_String] := type[][SelectFirst[StringMatchQ[name, #Names, IgnoreCase -> True] &], "Method"];
);

CatalogEntry[names:{__String}, method_, notes_String, title_String, authors:{__String}, year_Integer, url_String, extra___Rule] := <|
	"Names" -> names,
	"Method" -> method,
	"Source" -> <|
		"Title" -> title,
		"Authors" -> authors,
		"Year" -> year,
		"URL" -> url,
		extra
	|>,
	"Notes" -> notes
|>;

CatalogSearch[type_, name_] := type[][SelectFirst[StringMatchQ[name, #Names, IgnoreCase -> True] &], "Method"]


End[];
EndPackage[];
