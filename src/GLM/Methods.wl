(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GLM`Methods`"];


CSL`OdeUtils`GLM`Methods::usage = "Package containing functions for creating general linear methods";

GlmQ::usage = "Returns True if input is a valid general linear method, and False otherwise";
Glm::usage = "Constructs an association containing general linear method coefficients";
DimsimQ::usage = "Returns True if input is a valid diagonally implicit multistage integration method, and False otherwise";
Dimsim::usage = "Constructs an association containing diagonally implicit multistage integration method coefficients";
GlmType::usage = "Returns the type number of the general linear method";
GlmInternalStages::usage = "Returns the number of internal stages in a general linear method";
GlmExternalStages::usage = "Returns the number of external stages in a general linear method";
GlmTableau::usage = "Creates a tableau of general linear method coefficients";
GlmCatalog::usage = "A dataset of general linear methods";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];

glmOptions = OptionsPattern[{"Type" -> -1}];

TableauW[r_, p_] := Table[Subscript[\[FormalW], i, j], {i, r}, {j, 0, p}];

TypeToTableau[type_] := Switch[type, 1, TableauExplicit, 2, TableauSdirk, 3, TableauZeros, 4, TableauDiagonal, _, TableauFirk];

catalog = Catalog[{
	(*<|"Names" -> {"DIMSIM3 Type 1"}, "Method" -> Dimsim[*)
}];


GlmQ[x_] := AssociationQ[x] && SquareMatrixQ[x[\[FormalCapitalA]]] && MatrixQ[x[\[FormalCapitalB]]] && MatrixQ[x[\[FormalCapitalU]]] && SquareMatrixQ[x[\[FormalCapitalV]]] && VectorQ[x[\[FormalC]]] && MatrixQ[x[\[FormalCapitalW]]] &&
	With[{
		s = Length[x[\[FormalCapitalA]]],
		r = Length[x[\[FormalCapitalV]]]
	},
		Dimensions[x[\[FormalCapitalB]]] === {r, s} && Dimensions[x[\[FormalCapitalU]]] === {s, r} && Length[x[\[FormalC]]] === s && Length[x[\[FormalCapitalW]]] === r
	];

Glm::dims = "`1` must have the same number of rows as `2` and the same number of columns as `3`";
Glm::cdims = "c must have length equal to the size of A";
Glm::wdims = "W must have the same number of rows as V";
Glm[A_List?SquareMatrixQ, B_List?MatrixQ, U_List?MatrixQ, V_List?SquareMatrixQ, c_List?VectorQ, W_List?MatrixQ] := With[{
	s = Length[A],
	r = Length[V]
},
	If[Dimensions[B] =!= {r, s}, Message[Glm::dims, "B", "A", "V"]; Return[$Failed]];
	If[Dimensions[U] =!= {s, r}, Message[Glm::dims, "U", "V", "A"]; Return[$Failed]];
	If[Length[c] =!= s, Message[Glm::cdims]; Return[$Failed]];
	If[Length[W] =!= r, Message[Glm::wdims]; Return[$Failed]];
	<|\[FormalCapitalA] -> A, \[FormalCapitalB] -> B, \[FormalCapitalU] -> U, \[FormalCapitalV] -> V, \[FormalC] -> c, \[FormalCapitalW] -> W|>
];
Glm[A_List?SquareMatrixQ, B_List?MatrixQ, U_List?MatrixQ, V_List?SquareMatrixQ, W_List?MatrixQ] := Glm[A, B, U, V, Total[A, {2}] + U.W[[All, 2]], W];
Glm[s_Integer, r_Integer, p_Integer, glmOptions] := Glm[TypeToTableau[OptionValue["Type"]][s], TableauFirk[{r, s}, \[FormalB]], TableauFirk[{s, r}, \[FormalU]], TableauFirk[r, \[FormalV]], Table[Subscript[\[FormalC], i], {i, s}], TableauW[r, p]];
Glm[method_?RungeKuttaQ, p_Integer] := Glm[method[\[FormalCapitalA]], {method[\[FormalB]]}, ConstantArray[1, {RungeKuttaStages[method], 1}], {{1}}, method[\[FormalC]], {Factorial[Range[0, p]]}];

DimsimQ[x_] := GlmQ[x] && With[{s = GlmInternalStages[x]},
	s === GlmExternalStages[x] && SdirkQ[x] && MatrixRank[x[\[FormalCapitalV]]] === 1 && And @@ Thread[x[\[FormalCapitalU]] === IdentityMatrix[s], {2}]
];

Dimsim::dims = "`1` must have the same size as A";
Dimsim[A_List/;SquareMatrixQ[A] && TableauSdirkQ[A], B_List?SquareMatrixQ, V_List/;SquareMatrixQ[V] && MatrixRank[V] === 1, c_List?VectorQ, W_List?MatrixQ] := With[{
	s = Length[A]
},
	If[Length[B] =!= s, Message[Dimsim::dims, "B"]; Return[$Failed]];
	If[Length[V] =!= s, Message[Dimsim::dims, "V"]; Return[$Failed]];
	Glm[A, B, IdentityMatrix[s], V, c, W]
];
Dimsim[A_List/;SquareMatrixQ[A] && TableauSdirkQ[A], B_List?SquareMatrixQ, v_List?VectorQ, c_List?VectorQ, W_List?MatrixQ] := Dimsim[A, B, ConstantArray[v, Length[A]], c, W];
Dimsim[s_Integer, glmOptions] := Dimsim[TypeToTableau[OptionValue["Type"]][s], TableauFirk[s, \[FormalB]], Table[Subscript[\[FormalV], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}], TableauW[s, s]];

GlmType[method_?GlmQ] := With[{A = method[\[FormalCapitalA]]},
	Which[
		TableauZerosQ[A], 3,
		TableauDiagonalQ[A], 4,
		TableauExplicitQ[A], 1,
		TableauSdirkQ[A], 2,
		True, Null
	]
];

GlmInternalStages[method_?GlmQ] := Length[method[\[FormalCapitalA]]];

GlmExternalStages[method_?GlmQ] := Length[method[\[FormalCapitalV]]];

GlmTableau[method_?GlmQ] := With[{
	s = GlmInternalStages[method],
	cCol = Transpose[{method[\[FormalC]]}]
},
	Grid[
		ArrayFlatten[{{cCol, method[\[FormalCapitalA]], method[\[FormalCapitalU]]}, {Null, method[\[FormalCapitalB]], method[\[FormalCapitalV]]}}],
		Dividers -> {{2 -> True, 2 + s -> True}, {1 + s -> True}}
	]
];
	
GlmCatalog[search_] := CatalogSearch[catalog, search];
GlmCatalog[] := catalog


End[];


EndPackage[];
