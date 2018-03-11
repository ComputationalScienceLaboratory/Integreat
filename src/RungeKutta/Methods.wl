(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`Methods`"];


CSL`OdeUtils`RungeKutta`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RungeKuttaQ::usage = "Returns True if input is a valid Runge-Kutta method, and False otherwise";
RungeKuttaPairQ::usage = "Returns True if input is a valid Runge-Kutta pair method, and False otherwise";
RungeKutta::usage = "Constructs an association containing Runge-Kutta coefficients";
RungeKuttaPair::usage = "Constructs and Runge-Kutta method with an embedded method";
RungeKuttaEmbedded::usage = "Extracts the embedded method from a pair";
RungeKuttaStages::usage = "Returns the number of stages in a Runge-Kutta method";
RungeKuttaTableau::usage = "Creates a tableau of Runge-Kutta coefficients";
RungeKuttaFsalQ::usage = "Returns True if Runge-Kutta method have First Same As Last property, and False otherwise";
RungeKuttaCatalog::usage = "A dataset of Runge-Kutta methods";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];

PrimaryQ[x_, s_] := AssociationQ[x] && KeyExistsQ[x, \[FormalCapitalA]] && KeyExistsQ[x, \[FormalB]] && KeyExistsQ[x, \[FormalC]] && SquareMatrixQ[x[\[FormalCapitalA]]] && Dimensions[x[\[FormalB]]] === {s} && Dimensions[x[\[FormalC]]] === {s};
	
EmbeddedQ[x_, s_] := Dimensions[x[\[FormalD]]] === {s};

dimsErr = "`1` must have length equal to the size of A";

Tableau[s_, A_, b_, c_] := With[{
	cCol = Transpose[{c}],
	nulls = ConstantArray[Null, {Length[b], 1}],
	vertDivs = {2 -> True},
	horizDivs = Thread[s + Range[Length[b]] -> True]
},
	Grid[ArrayFlatten[{{cCol, A}, {nulls, b}}], Dividers -> {vertDivs, horizDivs}]
];

catalog = Dataset[{
	(*Explicit*)
	<|"Names" -> {"Euler", "Euler's Method", "Forward Euler", "Explicit Euler"}, "Method" -> RungeKutta[{{0}}, {1}]|>,
	<|"Names" -> {"Heun", "Heun's Method", "Explicit Trapezoid"}, "Method" -> RungeKuttaPair[{{0,0},{1,0}}, {1/2,1/2}, {1,0}]|>,
	<|"Names" -> {"Bogacki-Shampine", "ode23"}, "Method" -> RungeKuttaPair[{{0,0,0,0},{1/2,0,0,0},{0,3/4,0,0},{2/9,1/3,4/9,0}}, {2/9, 1/3, 4/9, 0}, {7/24,1/4,1/3,1/8}]|>,
	<|"Names" -> {"RK4", "Classiscal", "Classical Runge-Kutta Method", "The Runge-Kutta Method"}, "Method" -> RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,1/2,0,0},{0,0,1,0}}, {1/6,1/3,1/3,1/6}]|>,
	(*Implicit*)
	<|"Names" -> {"Backward Euler", "Implicit Euler"}, "Method" -> RungeKutta[{{1}}, {1}]|>,
	<|"Names" -> {"Implicit Midpoint"}, "Method" -> RungeKutta[{{1/2}}, {1}]|>
}];


RungeKuttaQ[x_] := AssociationQ[x] && KeyExistsQ[x, \[FormalCapitalA]] && KeyExistsQ[x, \[FormalB]] && KeyExistsQ[x, \[FormalC]] && SquareMatrixQ[x[\[FormalCapitalA]]] &&
	With[{s = Length[x[\[FormalCapitalA]]]},
		Dimensions[x[\[FormalB]]] == {s} && Dimensions[x[\[FormalC]]] == {s}
		&& If[KeyExistsQ[x, \[FormalD]], Dimensions[x[\[FormalD]]] == {s}, True]
	];

RungeKuttaPairQ[x_] := AssociationQ[x] && KeyExistsQ[x, \[FormalCapitalA]] && KeyExistsQ[x, \[FormalB]] && KeyExistsQ[x, \[FormalC]] && KeyExistsQ[x, \[FormalD]] && SquareMatrixQ[x[\[FormalCapitalA]]] &&
	With[{s = Length[x[\[FormalCapitalA]]]},
		Dimensions[x[\[FormalB]]] == {s} && Dimensions[x[\[FormalC]]] == {s} && Dimensions[x[\[FormalD]]] == {s}
	];

RungeKutta::dims = dimsErr;
RungeKutta[A_List/;SquareMatrixQ[A], b_List, c_List] := With[{
	s = Length[A],
	bDims = Dimensions[b],
	cDims = Dimensions[c]
},
	If[bDims != {s, 1} && bDims != {s}, Message[RungeKutta::dims, "b"]; Return[$Failed]];
	If[cDims != {s, 1} && cDims != {s}, Message[RungeKutta::dims, "c"]; Return[$Failed]];
	<|\[FormalCapitalA] -> A, \[FormalB] -> Flatten[b], \[FormalC] -> Flatten[c]|>
];
RungeKutta[A_List/;SquareMatrixQ[A], b_List] := RungeKutta[A, b, Total[A, {2}]];
RungeKutta[A_List/;SquareMatrixQ[A]] := RungeKutta[A, Table[Subscript[\[FormalB], i], {i, Length[A]}]];
RungeKutta[s_Integer] := RungeKutta[TableauFIRK[s], Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];

RungeKuttaPair::dims = dimsErr;
RungeKuttaPair[method_/;RungeKuttaQ[method], d_List] := With[{
	s = RungeKuttaStages[method],
	dDims = Dimensions[d]
},
	If[dDims != {s, 1} && dDims != {s}, Message[RungeKuttaPair::dims, "d"]; Return[$Failed]];
	Append[method, \[FormalD] -> Flatten[d]]
];
RungeKuttaPair[A_List/;SquareMatrixQ[A], b_List, c_List, d_List] := RungeKuttaPair[RungeKutta[A, b, c], d];
RungeKuttaPair[A_List/;SquareMatrixQ[A], b_List, d_List] := RungeKuttaPair[RungeKutta[A, b], d];

RungeKuttaEmbedded[method_/;RungeKuttaPairQ[method]] := RungeKutta[method[\[FormalCapitalA]], method[\[FormalD]], method[\[FormalC]]];

RungeKuttaStages[method_/;RungeKuttaQ[method]] := Length[method[\[FormalCapitalA]]];

RungeKuttaTableau[method_/;RungeKuttaPairQ[method]] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]], method[\[FormalD]]}, method[\[FormalC]]];
RungeKuttaTableau[method_/;RungeKuttaQ[method]] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]]}, method[\[FormalC]]];

RungeKuttaFsalQ[method_/;RungeKuttaQ[method]] := And @@ Thread[First[method[\[FormalCapitalA]]] == 0] && And @@ Thread[Last[method[\[FormalCapitalA]]] == method[\[FormalB]]];
	
RungeKuttaCatalog[search_/;StringQ[search]] := With[{
	pattern = StringMatchQ[search, IgnoreCase -> True, SpellingCorrection -> True]
},
	Normal[catalog[SelectFirst[MemberQ[pattern[#Names], True] &], "Method"]]
];
RungeKuttaCatalog[] := catalog


End[];


EndPackage[];
