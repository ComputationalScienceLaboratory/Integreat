(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`Methods`"];


CSL`OdeUtils`RungeKutta`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RungeKuttaQ::usage = "Returns True if input is a valid Runge-Kutta method, and False otherwise";
RungeKuttaPairQ::usage = "Returns True if input is a valid Runge-Kutta pair method, and False otherwise";
RungeKutta::usage = "Constructs an association containing Runge-Kutta coefficients";
RungeKuttaPair::usage = "Constructs a Runge-Kutta method with an embedded method";
RungeKuttaEmbedded::usage = "Extracts the embedded method from a pair";
RungeKuttaStages::usage = "Returns the number of stages in a Runge-Kutta method";
RungeKuttaTableau::usage = "Creates a tableau of Runge-Kutta coefficients";
RungeKuttaFsalQ::usage = "Returns True if Runge-Kutta method have First Same As Last property, and False otherwise";
RungeKuttaCatalog::usage = "A dataset of Runge-Kutta methods";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];

dimsErr = "`1` must have length equal to the size of A";

Tableau[s_, A_, b_, c_] := With[{
	cCol = Transpose[{c}],
	vertDivs = {2 -> True},
	horizDivs = Thread[s + Range[Length[b]] -> True]
},
	Grid[ArrayFlatten[{{cCol, A}, {Null, b}}], Dividers -> {vertDivs, horizDivs}]
];

catalog = Catalog[{
	(*Explicit*)
	<|"Names" -> {"Euler", "Euler's Method", "Forward Euler", "Explicit Euler"}, "Method" -> RungeKutta[{{0}}, {1}]|>,
	<|"Names" -> {"Heun", "Heun's Method", "Explicit Trapezoid"}, "Method" -> RungeKuttaPair[{{0,0},{1,0}}, {1/2,1/2}, {1,0}]|>,
	<|"Names" -> {"Ralston's 2nd Order Method", "Ralston 2"}, "Method" -> RungeKuttaPair[{{0,0},{2/3,0}}, {1/4,3/4}, {1,0}]|>,
	<|"Names" -> {"Ralston's 3rd Order Method", "Ralston 3"}, "Method" -> RungeKuttaPair[{{0,0,0},{1/2,0,0},{0,3/4,0}}, {2/9,1/3,4/9}, {1/40,37/40,1/20}]|>,
	<|"Names" -> {"Bogacki-Shampine", "ode23"}, "Method" -> RungeKuttaPair[{{0,0,0,0},{1/2,0,0,0},{0,3/4,0,0},{2/9,1/3,4/9,0}}, {2/9, 1/3, 4/9, 0}, {7/24,1/4,1/3,1/8}]|>,
	<|"Names" -> {"EX 4(3)"}, "Method" -> RungeKuttaPair[
		{{0,0,0,0,0},{2/5,0,0,0,0},{-3/20,3/4,0,0,0},{19/44,-15/44,10/11,0,0},{11/72,25/72,25/72,11/72,0}},
		{11/72,25/72,25/72,11/72,0}, {1251515/8970912,3710105/8970912,2519695/8970912,61105/8970912,119041/747576}
	]|>,
	<|"Names" -> {"Fehlberg's 4th Order Method", "Felhberg 4"}, "Method" -> RungeKuttaPair[
		{{0,0,0,0,0,0},{1/4,0,0,0,0,0},{3/32,9/32,0,0,0,0},{1932/2197,-7200/2197,7296/2197,0,0,0},{439/216,-8,3680/513,-845/4104,0,0},{-(8/27),2,-3544/2565,1859/4104,-11/40,0}},
		{25/216,0,1408/2565,2197/4104,-1/5,0}, {16/135,0,6656/12825,28561/56430,-9/50,2/55}
	]|>,
	<|"Names" -> {"RK4", "Classiscal", "Classical Runge-Kutta Method", "The Runge-Kutta Method"}, "Method" -> RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,1/2,0,0},{0,0,1,0}}, {1/6,1/3,1/3,1/6}]|>,
	(*Implicit*)
	<|"Names" -> {"Backward Euler", "Implicit Euler"}, "Method" -> RungeKutta[{{1}}, {1}]|>,
	<|"Names" -> {"Implicit Midpoint"}, "Method" -> RungeKutta[{{1/2}}, {1}]|>,
	<|"Names" -> {"SDIRK 2(1)"}, "Method" -> RungeKuttaPair[{{1-1/Sqrt[2],0},{1/Sqrt[2],1-1/Sqrt[2]}}, {1/Sqrt[2],1-1/Sqrt[2]}, {3/5,2/5}]|>,
	<|"Names" -> {"SDIRK 3(2)"}, "Method" -> RungeKuttaPair[
		{{1+Root[-4-9 #1+6 #1^3&,2], 0, 0},{Root[2-9 #1+24 #1^3&,2], 1+Root[-4-9 #1+6 #1^3&,2], 0},{Root[-7+36 #1-54 #1^2+24 #1^3&,3],Root[-8+27 #1^2+12 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2]}},
		{Root[-7+36 #1-54 #1^2+24 #1^3&,3],Root[-8+27 #1^2+12 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2]}, {Root[1-6 #1+3 #1^2+4 #1^3&,3],Root[-2+12 #1-15 #1^2+4 #1^3&,1],0}
	]|>,
	<|"Names" -> {"SDIRK 4(3)"}, "Method" -> RungeKuttaPair[
		{{1/4,0,0,0,0},{13/20,1/4,0,0,0},{580/1287,-175/5148,1/4,0,0},{12698/37375,-201/2990,891/11500,1/4,0},{944/1365,-400/819,99/35,-575/252,1/4}},
		{944/1365,-400/819,99/35,-575/252,1/4}, {41911/60060,-83975/144144,3393/1120,-27025/11088,103/352}
	]|>
}];


RungeKuttaQ[x_] := AssociationQ[x] && SquareMatrixQ[x[\[FormalCapitalA]]] && VectorQ[x[\[FormalB]]] && VectorQ[x[\[FormalC]]] &&
	With[{s = Length[x[\[FormalCapitalA]]]},
		Length[x[\[FormalB]]] === s && Length[x[\[FormalC]]] === s
		&& If[KeyExistsQ[x, \[FormalD]], VectorQ[x[\[FormalD]]] && Length[x[\[FormalD]]] === s, True]
	];

RungeKuttaPairQ[x_] := AssociationQ[x] && SquareMatrixQ[x[\[FormalCapitalA]]] && VectorQ[x[\[FormalB]]] && VectorQ[x[\[FormalC]]] && VectorQ[x[\[FormalD]]] &&
	With[{s = Length[x[\[FormalCapitalA]]]},
		Length[x[\[FormalB]]] === s && Length[x[\[FormalC]]] === s && Length[x[\[FormalD]]] === s
	];

RungeKutta::dims = dimsErr;
RungeKutta[A_List?SquareMatrixQ, b_List?VectorQ, c_List?VectorQ] := With[{
	s = Length[A]
},
	If[Length[b] =!= s, Message[RungeKutta::dims, "b"]; Return[$Failed]];
	If[Length[c] =!= s, Message[RungeKutta::dims, "c"]; Return[$Failed]];
	<|\[FormalCapitalA] -> A, \[FormalB] -> b, \[FormalC] -> c|>
];
RungeKutta[A_List?SquareMatrixQ, b_List?VectorQ] := RungeKutta[A, b, Total[A, {2}]];
RungeKutta[A_List?SquareMatrixQ] := RungeKutta[A, Table[Subscript[\[FormalB], i], {i, Length[A]}]];
RungeKutta[s_Integer] := RungeKutta[TableauFirk[s], Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];

RungeKuttaPair::dims = dimsErr;
RungeKuttaPair[method_?RungeKuttaQ, d_List?VectorQ] := (
	If[Length[d] =!= RungeKuttaStages[method], Message[RungeKuttaPair::dims, "d"]; Return[$Failed]];
	Append[method, \[FormalD] -> d]
);

RungeKuttaPair[A_List?SquareMatrixQ, b_List?VectorQ, c_List?VectorQ, d_List?VectorQ] := RungeKuttaPair[RungeKutta[A, b, c], d];
RungeKuttaPair[A_List?SquareMatrixQ, b_List?VectorQ, d_List?VectorQ] := RungeKuttaPair[RungeKutta[A, b], d];

RungeKuttaEmbedded[method_?RungeKuttaPairQ] := RungeKutta[method[\[FormalCapitalA]], method[\[FormalD]], method[\[FormalC]]];

RungeKuttaStages[method_?RungeKuttaQ] := Length[method[\[FormalCapitalA]]];

RungeKuttaTableau[method_?RungeKuttaPairQ] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]], method[\[FormalD]]}, method[\[FormalC]]];
RungeKuttaTableau[method_?RungeKuttaQ] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]]}, method[\[FormalC]]];

RungeKuttaFsalQ[method_?RungeKuttaQ] := And @@ Thread[First[method[\[FormalCapitalA]]] == 0] && And @@ Thread[Last[method[\[FormalCapitalA]]] == method[\[FormalB]]];

RungeKuttaCatalog[search_] := CatalogSearch[catalog, search];
RungeKuttaCatalog[] := catalog


End[];


EndPackage[];
