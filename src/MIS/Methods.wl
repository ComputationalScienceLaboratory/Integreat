(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Mis`Methods`"];


CSL`OdeUtils`Mis`Methods::usage = "?";

RungeKuttaQ::usage = "Returns True if input is a valid Runge-Kutta method, and False otherwise";
RungeKuttaPairQ::usage = "Returns True if input is a valid Runge-Kutta pair method, and False otherwise";
RungeKutta::usage = "Constructs an association containing Runge-Kutta coefficients";
RungeKuttaCollocation::usage = "Constructs a Runge-Kutta collocation method";
RungeKuttaPair::usage = "Constructs a Runge-Kutta method with an embedded method";
RungeKuttaEmbedded::usage = "Extracts the embedded method from a pair";
RungeKuttaStages::usage = "Returns the number of stages in a Runge-Kutta method";
RungeKuttaTableau::usage = "Creates a tableau of Runge-Kutta coefficients";
RungeKuttaFsalQ::usage = "Returns True if Runge-Kutta method have First Same As Last property, and False otherwise";
RungeKuttaCatalog::usage = "A dataset of Runge-Kutta methods";


Begin["`Private`"];


MisQ[x_] := True;

Mis[gammas_, betas_] := <|\[FormalGamma] -> gammas, \[FormalBeta] -> betas|>;

MisDegree[method_?MisQ] := Length[method[\[FormalGamma]]];

MisStages[method_?MisQ] := Length[First[method[\[FormalGamma]]]];

MisGammaBar[method_?MisQ] := Sum[method[\[FormalGamma]][[k]] / k, {k, MisDegree[method] + 1}];

MisBetaBar[method_?MisQ] := Sum[method[\[FormalBeta]][[k]] / k, {k, MisDegree[method] + 1}];

MisRungeKutta[method_?MisQ] := With[{
	s = MisStages[method],
	GBar = MisGammaBar[method]
},
	RungeKutta[
		LowerTriangularize[ConstantArray[1, {s, s}]] . GBar,
		Total[G
	]
];

RungeKuttaTableau[method_?RungeKuttaPairQ] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]], method[\[FormalD]]}, method[\[FormalC]]];
RungeKuttaTableau[method_?RungeKuttaQ] := Tableau[RungeKuttaStages[method], method[\[FormalCapitalA]], {method[\[FormalB]]}, method[\[FormalC]]];

RungeKuttaFsalQ[method_?RungeKuttaQ] := And @@ Thread[First[method[\[FormalCapitalA]]] == 0] && And @@ Thread[Last[method[\[FormalCapitalA]]] == method[\[FormalB]]];

RungeKuttaCatalog[search_] := CatalogSearch[catalog, search];
RungeKuttaCatalog[] := catalog


End[];


EndPackage[];
