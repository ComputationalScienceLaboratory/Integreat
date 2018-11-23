(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GARK`Methods`"];


CSL`OdeUtils`MrGARK`Methods::usage = "Package containing functions for creating generalized additive Runge-Kutta methods";

MrGarkQ::usage = "Returns True if input is a valid Runge-Kutta method, and False otherwise";
MrGarkPairQ::usage = "";
MrGark::usage = "Constructs an association containing multirate generalized additive Runge-Kutta coefficients";
MrGarkEmbedded::usage = "";
MrGarkFastStages::usage = "Returns the number of stages in a Runge-Kutta method";
MrGarkSlowStages::usage = "Returns the number of stages in a Runge-Kutta method";
MrGarkFullAff::usage = "";
MrGarkFullAfs::usage = "";
MrGarkFullAsf::usage = "";
MrGarkFullAss::usage = "";
MrGarkFullA::usage = "";
MrGarkFullBf::usage = "";
MrGarkFullBs::usage = "";
MrGarkFullB::usage = "";
MrGarkCf::usage = "";
MrGarkTableau::usage = "Creates a tableau of Runge-Kutta coefficients";
MrGarkFsalQ::usage = "Returns True if Runge-Kutta method have First Same As Last property, and False otherwise";
MrGarkTelescopicQ::usage = "Returns True if Runge-Kutta method have First Same As Last property, and False otherwise";
MrGarkCatalog::usage = "A dataset of Runge-Kutta methods";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];

Tableau[Aff_, Afs_, Asf_, Ass_, bf_, bs_] := With[{
	d1 = {Length[Aff] + 1 -> True},
	d2 = Thread[Length[Aff] + Length[Asf] + Range[Length[bf]] -> True]
},
	Grid[
		ArrayFlatten[{{Aff, Afs}, {Asf, Ass}, {bf, bs}}],
		Dividers -> {d1, Join[d1, d2]}
	]
];

catalog = Catalog[{}];


MrGarkQ[x_] := AssociationQ[x];

MrGarkPairQ[x_] := MrGarkQ[x] && RungeKuttaPairQ[x["f"]] && RungeKuttaPairQ[x["s"]];

MrGark[f_?RungeKuttaQ, s_?RungeKuttaQ, fs_List?MatrixQ, sf_List?MatrixQ] := With[{
	stagesF = RungeKuttaStages[f],
	stagesS = RungeKuttaStages[s]
},
	If[Dimensions[fs] =!= {stagesF, stagesS}, Message[MrGark::dims, "b"]; Return[$Failed]];
	If[Dimensions[sf] =!= {stagesS, stagesF}, Message[RungeKutta::dims, "c"]; Return[$Failed]];
	<|"f" -> f, "s" -> s, "fs" -> fs, "sf" -> sf|>
];
MrGark[b_?RungeKuttaQ, fs_List?MatrixQ, sf_List?MatrixQ] := MrGark[b, b, fs, sf];

MrGarkEmbedded[method_?MrGarkPairQ] := MrGark[
	RungeKuttaEmbedded[method["f"]],
	RungeKuttaEmbedded[method["s"]],
	method["fs"],
	method["sf"]
];

MrGarkFastStages[method_?MrGarkQ] := RungeKuttaStages[method["f"]];

MrGarkSlowStages[method_?MrGarkQ] := RungeKuttaStages[method["s"]];

MrGarkFullAff[method_?MrGarkQ, M_Integer] := With[{
	f = method["f"]
},
	(KroneckerProduct[LowerTriangularize[ConstantArray[1, {M, M}], -1], ConstantArray[f[\[FormalB]], RungeKuttaStages[f]]] + KroneckerProduct[IdentityMatrix[M], f[\[FormalCapitalA]]]) / M
];

MrGarkFullAfs[method_?MrGarkQ, M_Integer] := Flatten[Table[method["fs"] /. {\[FormalLambda] -> \[Lambda], \[FormalCapitalM] -> M}, {\[Lambda], M}], 1];

MrGarkFullAsf[method_?MrGarkQ, M_Integer] := Transpose[Flatten[Table[Transpose[method["sf"]] /. {\[FormalLambda] -> \[Lambda], \[FormalCapitalM] -> M}, {\[Lambda], M}], 1]] / M;

MrGarkFullAss[method_?MrGarkQ] := method["s"][\[FormalCapitalA]];

MrGarkFullA[method_?MrGarkQ, M_Integer] := ArrayFlatten[{{MrGarkFullAff[method, M], MrGarkFullAfs[method, M]}, {MrGarkFullAsf[method, M], MrGarkFullAss[method]}}];

MrGarkFullBf[method_?MrGarkQ, M_Integer] := Flatten[ConstantArray[method["f"][\[FormalB]], M]] / M;

MrGarkFullBs[method_?MrGarkQ] := method["s"][\[FormalB]];

MrGarkFullB[method_?MrGarkQ, M_Integer] := Join[MrGarkFullBf[method, M], MrGarkFullBs[method]];

MrGarkCf[method_?MrGarkQ, lambda_, M_] := (method["f"][\[FormalC]] + lambda - 1) / M;

MrGarkTableau[method_?MrGarkPairQ] := With[{e = MrGarkEmbedded[method]},
	Tableau[method["f"][\[FormalCapitalA]], method["fs"], method["sf"], method["s"][\[FormalCapitalA]], {method["f"][\[FormalB]], e["f"][\[FormalB]]}, {method["s"][\[FormalB]], e["s"][\[FormalB]]}]
];
MrGarkTableau[method_?MrGarkQ] := Tableau[method["f"][\[FormalCapitalA]], method["fs"], method["sf"], method["s"][\[FormalCapitalA]], {method["f"][\[FormalB]]}, {method["s"][\[FormalB]]}];
MrGarkTableau[method_?MrGarkPairQ, M_Integer] := With[{e = MrGarkEmbedded[method]},
	Tableau[MrGarkFullAff[method, M], MrGarkFullAfs[method, M], MrGarkFullAsf[method, M], MrGarkFullAss[method], {MrGarkFullBf[method, M], MrGarkFullBf[e, M]}, {MrGarkFullBs[method], MrGarkFullBs[e]}]
];
MrGarkTableau[method_?MrGarkQ, M_Integer] := Tableau[MrGarkFullAff[method, M], MrGarkFullAfs[method, M], MrGarkFullAsf[method, M], MrGarkFullAss[method], {MrGarkFullBf[method, M]}, {MrGarkFullBs[method]}];

RungeKuttaFsalQ[method_?RungeKuttaQ] := And @@ Thread[First[method[\[FormalCapitalA]]] == 0] && And @@ Thread[Last[method[\[FormalCapitalA]]] == method[\[FormalB]]];
	
MrGarkCatalog[search_] := CatalogSearch[catalog, search];
MrGarkCatalog[] := catalog


End[];


EndPackage[];
