(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RungeKutta`Methods`"];
Integreat`RungeKutta`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RungeKutta::usage = "Constructs a Runge-Kutta method";
RungeKuttaType::usage = "A string representation of the type of Runge-Kutta method";
RungeKuttaPrimary::usage = "Removes the embedded method from a Runge-Kutta method";
RungeKuttaEmbedded::usage = "Gets the embedded Runge-Kutta method";
RungeKuttaPairQ::usage = "Returns True if m is a Runge-Kutta method with an embedded method";
RungeKuttaCollocation::usage = "Constructs a collocated Runge-Kutta method";
RungeKuttaCompose::usage = "Builds a single Runge-Kutta method from the composition of sub-methods";
RungeKuttaA::usage = "Gets the A coefficients of a Runge-Kutta method";
RungeKuttaDenseOutput::usage = "Gets the interpolatory b coeffients as a function of \[FormalTheta] for a Runge-Kutta method";
RungeKuttaB::usage = "Gets the b coefficients of a Runge-Kutta method.  Optionally the second argument is a boolean for whether to return to return the embedded b coefficients.";
RungeKuttaC::usage = "Gets the c coefficients of a Runge-Kutta method";
RungeKuttaBHat::usage = "Gets the embedded coefficients of a Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Composition`"
}];

LagrangeBasis[t_, c_, i_] := Product[(t - c[[l]]) / (c[[i]] - c[[l]]), {l, DeleteCases[Range[Length[c]], i]}];

RkCompose[m_] := RungeKutta[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RungeKuttaA[m[[i, 1]]],
		i > j, m[[j, 2]] * ConstantArray[RungeKuttaB[m[[j, 1]]], Length[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RungeKuttaDenseOutput[First[#]] &, m]],
	Catenate[Map[Last[#] * RungeKuttaC[First[#]] &, m] + FoldList[#1 + Last[#2] * Total[RungeKuttaB[First[#2]]] &, 0, Most[m]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RungeKuttaPairQ], Catenate[Map[Last[#] * RungeKuttaBHat[First[#]] &, m]], Unevaluated[Sequence[]]]
];


(* ::Section:: *)
(*Package Definitions*)


RungeKutta[s_Integer, OptionsPattern[{Type -> "FIRK"}]] := RungeKutta[
	Switch[OptionValue[Type], "ERK", TableauExplicit, "ESDIRK", TableauEsdirk, "SDIRK", TableauSdirk, "DIRK", TableauDirk, _, TableauFirk][s],
	Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];
RungeKutta[A_] := RungeKutta[A, Last[A]];
RungeKutta[A_, b_] := RungeKutta[A, b, Total[A, {2}]];
RungeKutta[HoldPattern[RungeKutta[A_, b_, c_, ___]], bHat_] := RungeKutta[A, b, c, bHat];

AddComposition[RungeKutta, RungeKuttaCompose, RkCompose];

RungeKutta /: x_ * HoldPattern[RungeKutta[A_, b_, c_]] := RungeKutta[A, x * b, c];
RungeKutta /: x_ * HoldPattern[RungeKutta[A_, b_, c_, bHat_]] := RungeKutta[A, x * b, c, x * bHat];

RungeKutta /: HoldPattern[RungeKutta[A1_, b1_, c1_, bHat1_] + RungeKutta[A2_, b2_, c2_, bHat2_]] := RungeKutta[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2], Join[bHat1, bHat2]];
RungeKutta /: HoldPattern[RungeKutta[A1_, b1_, c1_, ___] + RungeKutta[A2_, b2_, c2_, ___]] := RungeKutta[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2]];

RungeKutta /: HoldPattern[RungeKutta[A_, b_, c_]]^-1 := RungeKutta[A - ConstantArray[b, Length[b]], -b, c - 1];
RungeKutta /: HoldPattern[RungeKutta[A_, b_, c_, d_]]^-1 := RungeKutta[A - ConstantArray[b, Length[b]], -b, c - 1, -d];

RungeKuttaType[HoldPattern[RungeKutta[A_, __]]] := Which[TableauExplicitQ[A], "ERK", TableauEsdirkQ[A], "ESDIRK", TableauSdirkQ[A], "SDIRK", TableauDirkQ[A], "DIRK", True, "FIRK"];

RungeKuttaPrimary[HoldPattern[RungeKutta[A_, b_, c_, ___]]] := RungeKutta[A, b, c];

RungeKuttaEmbedded[HoldPattern[RungeKutta[A_, _, c_, bHat_]]] := RungeKutta[A, bHat, c];

RungeKuttaPairQ[HoldPattern[RungeKutta[_, _, _, _]]] := True;
RungeKuttaPairQ[_] := False;

RungeKuttaCollocation[c_List?VectorQ] := RungeKutta[
	Table[Integrate[LagrangeBasis[t, c, j], {t, 0, c[[i]]}], {i, Length[c]}, {j, Length[c]}],
	Table[Integrate[LagrangeBasis[t, c, i], {t, 0, \[FormalTheta]}], {i, Length[c]}],
	c
];

RungeKuttaA[HoldPattern[RungeKutta[A_, __]]] := A;

RungeKuttaDenseOutput[HoldPattern[RungeKutta[_, b_, __]]] := b;

RungeKuttaB[HoldPattern[RungeKutta[_, b_, _, bHat_:Null]], embedded_:False]:= If[embedded, bHat, b /. \[FormalTheta] -> 1];

RungeKuttaC[HoldPattern[RungeKutta[_, _, c_, ___]]] := c;

RungeKuttaBHat[HoldPattern[RungeKutta[_, _, _, bHat_]]] := bHat;

RungeKutta /: Graph[rk:HoldPattern[RungeKutta[A_, _, _, bHat___]], opts:OptionsPattern[WeightedAdjacencyGraph]] := With[{
		K = Replace[Join[A, {RungeKuttaB[rk], bHat}], 0 -> Infinity, {2}],
		s = Length[A]
	},
	WeightedAdjacencyGraph[
		PadRight[K, {Automatic, Length[K]}, Infinity],
		opts,
		DirectedEdges -> True,
		EdgeLabels -> "EdgeWeight",
		VertexLabels -> Table[i -> Which[i <= s, StringForm["\!\(\*SubscriptBox[\(Y\), \(``\)]\)", i], i == s + 1, "\!\(\*SubscriptBox[\(y\), \(n\)]\)", True, "\!\(\*SubscriptBox[OverscriptBox[\(y\), \(^\)], \(n\)]\)"], {i, Length[K]}]
	]
];

RungeKutta /: Length[HoldPattern[RungeKutta[A_, __]]] := Length[A];

RungeKutta /: Variables[HoldPattern[RungeKutta[a___]]] := Variables[{a}];

RungeKutta /: MakeBoxes[rk:HoldPattern[RungeKutta[A_List, b_List, c_List, bHat___]], format_] := GridBox[
	Join[Map[MakeBoxes[#, format] &, MapThread[Prepend, {A, c}], {2}], Map[Prepend[#, ""] &, Map[MakeBoxes[#, format] &, {RungeKuttaB[rk], bHat}, {2}]]],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[A] - 1], True]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
