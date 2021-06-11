(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rk`Methods`"];
Integreat`Rk`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

Rk::usage = "Constructs a Runge-Kutta method";
RkRescale::usage = "Rescales the timestep";
RkType::usage = "A string representation of the type of Runge-Kutta method";
RkPrimary::usage = "Removes the embedded method from a Runge-Kutta method";
RkEmbedded::usage = "Gets the embedded Runge-Kutta method";
RkPairQ::usage = "Returns True if m is a Runge-Kutta method with an embedded method";
RkCollocation::usage = "Constructs a collocated Runge-Kutta method";
RkCompose::usage = "Builds a single Runge-Kutta method from the composition of sub-methods";
RkA::usage = "Gets the A coefficients of a Runge-Kutta method";
RkDenseOutput::usage = "Gets the interpolatory b coeffients as a function of \[FormalTheta] for a Runge-Kutta method";
RkB::usage = "Gets the b coefficients of a Runge-Kutta method.  Optionally the second argument is a boolean for whether to return to return the embedded b coefficients.";
RkC::usage = "Gets the c coefficients of a Runge-Kutta method";
RkBHat::usage = "Gets the embedded coefficients of a Runge-Kutta method";
RkStages::usage = "Gets the number of stages of a Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Composition`"
}];

rkCompose[m_] := Rk[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RkA[m[[i, 1]]],
		i > j, m[[j, 2]] * ConstantArray[RkB[m[[j, 1]]], Length[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RkDenseOutput[First[#]] &, m]],
	Catenate[Map[Last[#] * RkC[First[#]] &, m] + FoldList[#1 + Last[#2] * Total[RkB[First[#2]]] &, 0, Most[m]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RkPairQ], Catenate[Map[Last[#] * RkBHat[First[#]] &, m]], Unevaluated[Sequence[]]]
];


(* ::Section:: *)
(*Package Definitions*)


Rk[s_Integer, OptionsPattern[{Type -> "FIRK"}]] := Rk[
	Switch[OptionValue[Type], "ERK", TableauExplicit, "ESDIRK", TableauEsdirk, "SDIRK", TableauSdirk, "DIRK", TableauDirk, _, TableauFirk][s],
	Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];
Rk[A_?SquareMatrixQ] := Rk[A, Last[A]];
Rk[A_?SquareMatrixQ, b_List] := Rk[A, b, Total[A, {2}]];
Rk[HoldPattern[Rk[A_, b_, c_, ___]], bHat_] := Rk[A, b, c, bHat];

AddComposition[Rk, RkCompose, rkCompose];

Rk /: x_ * HoldPattern[Rk[A_, b_, c_]] := Rk[A, x * b, c];
Rk /: x_ * HoldPattern[Rk[A_, b_, c_, bHat_]] := Rk[A, x * b, c, x * bHat];

RkRescale[HoldPattern[Rk[a__]], x_] := Apply[Rk, x * {a}];

Rk /: HoldPattern[Rk[A1_, b1_, c1_, bHat1_] + Rk[A2_, b2_, c2_, bHat2_]] := Rk[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2], Join[bHat1, bHat2]];
Rk /: HoldPattern[Rk[A1_, b1_, c1_, ___] + Rk[A2_, b2_, c2_, ___]] := Rk[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2]];

Rk /: Power[rk:HoldPattern[Rk[A_, b_, c_]], -1] := Rk[A - ConstantArray[RkB[rk], Length[c]], -b, c - 1];
Rk /: Power[rk:HoldPattern[Rk[A_, b_, c_, bHat_]], -1] := Rk[A - ConstantArray[RkB[rk], Length[c]], -b, c - 1, -bHat];

RkType[HoldPattern[Rk[A_, __]]] := Which[TableauExplicitQ[A], "ERK", TableauEsdirkQ[A], "ESDIRK", TableauSdirkQ[A], "SDIRK", TableauDirkQ[A], "DIRK", True, "FIRK"];

RkPrimary[HoldPattern[Rk[A_, b_, c_, ___]]] := Rk[A, b, c];

RkEmbedded[HoldPattern[Rk[A_, _, c_, bHat_]]] := Rk[A, bHat, c];

RkPairQ[HoldPattern[Rk[_, _, _, _]]] := True;
RkPairQ[_] := False;

RkCollocation[c_List?VectorQ] := With[{
		V = SeriesVander[Append[c, 1], 1, Length[c]].Inverse[SeriesVander[c, 0, Length[c] - 1]]
	},
	Rk[Most[V], Last[V], c]
];

RkA[HoldPattern[Rk[A_, __]]] := A;

RkDenseOutput[HoldPattern[Rk[_, b_, __]]] := b;

RkB[HoldPattern[Rk[_, b_, _, bHat_:Null]], embedded_?BooleanQ:False]:= If[embedded, bHat, b /. \[FormalTheta] -> 1];

RkC[HoldPattern[Rk[_, _, c_, ___]]] := c;

RkBHat[HoldPattern[Rk[_, _, _, bHat_]]] := bHat;

RkStages[HoldPattern[Rk[_, _, c_, ___]]] := Length[c];

Rk /: Graph[rk:HoldPattern[Rk[A_, _, _, bHat___]], opts:OptionsPattern[WeightedAdjacencyGraph]] := With[{
		K = Replace[Join[A, {RkB[rk], bHat}], 0 -> Infinity, {2}],
		s = Length[A]
	},
	WeightedAdjacencyGraph[
		PadRight[K, {Automatic, Length[K]}, Infinity],
		opts,
		DirectedEdges -> True,
		EdgeLabels -> "EdgeWeight",
		VertexLabels -> i_ -> Which[i <= s, StringForm["\!\(\*SubscriptBox[\(Y\), \(``\)]\)", i], i == s + 1, "\!\(\*SubscriptBox[\(y\), \(n+1\)]\)", True, "\!\(\*SubscriptBox[OverscriptBox[\(y\), \(^\)], \(n+1\)]\)"]
	]
];

Rk /: Variables[HoldPattern[Rk[a___]]] := Variables[{a}];

Rk /: MakeBoxes[rk:HoldPattern[Rk[A_List, b_List, c_List, bHat___]], format_] := TagBox[GridBox[
	Map[If[# === "", #, MakeBoxes[#, format]] &, ArrayFlatten[{{ArrayReshape[c, {Length[c], 1}], A}, {"", {RkB[rk], bHat}}}], {2}],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[c] - 1], True]
], Grid];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
