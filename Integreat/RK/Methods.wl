(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Methods`"];
Integreat`RK`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RK::usage = "Constructs a Runge-Kutta method";
RKRescale::usage = "Rescales the timestep";
RKType::usage = "A string representation of the type of Runge-Kutta method";
RKPrimary::usage = "Removes the embedded method from a Runge-Kutta method";
RKEmbedded::usage = "Gets the embedded Runge-Kutta method";
RKPairQ::usage = "Returns True if m is a Runge-Kutta method with an embedded method";
RKCollocation::usage = "Constructs a collocated Runge-Kutta method";
RKCompose::usage = "Builds a single Runge-Kutta method from the composition of sub-methods";
RKA::usage = "Gets the A coefficients of a Runge-Kutta method";
RKDenseOutput::usage = "Gets the interpolatory b coeffients as a function of \[FormalTheta] for a Runge-Kutta method";
RKB::usage = "Gets the b coefficients of a Runge-Kutta method.  Optionally the second argument is a boolean for whether to return to return the embedded b coefficients.";
RKC::usage = "Gets the c coefficients of a Runge-Kutta method";
RKBHat::usage = "Gets the embedded coefficients of a Runge-Kutta method";
RKStages::usage = "Gets the number of stages of a Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Composition`"
}];

rkCompose[m_] := RK[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RKA[m[[i, 1]]],
		i > j, m[[j, 2]] * ConstantArray[RKB[m[[j, 1]]], RKStages[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RKDenseOutput[First[#]] &, m]],
	Catenate[Map[Last[#] * RKC[First[#]] &, m] + FoldList[#1 + Last[#2] * Total[RKB[First[#2]]] &, 0, Most[m]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RKPairQ], Catenate[Map[Last[#] * RKBHat[First[#]] &, m]], Unevaluated[Sequence[]]]
];


(* ::Section:: *)
(*Package Definitions*)


RK[s_Integer, OptionsPattern[{Type -> "FIRK"}]] := RK[
	Switch[OptionValue[Type], "ERK", TableauExplicit, "ESDIRK", TableauEsdirk, "SDIRK", TableauSdirk, "DIRK", TableauDirk, _, TableauFirk][s],
	Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];
RK[A_?SquareMatrixQ] := RK[A, Last[A]];
RK[A_?SquareMatrixQ, b_List] := RK[A, b, Total[A, {2}]];
RK[HoldPattern[RK[A_, b_, c_, ___]], bHat_] := RK[A, b, c, bHat];

AddComposition[RK, RKCompose, rkCompose];

RK /: x_ * HoldPattern[RK[A_, b_, c_]] := RK[A, x * b, c];
RK /: x_ * HoldPattern[RK[A_, b_, c_, bHat_]] := RK[A, x * b, c, x * bHat];

RKRescale[HoldPattern[RK[a__]], x_] := Apply[RK, x * {a}];

RK /: HoldPattern[RK[A1_, b1_, c1_, bHat1_] + RK[A2_, b2_, c2_, bHat2_]] := RK[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2], Join[bHat1, bHat2]];
RK /: HoldPattern[RK[A1_, b1_, c1_, ___] + RK[A2_, b2_, c2_, ___]] := RK[BlockDiag[A1, A2], Join[b1, b2], Join[c1, c2]];

RK /: Power[rk:HoldPattern[RK[A_, bDO_, c_]], -1] := With[{
		b = RKB[rk]
	},
	RK[A - ConstantArray[b, Length[b]], -bDO, c - Total[b]]
];
RK /: Power[rk:HoldPattern[RK[A_, bDO_, c_, bHat_]], -1] := With[{
		b = RKB[rk]
	},
	RK[A - ConstantArray[b, Length[b]], -bDO, c - Total[b], -bHat]
];

RKType[HoldPattern[RK[A_, __]]] := Which[TableauExplicitQ[A], "ERK", TableauEsdirkQ[A], "ESDIRK", TableauSdirkQ[A], "SDIRK", TableauDirkQ[A], "DIRK", True, "FIRK"];

RKPrimary[HoldPattern[RK[A_, b_, c_, ___]]] := RK[A, b, c];

RKEmbedded[HoldPattern[RK[A_, _, c_, bHat_]]] := RK[A, bHat, c];

RKPairQ[HoldPattern[RK[_, _, _, _]]] := True;
RKPairQ[_] := False;

RKCollocation[c_List?VectorQ] := With[{
		V = SeriesVander[Append[c, \[FormalTheta]], 1, Length[c]] . Inverse[SeriesVander[c, 0, Length[c] - 1]]
	},
	RK[Most[V], Last[V], c]
];

RKA[HoldPattern[RK[A_, __]]] := A;

RKDenseOutput[HoldPattern[RK[_, b_, __]]] := b;

Options[RKB] = {Embedded -> False, DenseOutput -> False};
RKB[HoldPattern[RK[_, b_, _, bHat_:Null]], OptionsPattern[]] := Which[
	TrueQ[OptionValue[Embedded]], bHat,
	TrueQ[OptionValue[DenseOutput]], b,
	True, b /. \[FormalTheta] -> 1
];

RKC[HoldPattern[RK[_, _, c_, ___]]] := c;

RKBHat[HoldPattern[RK[_, _, _, bHat_]]] := bHat;

RKStages[HoldPattern[RK[_, _, c_, ___]]] := Length[c];

RK /: Graph[HoldPattern[RK[A_, b_, _, bHat___]], opts:OptionsPattern[WeightedAdjacencyGraph]] := With[{
		K = Replace[Join[A, {b, bHat}], 0 | _?PossibleZeroQ -> Infinity, {2}],
		s = Length[A]
	},
	WeightedAdjacencyGraph[
		PadRight[K, {Automatic, Length[K]}, Infinity],
		opts,
		DirectedEdges -> True,
		EdgeLabels -> "EdgeWeight",
		VertexLabels -> i_ :> Which[i <= s, StringForm["\!\(\*SubscriptBox[\(Y\), \(``\)]\)", i], i == s + 1, "\!\(\*SubscriptBox[\(y\), \(n+1\)]\)", True, "\!\(\*SubscriptBox[OverscriptBox[\(y\), \(^\)], \(n+1\)]\)"]
	]
];

RK /: Variables[HoldPattern[RK[a___]]] := Variables[{a}];

RK /: MakeBoxes[rk:HoldPattern[RK[A_List, b_List, c_List, bHat___]], format_] := TagBox[GridBox[
	Map[If[# === "", #, MakeBoxes[#, format]] &, ArrayFlatten[{{ArrayReshape[c, {Length[c], 1}], A}, {"", {RKB[rk], bHat}}}], {2}],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[c] - 1], True]
], Grid];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
