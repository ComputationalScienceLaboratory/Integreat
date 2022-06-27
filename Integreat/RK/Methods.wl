(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Methods`"];

RK::usage =
	"RK[] returns a Dataset containing a catalog of Runge-Kutta methods.\n" <>
	"RK[\"name\"] retreives the method named name from the catalog.\n" <>
	"RK[s] constructs a generic s-stage Runge-Kutta method.\n" <>
	"RK[A] constructs a Runge-Kutta method with b coefficients taken to be the last row of A and c coefficients that are the row sum of A.\n" <>
	"RK[A, b] constructs a Runge-Kutta method with c coefficients that are the row sum of A.\n" <>
	"RK[A, b, c] constructions a Runge-Kutta method with coefficients A, b, and c.\n" <>
	"RK[A, b, c, bHat] constructs an embedded Runge-Kutta pair.\n" <>
	"RK[rk, bHat] adds embedded coefficients bHat to rk."
RKRescale::usage = "RKRescale[rk, x] scales all coefficients of rk by x.";
RKType::usage = "RKType[rk] returns a string representation of the tableau type of rk.";
RKPrimary::usage = "RKPrimary[rk] creates a copy of rk without an embedded method.";
RKEmbedded::usage = "RKEmbedded[rk] creates a copy of rk in which";
RKPairQ::usage = "RKPairQ[rk] returns True if rk is an embedded Runge-Kutta pair and False, otherwise.";
RKCollocation::usage = "RKCollocation[c] constructs a collocation method with abscissae c.";
RKCompose::usage
	"RKCompose[rk1, \[Ellipsis], rkm] creates a Runge-Kutta method from a step of rk1, \[Ellipsis], rkm in sequence using a step size h/m.\n" <>
	"RKCompose[{rk1, w1}, \[Ellipsis], {rkm, wm}] composes rk1, \[Ellipsis], rkm using step sizes w1*h, \[Ellipsis], wm*h, respectively.\n" <>
	"rk1[rk2] composes a half step of rk1 with a half step of rk2.\n" <>
	"rk^p composes p steps of rk."
RKA::usage = "RKA[rk] returns the A coefficient matrix of rk.";
RKDenseOutput::usage = "RKDenseOutput[rk] returns the dense output coefficients of rk paramtereized by \[FormalTheta].";
RKB::usage = "RKB[rk] returns the b weights of rk.";
RKC::usage = "RKC[rk] returns the abscissae of rk.";
RKBHat::usage = "RKBHat[rk] returns the embedded coefficients of an embedded pair rk.";
RKStages::usage = "RKStages[rk] returns the number of stages of rk.";


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

rkB[False, None, True, _, b_, _] := b;
rkB[False, None, False, _, b_, _] := b /. \[FormalTheta] -> 1;
rkB[False, None, do_, _, b_, _] := b /. \[FormalTheta] -> do;
rkB[False, i_Integer, _, A_, _, _] := A[[i]];
rkB[True, _, _, _, _, bHat_] := bHat;


(* ::Section:: *)
(*Package Definitions*)


RK[s_Integer, OptionsPattern[{Type -> "FIRK"}]] := RK[
	Switch[OptionValue[Type], "ERK", TableauExplicit, "ESDIRK", TableauESDIRK, "SDIRK", TableauSDIRK, "DIRK", TableauDIRK, _, TableauFIRK][s],
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

RKType[HoldPattern[RK[A_, __]]] := Which[TableauExplicitQ[A], "ERK", TableauESDIRKQ[A], "ESDIRK", TableauSDIRKQ[A], "SDIRK", TableauDIRKQ[A], "DIRK", True, "FIRK"];

RKPrimary[HoldPattern[RK[A_, b_, c_, ___]]] := RK[A, b, c];

RKEmbedded[HoldPattern[RK[A_, _, c_, bHat_]]] := RK[A, bHat, c];

RKPairQ[HoldPattern[RK[_, _, _, _]]] := True;
RKPairQ[_] := False;

RKCollocation[c_List /; VectorQ[c] && DuplicateFreeQ[c]] := With[{
		V = SeriesVander[Append[c, \[FormalTheta]], 1, Length[c]] . Inverse[SeriesVander[c, 0, Length[c] - 1]]
	},
	RK[Most[V], Last[V], c]
];

RKA[HoldPattern[RK[A_, __]]] := A;

RKDenseOutput[HoldPattern[RK[_, b_, __]]] := b;

Options[RKB] = {Embedded -> False, Stage -> None, DenseOutput -> False};
RKB[HoldPattern[RK[A_, b_, _, bHat_:True]], OptionsPattern[]] := With[{
		em = OptionValue[Embedded],
		s = OptionValue[Stage],
		do = OptionValue[DenseOutput]
	},
	rkB[em, s, do, A, b, bHat] /; BooleanQ[em] && (s === None || IntegerQ[s]) && bHat =!= em
];

RKC[HoldPattern[RK[_, _, c_, ___]]] := c;

RKBHat[HoldPattern[RK[_, _, _, bHat_]]] := bHat;

RKStages[HoldPattern[RK[_, _, c_, ___]]] := Length[c];

RK /: Graph[HoldPattern[RK[A_, b_, _, bHat___]], opts:OptionsPattern[WeightedAdjacencyGraph]] := With[{
		K = Replace[Join[A, {b, bHat}], 0 | _?ZeroQ -> Infinity, {2}],
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
