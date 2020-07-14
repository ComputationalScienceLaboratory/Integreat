(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rosenbrock`Methods`"];
Integreat`Rosenbrock`Methods::usage = "Package containing functions for creating Rosenbrock methods";

Rosenbrock::usage = "Constructs a Rosenbrock method";
RosenbrockPrimary::usage = "Removes the embedded method from a Rosenbrock method";
RosenbrockEmbedded::usage = "Gets the embedded Rosenbrock method";
RosenbrockPairQ::usage = "Returns True if m is a Rosenbrock method with an embedded method";
RosenbrockAlpha::usage = "Gets the \[Alpha] coefficients of a Rosenbrock method";
RosenbrockGamma::usage = "Gets the \[Gamma] coefficients of a Rosenbrock method";
RosenbrockBeta::usage = "Gets the \[Beta] coefficients of a Rosenbrock method";
RosenbrockDenseOutput::usage = "Gets the interpolatory b coeffients as a function of \[FormalTheta] for a Rosenbrock method";
RosenbrockB::usage = "Gets the b coefficients of a Rosenbrock method.  Optionally the second argument is a boolean for whether to return to return the embedded b coefficients.";
RosenbrockC::usage = "Gets the c coefficients of a Rosenbrock method";
RosenbrockD::usage = "Gets the d coefficients of a Rosenbrock method";
RosenbrockBHat::usage = "Gets the embedded coefficients of a Rosenbrock method";
RosenbrockStages::usage = "The number of stages in a Rosenbrock method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Catalog`",
	"Integreat`Internal`Composition`"
}];


(* ::Section:: *)
(*Package Definitions*)


Rosenbrock[s_Integer] := Rosenbrock[TableauExplicit[s, \[FormalAlpha]], TableauDirk[s, \[FormalGamma]], Table[Subscript[\[FormalB], i], {i, s}]];
Rosenbrock[a_, g_] := Rosenbrock[a, g, Last[a] + Last[g]];
Rosenbrock[HoldPattern[Rosenbrock[a_, g_, b_, ___]], bHat_] := Rosenbrock[a, g, b, bHat];

Rosenbrock /: HoldPattern[x_ * Rosenbrock[a_, g_, b_]] := Rosenbrock[a, g, x * b];
Rosenbrock /: HoldPattern[x_ * Rosenbrock[a_, g_, b_, bHat_]] := Rosenbrock[a, g, x * b, x * bHat];

Rosenbrock /: HoldPattern[Rosenbrock[a1_, g1_, b1_, bHat1_] + Rosenbrock[a2_, g2_, b2_, bHat2_]] := Rosenbrock[BlockDiag[a1, a2], BlockDiag[g1, g2], Join[b1, b2], Join[bHat1, bHat2]];
Rosenbrock /: HoldPattern[Rosenbrock[a1_, g1_, b1_, ___] + Rosenbrock[a2_, g2_, b2_, ___]] := Rosenbrock[BlockDiag[a1, a2], BlockDiag[g1, g2], Join[b1, b2]];

RosenbrockPrimary[HoldPattern[Rosenbrock[a_, g_, b_, ___]]] := Rosenbrock[a, g, b];

RosenbrockEmbedded[HoldPattern[Rosenbrock[a_, g_, _, bHat_]]] := Rosenbrock[a, g, bHat];

RosenbrockPairQ[HoldPattern[Rosenbrock[_, _, _, _]]] := True;
RosenbrockPairQ[_] := False;

RosenbrockAlpha[HoldPattern[Rosenbrock[a_, __]]] := a;

RosenbrockGamma[HoldPattern[Rosenbrock[_, g_, __]]] := g;

RosenbrockBeta[HoldPattern[Rosenbrock[a_, g_, __]]] := a + g;

RosenbrockDenseOutput[HoldPattern[Rosenbrock[_, _, b_, ___]]] := b;

RosenbrockB[HoldPattern[Rosenbrock[_, _, b_, bHat_:Null]], embedded_:False]:= If[embedded, bHat, b /. \[FormalTheta] -> 1];

RosenbrockC[HoldPattern[Rosenbrock[a_, __]]] := Total[a, {2}];

RosenbrockD[HoldPattern[Rosenbrock[_, g_, __]]] := Total[g, {2}];

RosenbrockBHat[HoldPattern[Rosenbrock[_, _, _, bHat_]]] := bHat;

RosenbrockStages[HoldPattern[Rosenbrock[a_, __]]] := Length[a];

Rosenbrock /: Length[HoldPattern[Rosenbrock[a_, __]]] := Length[a];

Rosenbrock /: Variables[HoldPattern[Rosenbrock[a___]]] := Variables[{a}];

(*Rosenbrock /: MakeBoxes[ros:HoldPattern[Rosenbrock[a_List, g_List, b_List, bHat___]], format_] := GridBox[
	Join[Map[MakeBoxes[#, format] &, MapThread[Prepend, {A, c}], {2}], Map[Prepend[#, ""] &, Map[MakeBoxes[#, format] &, {RungeKuttaB[rk], bHat}, {2}]]],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[A] - 1], True]
];*)


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
