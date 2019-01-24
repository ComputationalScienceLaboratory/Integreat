(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GLM`Methods`"];


CSL`OdeUtils`GLM`Methods::usage = "Package containing functions for creating general linear methods";

Glm::usage = "Constructs an association containing general linear method coefficients";
Dimsim::usage = "Constructs an association containing diagonally implicit multistage integration method coefficients";
DimsimQ::usage = "Returns True if input is a valid diagonally implicit multistage integration method, and False otherwise";
GlmA::usage = "Gets the A coefficients of a general linear method";
GlmB::usage = "Gets the B coefficients of a general linear method";
GlmU::usage = "Gets the U coefficients of a general linear method";
GlmV::usage = "Gets the V coefficients of a general linear method";
GlmC::usage = "Gets the c coefficients of a general linear method";
GlmInternalStages::usage = "Returns the number of internal stages in a general linear method";
GlmExternalStages::usage = "Returns the number of external stages in a general linear method";
GlmType::usage = "Returns the type number of the general linear method";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];

TypeToTableau[type_] := Switch[type, 1, TableauExplicit, 2, TableauSdirk, 3, TableauZeros, 4, TableauDiagonal, _, TableauFirk];

Phi[x_, c_, i_] := Product[x - c[[j]], {j, DeleteCases[Range[Length[c]], i]}];
D0[c_] := Table[Integrate[Phi[x, c, j], {x, 0, 1 + c[[i]]}] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];
D1[c_] := Table[Phi[1 + c[[i]], c, j] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];
D2[c_] := Table[Integrate[Phi[x, c, j], {x, 0, c[[i]]}] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];

GlmCheck[A_, B_, U_, V_, c_] := TableauQ[A] && MatrixQ[B] && MatrixQ[U] && TableauQ[V] && VectorQ[c] && With[{s = Length[c], r = Length[V]},
	Length[A] === s && Dimensions[B] === {r, s} && Dimensions[U] === {s, r}
];


Glm[s_Integer, r_Integer, type_Integer:0] := Glm[TypeToTableau[type][s], TableauFirk[{r, s}, \[FormalB]], TableauFirk[{s, r}, \[FormalU]], TableauFirk[r, \[FormalV]], Table[Subscript[\[FormalC], i], {i, s}]];
Glm[rk_RungeKutta] := Glm[RungeKuttaA[rk], {RungeKuttaB[rk]}, ConstantArray[1, {Length[rk], 1}], {{1}}, RungeKuttaC[rk]];

Glm /: HoldPattern[Times[x_, Glm[A_, B_, U_, V_, c_]]] := Glm[A, x * B, U, x * V, c];

Glm /: HoldPattern[Plus[Glm[A1_, B1_, U1_, V1_, c1_], Glm[A2_, B2_, U2_, V2_, c2_]]] := Glm[ArrayFlatten[{{A1, 0}, {0, A2}}], ArrayFlatten[{{B1, B2}}], ArrayFlatten[{{U1, 0}, {0, U2}}], ArrayFlatten[{{V1, V2}}], Join[c1, c2]];

Dimsim[s_Integer, r_Integer, type_Integer:2] := Dimsim[TypeToTableau[type][s], TableauFirk[{r, s}, \[FormalB]], Table[Subscript[\[FormalV], i], {i, r}], Table[Subscript[\[FormalC], i], {i, s}]];
Dimsim[A_?TableauQ, v_?VectorQ, c_?VectorQ] := Dimsim[A, D0[c] - A.D1[c] + ConstantArray[v, Length[v]].(A - D2[c]), v, c];
Dimsim[A_?TableauQ, B_?MatrixQ, v_?VectorQ, c_?VectorQ] := Glm[A, B, IdentityMatrix[{Length[A], Length[v]}], ConstantArray[v, Length[v]], c];

DimsimQ[m_] := MatchQ[m, HoldPattern[Glm[A_, B_, U_, V_, c_] /; TableauSdirk[A] && U === IdentityMatrix[Dimensions[U]] && Rank[V] === 1 && Total[First[V]] === 1]];

HoldPattern[GlmA[Glm[A_, __]]] := A;

HoldPattern[GlmB[Glm[_, B_, __]]] := B;

HoldPattern[GlmU[Glm[_, _, U_, __]]] := U;

HoldPattern[GlmV[Glm[_, _, _, V_, __]]] := V;

HoldPattern[GlmC[Glm[_, _, _, _, c_]]] := c;

HoldPattern[GlmInternalStages[Glm[A_, __]]] := Length[A];

HoldPattern[GlmExternalStages[Glm[_, B_, __]]] := Length[B];

GlmType[glm_Glm] := With[{A = GlmA[glm]},
	Which[
		TableauExplicitQ[A], 1,
		TableauSdirkQ[A], 2,
		TableauZerosQ[A], 3,
		TableauDiagonalQ[A], 4,
		True, Undefined
	]
];

Glm /: MakeBoxes[Glm[A_List, B_List, U_List, V_List, c_List], format_] := GridBox[
	ArrayFlatten[{
		{Map[{MakeBoxes[#, format]} &, c], Map[MakeBoxes[#, format] &, A, {2}], Map[MakeBoxes[#, format] &, U, {2}]},
		{ConstantArray[{""}, Length[B]], Map[MakeBoxes[#, format] &, B, {2}], Map[MakeBoxes[#, format] &, V, {2}]}
	}],
	ColumnLines -> Join[{True}, ConstantArray[False, Length[c] - 1], {True, False}],
	RowLines -> Join[ConstantArray[False, Length[c] - 1], {True, False}]
];

AddCatalog[
	Glm,
	{"GLM 2-1", Glm[{{0,0},{2490/1943,0}}, {{2723542/2656081,224/1367},{9733585/10624324,1406013/10624324}}, {{1,0},{0,1}}, {{448/1367,919/1367},{-471/2734,3205/2734}}, {0,1}]},
	{"GLM 2-3", Glm[{{0,0},{0,0}}, {{-2371/15160,-2371/15160},{-13741/15160,16579/15160}}, {{1,0},{0,1}}, {{-2371/7580,9951/7580},{-6161/7580,13741/7580}}, {0,1}]},
	{"DIMSIM 2-1", Dimsim[{{0,0},{2,0}}, {{5/4,1/4},{3/4,-1/4}}, {1/2,1/2}, {0,1}]},
	{"DIMSIM 2-2", Dimsim[{{1-1/Sqrt[2],0},{(6+2*Sqrt[2])/7,1-1/Sqrt[2]}}, {{(73-34*Sqrt[2])/28,(4*Sqrt[2]-5)/4},{(87-48*Sqrt[2])/28,(34*Sqrt[2]-45)/28}}, {(3-Sqrt[2])/2, (Sqrt[2]-1)/2}, {0,1}]},
	{"DIMSIM 2-3", Dimsim[{{0,0},{0,0}}, {{-3/8,-3/8},{-7/8,9/8}}, {-3/4, 7/4}, {0,1}]},
	{"DIMSIM 2-4", Dimsim[{{(3-Sqrt[3])/2,0},{0,(3-Sqrt[3])/2}}, {{(18-11*Sqrt[3])/4,(7*Sqrt[3]-12)/4},{(22-13*Sqrt[3])/4,(9*Sqrt[3]-12)/4}}, {(3-2*Sqrt[3])/2,(2*Sqrt[3]-1)/2}, {0,1}]},
	{"DIMSIM 3-1", Dimsim[{{0,0,0},{1,0,0},{1/4,1,0}}, {{5/4,1/3,1/6},{35/24,-1/3,1/8},{17/12,0,1/12}}, {-2/3,4/3,1/3}, {0,1/2,1}]}
];

Glm[args___] /; Not[Length[Unevaluated[args]] === 5 && GlmCheck[args]] := $Failed;


End[];


EndPackage[];
