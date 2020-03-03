(* ::Package:: *)

BeginPackage["Integreat`GLM`Methods`", {
	"Integreat`Tableaus`",
	"Integreat`RungeKutta`Methods`",
	"Integreat`RungeKutta`OrderConditions`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Catalog`",
	"Integreat`Internal`Composition`"
}];


Integreat`GLM`Methods::usage = "Package containing functions for creating general linear methods";

Glm::usage = "Constructs an association containing general linear method coefficients";
GlmCompose::usage = "";
GlmDimsim::usage = "Constructs an association containing diagonally implicit multistage integration method coefficients";
GlmPeer::usage = "Constructs a peer type general linear method";
GlmParallelEnsemble::usage = "Constructs a parallel emsemble general linear method";
GlmA::usage = "Gets the A coefficients of a general linear method";
GlmB::usage = "Gets the B coefficients of a general linear method";
GlmU::usage = "Gets the U coefficients of a general linear method";
GlmV::usage = "Gets the V coefficients of a general linear method";
GlmQ::usage = "Gets the Q coefficients of a general linear method";
GlmC::usage = "Gets the c coefficients of a general linear method";
GlmInternalStages::usage = "Returns the number of internal stages in a general linear method";
GlmExternalStages::usage = "Returns the number of external stages in a general linear method";
GlmOrder::usage = "Returns the order of a general linear method";
GlmType::usage = "Returns the type number of the general linear method";
GlmTransform::usage = "Transforms a general linear method into an equivalent formulation";


Begin["`Private`"];

TypeToTableau[type_] := Switch[type, 1, TableauExplicit, 2, TableauSdirk, 3, TableauZeros, 4, TableauDiagonal, _, TableauFirk];

GlmComp[m_] := With[{
		n = Length[m]
	},
	Glm[
		ArrayFlatten[Table[Which[
			i == j, m[[i, 2]] * GlmA[m[[i, 1]]],
			i > j, m[[j, 2]] * GlmU[m[[i, 1]]] . Dot @@ Map[GlmV, m[[i - 1;;j + 1;;-1, 1]]] . GlmB[m[[j, 1]]],
			True, 0
		], {i, n}, {j, n}]],
		ArrayFlatten[{Table[m[[i, 2]] * Dot @@ Map[GlmV, m[[n;;i + 1;;-1, 1]]] . GlmB[m[[n + 1 - i, 1]]], {i, n}]}],
		ArrayFlatten[Table[{GlmU[m[[i, 1]]] . Dot @@ Map[GlmV, m[[1;;i - 1, 1]]]}, {i, n}]],
		Dot @@ Map[GlmV, m[[All, 1]]],
		GlmQ[m[[1, 1]]] . DiagonalMatrix[m[[1, 2]] ^ Range[0, GlmOrder[m[[1, 1]]]]],
		Catenate[Map[Last[#] * GlmC[First[#]] &, m] + FoldList[Plus, 0, m[[1 ;; -2, 2]]]]
	]
];

Phi[x_, c_, i_] := Product[x - c[[j]], {j, DeleteCases[Range[Length[c]], i]}];
D0[c_] := Table[Integrate[Phi[x, c, j], {x, 0, 1 + c[[i]]}] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];
D1[c_] := Table[Phi[1 + c[[i]], c, j] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];
D2[c_] := Table[Integrate[Phi[x, c, j], {x, 0, c[[i]]}] / Phi[c[[j]], c, j], {i, Length[c]}, {j, Length[c]}];
DimsimB[A_, v_, c_] := D0[c] - A.D1[c] + ConstantArray[v.(A - D2[c]), Length[v]];

GlmCheck[A_, B_, U_, V_, Q_, c_] := SquareMatrixQ[A] && MatrixQ[B] && MatrixQ[U] && SquareMatrixQ[V] && MatrixQ[Q] && VectorQ[c] && With[{s = Length[c], r = Length[V]},
	Length[A] === s && Dimensions[B] === {r, s} && Dimensions[U] === {s, r} && Length[Q] === r
];


Glm[s_Integer, r_Integer, p_Integer, OptionsPattern[{GlmType -> 0}]] := Glm[TypeToTableau[OptionValue[GlmType]][s], TableauFirk[{r, s}, \[FormalB]], TableauFirk[{s, r}, \[FormalU]], TableauFirk[r, \[FormalV]], Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}], Table[Subscript[\[FormalC], i], {i, s}]];
Glm[rk_RungeKutta, p_Integer] := Glm[RungeKuttaA[rk], {RungeKuttaB[rk]}, ConstantArray[1, {Length[rk], 1}], {{1}}, {UnitVector[p + 1, 1]}, RungeKuttaC[rk]];
Glm[rk_RungeKutta] := Glm[rk, RungeKuttaOrder[rk]];

Glm /: HoldPattern[Times[x_, Glm[A_, B_, U_, V_, Q_, c_]]] := Glm[A, x * B, U, x * V, x * Q, c];

Glm /: HoldPattern[Plus[Glm[A1_, B1_, U1_, V1_, Q1_, c1_], Glm[A2_, B2_, U2_, V2_, Q2_, c2_]]] := With[{
		pMin = Min[Dimensions[Q1][[2]], Dimensions[Q2][[2]]]
	},
	Glm[ArrayFlatten[{{A1, 0}, {0, A2}}], ArrayFlatten[{{B1, B2}}], ArrayFlatten[{{U1, 0}, {0, U2}}], ArrayFlatten[{{V1, V2}}], Q1[[All, 1;;pMin]] + Q2[[All, 1;;pMin]], Join[c1, c2]]
];

AddComposition[Glm, GlmCompose, GlmComp];

GlmDimsim[s_Integer, r_Integer, p_Integer, OptionsPattern[{GlmType -> 2}]] := With[{
		v1 = Table[Subscript[\[FormalV], i], {i, r - 1}]
	},
	GlmDimsim[TypeToTableau[OptionValue[GlmType]][s], TableauFirk[{r, s}, \[FormalB]], Append[v1, 1 - Total[v1]], Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}], Table[Subscript[\[FormalC], i], {i, s}]]
];
GlmDimsim[A_?SquareMatrixQ, v_?VectorQ, c_?VectorQ, o:{p_Integer, q_Integer}] /; Length[c] === Length[v] === p === q := With[{
		s = Length[c],
		C = SeriesVander[c, -1, Length[c]]
	},
	GlmDimsim[A, DimsimB[A, v, c], v, C[[All, 2;;q + 2]] - A.C[[All, 1;;q+1]], c]
];
GlmDimsim[A_?SquareMatrixQ, B_?MatrixQ, v_?VectorQ, Q_?MatrixQ, c_?VectorQ] := Glm[A, B, IdentityMatrix[{Length[c], Length[v]}], ConstantArray[v, Length[v]], Q, c];

GlmPeer[B_?MatrixQ, A_?SquareMatrixQ, R_?SquareMatrixQ, c_?VectorQ, p_Integer] := With[{
		BA = ArrayFlatten[{{B, A}}]
	},
	Glm[R, ArrayFlatten[{{R}, {IdentityMatrix[Length[R]]}}], BA, KroneckerProduct[{{1}, {0}}, BA], ArrayFlatten[{{SeriesVander[c - 1, 0, p]}, {SeriesVander[c - 1, -1, p - 1]}}], c]
];

GlmParallelEnsemble[c_?VectorQ, \[Lambda]_] := With[{
		s = Length[c],
		i = Range[2, Length[c]],
		I = IdentityMatrix[Length[c]],
		C = SeriesVander[c, -1, Length[c]]
	},
	Glm[\[Lambda] * I, C[[All, 2;;s+1]].ToeplitzMatrix[UnitVector[s, 1], Prepend[(1 - \[Lambda] * i) / i!, 1]].Inverse[C[[All, 2;;s+1]]], I, I, C[[All, 2;;]] - \[Lambda] * C[[All, ;;s+1]], c]
];

HoldPattern[GlmA[Glm[A_, __]]] := A;

HoldPattern[GlmB[Glm[_, B_, __]]] := B;

HoldPattern[GlmU[Glm[_, _, U_, __]]] := U;

HoldPattern[GlmV[Glm[_, _, _, V_, __]]] := V;

HoldPattern[GlmQ[Glm[_, _, _, _, Q_, __]]] := Q;

HoldPattern[GlmC[Glm[_, _, _, _, _, c_]]] := c;

HoldPattern[GlmInternalStages[Glm[A_, __]]] := Length[A];

HoldPattern[GlmExternalStages[Glm[_, B_, __]]] := Length[B];

HoldPattern[GlmOrder[Glm[_, _, _, _, Q_, __]]] := Dimensions[Q][[2]] - 1;

GlmType[glm_Glm] := With[{A = GlmA[glm]},
	Which[
		TableauZerosQ[A], 3,
		TableauDiagonalQ[A], 4,
		TableauExplicitQ[A], 1,
		TableauSdirkQ[A], 2,
		True, Undefined
	]
];

GlmTransform[Glm[A_, B_, U_, V_, W_, c_], T_?SquareMatrixQ] := With[{
		Tinv = Inverse[T]
	},
	Glm[A, T.B, U.Tinv, T.V.Tinv, T.W, c]
];

Glm /: Variables[HoldPattern[Glm[a___]]] := Variables[{a}];

Glm /: HoldPattern[MakeBoxes[Glm[A_List, B_List, U_List, V_List, _List, c_List], format_]] := GridBox[
	ArrayFlatten[{
		{Map[{MakeBoxes[#, format]} &, c], Map[MakeBoxes[#, format] &, A, {2}], Map[MakeBoxes[#, format] &, U, {2}]},
		{ConstantArray[{""}, Length[B]], Map[MakeBoxes[#, format] &, B, {2}], Map[MakeBoxes[#, format] &, V, {2}]}
	}],
	ColumnLines -> Join[{True}, ConstantArray[False, Length[c] - 1], {True, False}],
	RowLines -> Join[ConstantArray[False, Length[c] - 1], {True, False}]
];

AddCatalog[
	Glm,
	{"GLM 2-1", Glm[
		{{0,0},{2490/1943,0}},
		{{2723542/2656081,224/1367},{9733585/10624324,1406013/10624324}},
		{{1,0},{0,1}},
		{{448/1367,919/1367},{-471/2734,3205/2734}},
		{{1,0,0},{1,-547/1943,1/2}},
		{0,1}
	]},
	{"GLM 2-3", Glm[
		{{0,0},{0,0}},
		{{-2371/15160,-2371/15160},{-13741/15160,16579/15160}},
		{{1,0},{0,1}},
		{{-2371/7580,9951/7580},{-6161/7580,13741/7580}},
		{{1,0,0},{1,1,1/2}},
		{0,1}
	]},
	{"GLM 2-4", Glm[
		{{2-Sqrt[2],0},{0,2-Sqrt[2]}},
		{{9-7 Sqrt[2],-7+5 Sqrt[2]},{13-9 Sqrt[2],-9+7 Sqrt[2]}},
		{{1,0},{0,1}},
		{{2-2 Sqrt[2],-1+2 Sqrt[2]},{3-2 Sqrt[2],-2+2 Sqrt[2]}},
		{{1,-2+Sqrt[2],0},{1,-1+Sqrt[2],-(3/2)+Sqrt[2]}},
		{0,1}
	]},
	{"DIMSIM 2-1", Glm[
		{{0,0},{2,0}},
		{{5/4,1/4},{3/4,-1/4}},
		{{1,0},{0,1}},
		{{1/2,1/2},{1/2,1/2}},
		{{1,0,0},{1,-1,1/2}},
		{0,1}
	]},
	{"DIMSIM 2-2", Glm[
		{{1-1/Sqrt[2],0},{2/7 (3+Sqrt[2]),1-1/Sqrt[2]}},
		{{1/28 (73-34 Sqrt[2]),-(5/4)+Sqrt[2]},{1/28 (87-48 Sqrt[2]),1/28 (-45+34 Sqrt[2])}},
		{{1,0},{0,1}},
		{{3/2-1/Sqrt[2],-(1/2)+1/Sqrt[2]},{3/2-1/Sqrt[2],-(1/2)+1/Sqrt[2]}},
		{{1,-1+1/Sqrt[2],0},{1,3/14 (-4+Sqrt[2]),-(1/2)+1/Sqrt[2]}},
		{0,1}
	]},
	{"DIMSIM 2-3", Glm[
		{{0,0},{0,0}},
		{{-(3/8),-(3/8)},{-(7/8),9/8}},
		{{1,0},{0,1}},
		{{-(3/4),7/4},{-(3/4),7/4}},
		{{1,0,0},{1,1,1/2}},
		{0,1}
	]},
	{"DIMSIM 2-4", Glm[
		{{1/2 (3-Sqrt[3]),0},{0,1/2 (3-Sqrt[3])}},
		{{9/2-(11 Sqrt[3])/4,-3+(7 Sqrt[3])/4},{1/4 (22-13 Sqrt[3]),-3+(9 Sqrt[3])/4}},
		{{1,0},{0,1}},
		{{3/2-Sqrt[3],-(1/2)+Sqrt[3]},{3/2-Sqrt[3],-(1/2)+Sqrt[3]}},
		{{1,1/2 (-3+Sqrt[3]),0},{1,1/2 (-1+Sqrt[3]),1/2 (-2+Sqrt[3])}},
		{0,1}
	]},
	{"DIMSIM 3-1", Glm[
		{{0,0,0},{1,0,0},{1/4,1,0}},
		{{5/4,1/3,1/6},{35/24,-(1/3),1/8},{17/12,0,1/12}},
		{{1,0,0},{0,1,0},{0,0,1}},
		{{-(2/3),4/3,1/3},{-(2/3),4/3,1/3},{-(2/3),4/3,1/3}},
		{{1,0,0,0},{1,-(1/2),1/8,1/48},{1,-(1/4),0,1/24}},
		{0,1/2,1}
	]}
];

Glm[args___] /; Not[Length[Unevaluated[args]] === 6 && GlmCheck[args]] := $Failed;


End[];
EndPackage[];
