(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Glm`Methods`"];
Integreat`Glm`Methods::usage = "Package containing functions for creating general linear methods";

Glm::usage = "Constructs an association containing general linear method coefficients";
GlmCompose::usage = "";
GlmDimsim::usage = "Constructs an association containing diagonally implicit multistage integration method coefficients";
GlmPeer::usage = "Constructs a peer type general linear method";
GlmOneLeg::usage = "Constructs a one leg method";
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


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`Rk`Methods`",
	"Integreat`Rk`OrderConditions`",
	"Integreat`Lmm`Methods`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Catalog`",
	"Integreat`Internal`Composition`"
}];

TypeToTableau[type_] := Switch[type, 1, TableauExplicit, 2, TableauSdirk, 3, TableauZeros, 4, TableauDiagonal, _, TableauFirk];

HankelBR[x_] := HankelMatrix[Append[ConstantArray[0, Length[x] - 2], First[x]], Most[x]];

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


(* ::Section:: *)
(*Package Definitions*)


Glm[s_Integer, r_Integer, p_Integer, OptionsPattern[{Type -> 0}]] := Glm[TypeToTableau[OptionValue[Type]][s], TableauFirk[{r, s}, \[FormalB]], TableauFirk[{s, r}, \[FormalU]], TableauFirk[r, \[FormalV]], Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}], Table[Subscript[\[FormalC], i], {i, s}]];
Glm[rk_RungeKutta, p_Integer] := Glm[RungeKuttaA[rk], {RungeKuttaB[rk]}, ConstantArray[1, {Length[rk], 1}], {{1}}, {UnitVector[p + 1, 1]}, RungeKuttaC[rk]];
Glm[rk_RungeKutta] := Glm[rk, RungeKuttaOrder[rk]];
Glm[lmm_Lmm] := With[{
		k = Length[lmm],
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		c = 1 - LengthWhile[Reverse[LmmBeta[lmm]], PossibleZeroQ]
	},
	Glm[
		{{Last[b] / Last[a]}},
		Transpose[{Most[b]  - Last[b] / Last[a] * Most[a]}],
		{Append[ConstantArray[0, k - 1], 1 / Last[a]]},
		Transpose[CompanionMatrix[-Most[a] / Last[a]]],
		With[{v = SeriesVander[Range[c - k, c - 1], -1, k]}, HankelBR[b].v[[All, ;;-2]] - HankelBR[a].v[[All, 2;;]]],
		{c}
	]
];

AddComposition[Glm, GlmCompose, GlmComp];

Glm /: HoldPattern[x_ * Glm[A_, B_, U_, V_, Q_, c_]] := Glm[A, x * B, U, x * V, x * Q, c];

Glm /: HoldPattern[Glm[A1_, B1_, U1_, V1_, Q1_, c1_] + Glm[A2_, B2_, U2_, V2_, Q2_, c2_]] := With[{
		pMin = Min[Dimensions[Q1][[2]], Dimensions[Q2][[2]]]
	},
	Glm[BlockDiag[A1, A2], BlockDiag[B1, B2], BlockDiag[U1, U2], BlockDiag[V1, V2], ArrayFlatten[{{Q1[[All, ;;pMin]]}, {Q2[[All, ;;pMin]]}}], Join[c1, c2]]
];

GlmDimsim[s_Integer, r_Integer, p_Integer, OptionsPattern[{Type -> 2}]] := With[{
		v1 = Table[Subscript[\[FormalV], i], {i, r - 1}]
	},
	GlmDimsim[TypeToTableau[OptionValue[Type]][s], TableauFirk[{r, s}, \[FormalB]], Append[v1, 1 - Total[v1]], Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}], Table[Subscript[\[FormalC], i], {i, s}]]
];
GlmDimsim[A_?SquareMatrixQ, v_?VectorQ, c_?VectorQ] /; Length[A] === Length[v] === Length[c] := With[{
		C = SeriesVander[c, -1, Length[c]],
		s = Length[c]
	},
	With[{
			Q = C[[All, 2;;]] - A.C[[All, ;;-2]],
			mu = ToeplitzMatrix[Join[{1, 1}, ConstantArray[0, s - 1]], 1 / Range[s]!]
		},
		GlmDimsim[A, (Q.mu - ConstantArray[v.Q[[All, 2;;]], s]).Inverse[C[[All, 2;;-2]]], v, Q, c]
	]
];
GlmDimsim[A_?SquareMatrixQ, B_?MatrixQ, v_?VectorQ, Q_?MatrixQ, c_?VectorQ] := Glm[A, B, IdentityMatrix[{Length[c], Length[v]}], ConstantArray[v, Length[v]], Q, c];

GlmPeer[B_?MatrixQ, A_?SquareMatrixQ, R_?SquareMatrixQ, c_?VectorQ, p_Integer] := With[{
		BA = ArrayFlatten[{{B, A}}]
	},
	Glm[R, ArrayFlatten[{{R}, {IdentityMatrix[Length[R]]}}], BA, KroneckerProduct[{{1}, {0}}, BA], ArrayFlatten[{{SeriesVander[c - 1, 0, p]}, {SeriesVander[c - 1, -1, p - 1]}}], c]
];

GlmOneLeg[a_List, b_List, p_Integer] := With[{
		q = Length[a] - 1,
		aq = Last[a],
		bq = Last[b]
	},
	Glm[{{bq / aq}}, Append[ConstantArray[{0}, q - 1], {1 / aq}], {Most[b] - bq / aq * Most[a]}, CompanionMatrix[-Most[a] / aq], SeriesVander[Range[1 - q, 0], 0, p], {b.Range[1-q,1]}]
];

GlmParallelEnsemble[c_?VectorQ, \[Lambda]_:0] := With[{
		s = Length[c],
		i = Range[2, Length[c]],
		I = IdentityMatrix[Length[c]],
		C = SeriesVander[c, -1, Length[c]]
	},
	Glm[\[Lambda] * I, C[[All, 2;;s+1]].ToeplitzMatrix[UnitVector[s, 1], Prepend[(1 - \[Lambda] * i) / i!, 1]].Inverse[C[[All, 2;;s+1]]], I, I, C[[All, 2;;]] - \[Lambda] * C[[All, ;;s+1]], c]
];

GlmA[HoldPattern[Glm[A_, __]]] := A;

GlmB[HoldPattern[Glm[_, B_, __]]] := B;

GlmU[HoldPattern[Glm[_, _, U_, __]]] := U;

GlmV[HoldPattern[Glm[_, _, _, V_, __]]] := V;

GlmQ[HoldPattern[Glm[_, _, _, _, Q_, __]]] := Q;

GlmC[HoldPattern[Glm[_, _, _, _, _, c_]]] := c;

GlmInternalStages[HoldPattern[Glm[A_, __]]] := Length[A];

GlmExternalStages[HoldPattern[Glm[_, B_, __]]] := Length[B];

GlmOrder[HoldPattern[Glm[_, _, _, _, Q_, __]]] := Dimensions[Q][[2]] - 1;

GlmType[HoldPattern[Glm[A_, __]]] := Which[
	TableauZerosQ[A], 3,
	TableauDiagonalQ[A], 4,
	TableauExplicitQ[A], 1,
	TableauSdirkQ[A], 2,
	True, Undefined
];

GlmTransform[HoldPattern[Glm[A_, B_, U_, V_, W_, c_]], T_?SquareMatrixQ] := With[{
		Tinv = Inverse[T]
	},
	Glm[A, T.B, U.Tinv, T.V.Tinv, T.W, c]
];

Glm /: Variables[HoldPattern[Glm[a___]]] := Variables[{a}];

Glm /: MakeBoxes[HoldPattern[Glm[A_List, B_List, U_List, V_List, _List, c_List]], format_] := GridBox[
	ArrayFlatten[{
		{Map[{MakeBoxes[#, format]} &, c], Map[MakeBoxes[#, format] &, A, {2}], Map[MakeBoxes[#, format] &, U, {2}]},
		{ConstantArray[{""}, Length[B]], Map[MakeBoxes[#, format] &, B, {2}], Map[MakeBoxes[#, format] &, V, {2}]}
	}],
	ColumnLines -> Join[{True}, ConstantArray[False, Length[c] - 1], {True, False}],
	RowLines -> Join[ConstantArray[False, Length[c] - 1], {True, False}]
];


(* ::Section:: *)
(*Catalog*)


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


(* ::Section:: *)
(*Error Handling*)


Glm::args = "Glm called with `1` arguments; must have an A, B, U, V, W, and c.";
Glm[args___] /; Length[{args}] =!= 6 := (Message[Glm::args, Length[{args}]]; $Failed);
Glm::squarematrix = "Glm `1` coefficients must be a square matrix.";
Glm[A_, __] /; !SquareMatrixQ[A] := (Message[Glm::squarematrix, "A"]; $Failed);
Glm::matrix = "Glm `1` coefficients must be a matrix.";
Glm[_, B_, __] /; !MatrixQ[B] := (Message[Glm::matrix, "B"]; $Failed);
Glm[_, _, U_, __] /; !MatrixQ[U] := (Message[Glm::matrix, "U"]; $Failed);
Glm[_, _, _, V_, __] /; !SquareMatrixQ[V] := (Message[Glm::squarematrix, "V"]; $Failed);
Glm[_, _, _, _, W_, _] /; !MatrixQ[W] := (Message[Glm::matrix, "W"]; $Failed);
Glm::vector = "Glm c coefficients must be a vector.";
Glm[_, _, _, _, _, c_] /; !VectorQ[c] := (Message[Glm::vector]; $Failed);
Glm::length = "Glm coefficients must have compatible lengths.";
Glm[A_, B_, U_, V_, W_, c_] /; !With[{
		s = Length[c],
		r = Length[V]
	},
	s === Length[A] && Dimensions[B] === {r, s} && Dimensions[U] === {s, r} && Length[W] === r
] := (Message[Glm::length]; $Failed);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
