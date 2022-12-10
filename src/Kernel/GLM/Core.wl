(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


hankelBR[x_] := HankelMatrix[Append[ConstantArray[0, Length[x] - 2], First[x]], Most[x]];


glmComp[m_] := With[{
		n = Length[m]
	},
	GLM[
		ArrayFlatten[Table[Which[
			i == j, m[[i, 2]] * GLMA[m[[i, 1]]],
			i > j, m[[j, 2]] * GLMU[m[[i, 1]]] . Dot @@ Map[GLMV, m[[i - 1;;j + 1;;-1, 1]]] . GLMB[m[[j, 1]]],
			True, 0
		], {i, n}, {j, n}]],
		ArrayFlatten[{Table[m[[i, 2]] * Dot @@ Map[GLMV, m[[n;;i + 1;;-1, 1]]] . GLMB[m[[n + 1 - i, 1]]], {i, n}]}],
		ArrayFlatten[Table[{GLMU[m[[i, 1]]] . Dot @@ Map[GLMV, m[[1;;i - 1, 1]]]}, {i, n}]],
		Dot @@ Map[GLMV, m[[All, 1]]],
		GLMQ[m[[1, 1]]] . DiagonalMatrix[m[[1, 2]] ^ Range[0, GLMP[m[[1, 1]]]]],
		Catenate[Map[Last[#] * GLMC[First[#]] &, m] + FoldList[Plus, 0, m[[1 ;; -2, 2]]]]
	]
];


(* ::Section:: *)
(*Package Definitions*)


GLM[s_Integer?Positive, r_Integer?Positive, p_Integer?NonNegative] := GLM[
	Table[Subscript[\[FormalA], i, j], {i, s}, {j, s}],
	Table[Subscript[\[FormalB], i, j], {i, r}, {j, s}],
	Table[Subscript[\[FormalU], i, j], {i, s}, {j, r}],
	Table[Subscript[\[FormalV], i, j], {i, r}, {j, r}],
	Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}],
	Table[Subscript[\[FormalC], i], {i, s}]
];
GLM[rk_RK, p_Integer?NonNegative] := GLM[RKA[rk], {RKB[rk]}, ConstantArray[1, {RKStages[rk], 1}], {{1}}, {UnitVector[p + 1, 1]}, RKC[rk]];
GLM[rk_RK] := GLM[rk, RKOrder[rk]];
GLM[lmm_LMM] := With[{
		k = LMMSteps[lmm],
		a = LMMAlpha[lmm],
		b = LMMBeta[lmm],
		c = 1 - LengthWhile[Reverse[LMMBeta[lmm]], ZeroQ]
	},
	GLM[
		{{Last[b] / Last[a]}},
		Transpose[{Most[b]  - Last[b] / Last[a] * Most[a]}],
		{Append[ConstantArray[0, k - 1], 1 / Last[a]]},
		Transpose[CompanionMatrix[-Most[a] / Last[a]]],
		With[{v = SeriesVander[Range[c - k, c - 1], -1, k]}, hankelBR[b] . v[[All, ;;-2]] - hankelBR[a] . v[[All, 2;;]]],
		{c}
	]
];
AddComposition[GLM, GLMCompose, glmComp];


GLM /: Variables[HoldPattern[GLM[a___]]] := Variables[{a}];


GLM /: HoldPattern[x_ * GLM[A_, B_, U_, V_, Q_, c_]] := GLM[A, x * B, U, x * V, x * Q, c];


GLM /: HoldPattern[GLM[A1_, B1_, U1_, V1_, Q1_, c1_] + GLM[A2_, B2_, U2_, V2_, Q2_, c2_]] := With[{
		pMin = Min[Dimensions[Q1][[2]], Dimensions[Q2][[2]]]
	},
	GLM[BlockDiag[A1, A2], BlockDiag[B1, B2], BlockDiag[U1, U2], BlockDiag[V1, V2], ArrayFlatten[{{Q1[[All, ;;pMin]]}, {Q2[[All, ;;pMin]]}}], Join[c1, c2]]
];


GLMDIMSIM[A_?SquareMatrixQ, v_?VectorQ, c_?VectorQ] /; Length[A] === Length[v] === Length[c] := With[{
		C = SeriesVander[c, -1, Length[c]],
		s = Length[c]
	},
	With[{
			Q = C[[All, 2;;]] - A . C[[All, ;;-2]],
			mu = ToeplitzMatrix[Join[{1, 1}, ConstantArray[0, s - 1]], 1 / Range[s]!]
		},
		GLMDIMSIM[A, (Q . mu - ConstantArray[v . Q[[All, 2;;]], s]) . Inverse[C[[All, 2;;-2]]], v, Q, c]
	]
];
GLMDIMSIM[A_?SquareMatrixQ, B_?MatrixQ, v_?VectorQ, Q_?MatrixQ, c_?VectorQ] := GLM[A, B, IdentityMatrix[{Length[c], Length[v]}], ConstantArray[v, Length[v]], Q, c];


GLMPeer[B_?SquareMatrixQ, A_?SquareMatrixQ, R_?SquareMatrixQ, c_?VectorQ, p_Integer?NonNegative] := With[{
		BA = ArrayFlatten[{{B, A}}]
	},
	GLM[R, ArrayFlatten[{{R}, {IdentityMatrix[Length[R]]}}], BA, KroneckerProduct[{{1}, {0}}, BA], ArrayFlatten[{{SeriesVander[c - 1, 0, p]}, {SeriesVander[c - 1, -1, p - 1]}}], c]
];


GLMOneLeg[a_?VectorQ, b_?VectorQ, p_Integer?NonNegative] := With[{
		q = Length[a] - 1,
		aq = Last[a],
		bq = Last[b]
	},
	GLM[{{bq / aq}}, Append[ConstantArray[{0}, q - 1], {1 / aq}], {Most[b] - bq / aq * Most[a]}, CompanionMatrix[-Most[a] / aq], SeriesVander[Range[1 - q, 0], 0, p], {b . Range[1 - q, 1]}]
];


GLMParallelEnsemble[c_?VectorQ, \[Lambda]_:0] := With[{
		s = Length[c],
		i = Range[2, Length[c]],
		I = IdentityMatrix[Length[c]],
		C = SeriesVander[c, -1, Length[c]]
	},
	GLM[\[Lambda] * I, C[[All, 2;;s+1]] . ToeplitzMatrix[UnitVector[s, 1], Prepend[(1 - \[Lambda] * i) / i!, 1]] . Inverse[C[[All, 2;;s+1]]], I, I, C[[All, 2;;]] - \[Lambda] * C[[All, ;;s+1]], c]
];


GLMA[HoldPattern[GLM[A_, __]]] := A;


GLMB[HoldPattern[GLM[_, B_, __]]] := B;


GLMU[HoldPattern[GLM[_, _, U_, __]]] := U;


GLMV[HoldPattern[GLM[_, _, _, V_, __]]] := V;


GLMQ[HoldPattern[GLM[_, _, _, _, Q_, __]]] := Q;


GLMC[HoldPattern[GLM[_, _, _, _, _, c_]]] := c;


GLMInternalStages[HoldPattern[GLM[A_, __]]] := Length[A];


GLMExternalStages[HoldPattern[GLM[_, B_, __]]] := Length[B];


GLMP[HoldPattern[GLM[_, _, _, _, Q_, __]]] := Dimensions[Q][[2]] - 1;


GLMTransform[HoldPattern[GLM[A_, B_, U_, V_, W_, c_]], T_?SquareMatrixQ] := With[{
		Tinv = Inverse[T]
	},
	GLM[A, T . B, U . Tinv, T . V . Tinv, T . W, c]
];


GLM /: MakeBoxes[HoldPattern[GLM[A_List, B_List, U_List, V_List, _List, c_List]], format_] := With[{
		s = Length[c]
	},
	TagBox[GridBox[
		Map[If[# === "", #, MakeBoxes[#, format]] &, ArrayFlatten[{{ArrayReshape[c, {s, 1}], A, U}, {"", B, V}}], {2}],
		ColumnLines -> Join[{True}, ConstantArray[False, s - 1], {True, False}],
		RowLines -> Join[ConstantArray[False, s - 1], {True, False}]
	], Grid]
];
