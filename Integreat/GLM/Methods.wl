(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`GLM`Methods`"];

GLM::usage =
	"GLM[s, r, p] constructs a generic general linear method with s internal stages, r external stages, and order p.\n" <>
	"GLM[rk, p] converts a Runge-Kutta method rk into a general linear method of order p.\n" <>
	"GLM[rk] converts a Runge-Kutta method rk into a general linear method.\n" <>
	"GLM[lmm] converts a linear multistep method lmm into a general linear method.";
GLMCompose::usage =
	"GLMCompose[glm1, \[Ellipsis], glmm] creates a general linear method from a step of glm1, \[Ellipsis], glmm in sequence using a step size h/m.\n" <>
	"GLMCompose[{glm1, w1}, \[Ellipsis], {glmm, wm}] composes glm1, \[Ellipsis], glmm using step sizes w1*h, \[Ellipsis], wm*h, respectively.\n" <>
	"glm1[glm2] composes a half step of glm1 with a half step of glm2.\n" <>
	"glm^p composes p steps of glm."
GLMDIMSIM::usage =
	"GLMDIMSIM[A, B, v, Q, c] constructs a diagonally implicit multistage integration method from coefficients A, B, v (repeated row of V), Q, and abscissae c.\n" <>
	"GLMDIMSIM[A, v, c] uses A, v, and c then deduces other coefficients from the assumption p=q=r=s.";
GLMPeer::usage = "GLMPeer[B, A, R, c, p] constructs an order p peer method from coefficients B, A, R, and abscissae c.";
GLMOneLeg::usage = "GLMOneLeg[{\[Alpha]0, \[Alpha]1, \[Ellipsis], \[Alpha]k}, {\[Beta]0, \[Beta]1, \[Ellipsis], \[Beta]k}, p] creates a k-step one-leg method.";
GLMParallelEnsemble::usage =
	"GLMParallelEnsemble[c, \[Lambda]] constructs a parallel ensemble method with abscissae c and A=\[Lambda]*I.\n" <>
	"GLMParallelEnsemble[c] constructs an explicit parallel ensemble method.";
GLMA::usage = "GLMA[glm] returns the A coefficient matrix of glm.";
GLMB::usage = "GLMB[glm] returns the B coefficient matrix of glm.";
GLMU::usage = "GLMU[glm] returns the U coefficient matrix of glm.";
GLMV::usage = "GLMV[glm] returns the V coefficient matrix of glm.";
GLMQ::usage = "GLMQ[glm] returns the Q coefficient matrix of glm.";
GLMC::usage = "GLMC[glm] returns the abscissae of glm.";
GLMInternalStages::usage = "GLMInternalStages[glm] returns the number of internal stages in glm.";
GLMExternalStages::usage = "GLMExternalStages[glm] returns the number of external stages in glm.";
GLMP::usage = "GLMP[glm] returns the order to which external stages are expanded for glm.";
GLMType::usage = "GLMType[glm] returns the type number of glm.";
GLMTransform::usage = "GLMTransform[glm, T] Transforms glm into an equivalent form with the matrix T.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Tableaus`",
	"Integreat`RK`Methods`",
	"Integreat`RK`OrderConditions`",
	"Integreat`LMM`Methods`",
	"Integreat`Internal`MathUtils`",
	"Integreat`Internal`Composition`"
}];

TypeToTableau[type_] := Switch[type, 1, TableauExplicit, 2, TableauSDIRK, 3, TableauZeros, 4, TableauDiagonal, _, TableauFIRK];

HankelBR[x_] := HankelMatrix[Append[ConstantArray[0, Length[x] - 2], First[x]], Most[x]];

GLMComp[m_] := With[{
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


GLM[s_Integer?Positive, r_Integer?Positive, p_Integer?NonNegative, OptionsPattern[{Type -> 0}]] := GLM[TypeToTableau[OptionValue[Type]][s], TableauFIRK[{r, s}, \[FormalB]], TableauFIRK[{s, r}, \[FormalU]], TableauFIRK[r, \[FormalV]], Table[Subscript[\[FormalQ], i, j], {i, r}, {j, 0, p}], Table[Subscript[\[FormalC], i], {i, s}]];
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
		With[{v = SeriesVander[Range[c - k, c - 1], -1, k]}, HankelBR[b] . v[[All, ;;-2]] - HankelBR[a] . v[[All, 2;;]]],
		{c}
	]
];

AddComposition[GLM, GLMCompose, GLMComp];

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

GLMType[HoldPattern[GLM[A_, __]]] := Which[
	TableauZerosQ[A], 3,
	TableauDiagonalQ[A], 4,
	TableauExplicitQ[A], 1,
	TableauSDIRKQ[A], 2,
	True, Undefined
];

GLMTransform[HoldPattern[GLM[A_, B_, U_, V_, W_, c_]], T_?SquareMatrixQ] := With[{
		Tinv = Inverse[T]
	},
	GLM[A, T . B, U . Tinv, T . V . Tinv, T . W, c]
];

GLM /: Variables[HoldPattern[GLM[a___]]] := Variables[{a}];

GLM /: MakeBoxes[HoldPattern[GLM[A_List, B_List, U_List, V_List, _List, c_List]], format_] := With[{
		s = Length[c]
	},
	TagBox[GridBox[
		Map[If[# === "", #, MakeBoxes[#, format]] &, ArrayFlatten[{{ArrayReshape[c, {s, 1}], A, U}, {"", B, V}}], {2}],
		ColumnLines -> Join[{True}, ConstantArray[False, s - 1], {True, False}],
		RowLines -> Join[ConstantArray[False, s - 1], {True, False}]
	], Grid]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
