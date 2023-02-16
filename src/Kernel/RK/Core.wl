(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


rkCompose[m_] := RK[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RKA[m[[i, 1]]],
		i > j, m[[j, 2]] * ConstantArray[RKB[m[[j, 1]]], RKStages[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RKB[First[#]] &, m]],
	Catenate[Map[Last[#] * RKC[First[#]] &, m] + FoldList[#1 + Last[#2] * Total[RKB[First[#2]]] &, 0, Most[m]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RKPairQ], Catenate[Map[Last[#] * RKBHat[First[#]] &, m]], Unevaluated[Sequence[]]]
];


rkB[False, None, True, _, bDO_, _] := bDO;
rkB[False, None, False, _, bDO_, _] := bDO /. \[FormalTheta] -> 1;
rkB[False, None, do_, _, bDO_, _] := bDO /. \[FormalTheta] -> do;
rkB[False, i_Integer, _, A_, _, _] := A[[i]];
rkB[True, _, _, _, _, bHat_] := bHat;


(* ::Section:: *)
(*Package Definitions*)


RK[s_Integer] := RK[
	Table[Subscript[\[FormalA], i, j], {i, s}, {j, s}],
	Table[Subscript[\[FormalB], i], {i, s}],
	Table[Subscript[\[FormalC], i], {i, s}]
];
RK[A_?SquareMatrixQ] := RK[A, Last[A]];
RK[A_?SquareMatrixQ, b_List] := RK[A, b, Total[A, {2}]];
RK[HoldPattern[RK[A_, b_, c_, ___]], bHat_] := RK[A, b, c, bHat];
AddComposition[RK, RKCompose, rkCompose];


RK /: x_ * HoldPattern[RK[A_, b_, c_]] := RK[A, x * b, c];
RK /: x_ * HoldPattern[RK[A_, b_, c_, bHat_]] := RK[A, x * b, c, x * bHat];


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


RKA[HoldPattern[RK[A_, __]]] := A;


Options[RKB] = {Embedded -> False, Stage -> None, DenseOutput -> False};
RKB[HoldPattern[RK[A_, b_, _, bHat_:True]], OptionsPattern[]] := With[{
		em = OptionValue[Embedded],
		s = OptionValue[Stage],
		do = OptionValue[DenseOutput]
	},
	rkB[em, s, do, A, b, bHat] /; BooleanQ[em] && (s === None || IntegerQ[s]) && bHat =!= em
];


RKDenseOutput[HoldPattern[RK[_, b_, __]]] := b;


RKC[HoldPattern[RK[_, _, c_, ___]]] := c;


RKBHat[HoldPattern[RK[_, _, _, bHat_]]] := bHat;


RKStages[HoldPattern[RK[_, _, c_, ___]]] := Length[c];


RKPrimary[HoldPattern[RK[A_, b_, c_, ___]]] := RK[A, b, c];


RKEmbedded[HoldPattern[RK[A_, _, c_, bHat_]]] := RK[A, bHat, c];


RKPairQ[HoldPattern[RK[_, _, _, _]]] := True;
RKPairQ[_] := False;


RKCollocation[c_List /; VectorQ[c] && DuplicateFreeQ[c]] := With[{
		V = SeriesVDM[Append[c, \[FormalTheta]], 1, Length[c]] . Inverse[SeriesVDM[c, 0, Length[c] - 1]]
	},
	RK[Most[V], Last[V], c]
];


RKExtrapolate[m_RK, steps:{__Integer?Positive}?DuplicateFreeQ, j:(_Integer?Positive):1] := With[{
		n = Length[steps],
		p = RKOrder[m]
	},
	Inner[#1 * m^#2 &, LinearSolve[Append[Table[1 / steps^(j * i + p), {i, 0, n - 2}], ConstantArray[1, n]], UnitVector[n, n]], steps, Plus]
];


RK /: Graph[HoldPattern[RK[A_, b_, _, bHat___]], opts:OptionsPattern[WeightedAdjacencyGraph]] := With[{
		K = Replace[Join[A, {b, bHat}], _?ZeroQ -> Infinity, {2}],
		s = Length[A]
	},
	WeightedAdjacencyGraph[
		PadRight[K, {Automatic, Length[K]}, Infinity],
		opts,
		DirectedEdges -> True,
		EdgeLabels -> "EdgeWeight",
		VertexLabels -> i_ :> Which[
			i <= s, StringForm["\!\(\*SubscriptBox[\(Y\), \(``\)]\)", i],
			i == s + 1, "\!\(\*SubscriptBox[\(y\), \(n+1\)]\)",
			True, "\!\(\*SubscriptBox[OverscriptBox[\(y\), \(^\)], \(n+1\)]\)"
		]
	]
];


RK /: Variables[HoldPattern[RK[a___]]] := Variables[{a}];


RK /: MakeBoxes[rk:HoldPattern[RK[A_List, b_List, c_List, bHat___]], format_] := With[{
		boxes = GridBox[
			ArrayFlatten[{
				{
					ArrayReshape[Map[MakeBoxes, c], {Length[c], 1}],
					Map[MakeBoxes, A, {2}]
				},
				{"", Map[MakeBoxes, {RKB[rk], bHat}, {2}]}
			}],
			ColumnLines -> {True, False},
			RowLines -> Append[ConstantArray[False, Length[c] - 1], True]
		]
	},
	InterpretationBox[boxes, rk]
];
