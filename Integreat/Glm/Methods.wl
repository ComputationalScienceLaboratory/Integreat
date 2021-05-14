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
		With[{v = SeriesVander[Range[c - k, c - 1], -1, k]}, HankelBR[b] . v[[All, ;;-2]] - HankelBR[a] . v[[All, 2;;]]],
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
			Q = C[[All, 2;;]] - A . C[[All, ;;-2]],
			mu = ToeplitzMatrix[Join[{1, 1}, ConstantArray[0, s - 1]], 1 / Range[s]!]
		},
		GlmDimsim[A, (Q . mu - ConstantArray[v . Q[[All, 2;;]], s]) . Inverse[C[[All, 2;;-2]]], v, Q, c]
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
	Glm[{{bq / aq}}, Append[ConstantArray[{0}, q - 1], {1 / aq}], {Most[b] - bq / aq * Most[a]}, CompanionMatrix[-Most[a] / aq], SeriesVander[Range[1 - q, 0], 0, p], {b . Range[1-q,1]}]
];

GlmParallelEnsemble[c_?VectorQ, \[Lambda]_:0] := With[{
		s = Length[c],
		i = Range[2, Length[c]],
		I = IdentityMatrix[Length[c]],
		C = SeriesVander[c, -1, Length[c]]
	},
	Glm[\[Lambda] * I, C[[All, 2;;s+1]] . ToeplitzMatrix[UnitVector[s, 1], Prepend[(1 - \[Lambda] * i) / i!, 1]] . Inverse[C[[All, 2;;s+1]]], I, I, C[[All, 2;;]] - \[Lambda] * C[[All, ;;s+1]], c]
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
	Glm[A, T . B, U . Tinv, T . V . Tinv, T . W, c]
];

Glm /: Variables[HoldPattern[Glm[a___]]] := Variables[{a}];

Glm /: MakeBoxes[HoldPattern[Glm[A_List, B_List, U_List, V_List, _List, c_List]], format_] := With[{
		s = Length[c]
	},
	TagBox[GridBox[
		Map[If[# === "", #, MakeBoxes[#, format]] &, ArrayFlatten[{{ArrayReshape[c, {s, 1}], A, U}, {"", B, V}}], {2}],
		ColumnLines -> Join[{True}, ConstantArray[False, s - 1], {True, False}],
		RowLines -> Join[ConstantArray[False, s - 1], {True, False}]
	], Grid]
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
	]},
	{"IRKSM 1", Glm[
		{{0, 0},{4/3, 0}},
		{{17/16, 3/16},{1/4, 3/4}},
		{{1,0},{1 ,-1/3 }},
		{{1 ,-1/4},{0, 0}},
		{{1,0},{0,1}},
		{0,1}
	]},
	{"IRKSM 2", Glm[
		{{0, 0, 0},{279/574, 0, 0},{81/14105 ,1968/2015,0}},
		{{608663/499968, -2009/35712, 455/2304},{-113815/71424, 85567/35712, 455/2304},{17/24,-41/12,65/24}},
		{{1,0, 0},{1 ,4/287 ,1/8},{1, 8/455, 47/4030}},
		{{1 ,-241/672, 41/124},{0, 0,-1177/2976},{0, 0, 0}},
		{{1,0,0},{0,1,0},{0,0,1}},
		{0,1/2,1}
	]},
	{"IRKSM 3", Glm[
		{{0, 0, 0,0 },{41/324, 0, 0,0},{-1147432/1657179 ,20412/20459,0,0},{-884267971/603213156, 195810716/150803289,4990/7371,0}},
		{{1368599/1033200, 443273/1033200, -13343/25200,117/400},{-11208301/1033200, 12486353/1033200, -13343/25200, 117/400},{73403/8400,-169549/8400,93689/8400, 117/400},{-171/20, 693/20, -873/20, 351/20}},
		{{1,0, 0,0},{1 ,67/324,1/18, 1/162},{1, 14606/40419, -20318/184131, -10018/1657179},{1, 343643/700596, -5517035/14362218, -151762489/2714459202}},
		{{1 ,-31/60, 1027/2460, 53117/464940},{0, 0,-7301/2460, -4649/23247},{0, 0, 0, -1903/3780},{0,0,0,0}},
		{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},
		{0,1/3,2/3,1}
	]},
	{"IRKSM 4", Glm[
		{{0, 0, 0,0 ,0},{511/11776, 0, 0,0,0},{-3101781017/7028858144 ,5166720/9550079,0,0,0},{-83353109372729587/145067197462387200, -1001725806316/12318885654075,4242403/5159700,0,0},{-41921552692643159297/191921635567777915800,-6767984667820805501/4172209468864737300,696338101127/436876958700,165375/338684,0}},
		{{172958424193/101787367500, -28218803078/25446841875, 202394939/99596250,-2106322/1276875,2984/5625},{-1692541208191/50893683750, 848483163322/25446841875, 202394939/99596250, -2106322/1276875,2984/5625},{996053384/16599375,-689424962/5533125,1090797904/16599375, -2106322/1276875,2984/5625},{-78390032/1276875, 254922728/1276875, -91332664/425625, 96787928/1276875,2984/5625},{7136/75,-33344/75, 19072/25,-42944/75, 11936/75}},
		{{1,0, 0,0,0},{1 ,2433/11776,1/32, 1/384, 1/6144},{1, 5505879/13755104, -783361/76400632, 1799999/458403792,4383359/3667230336},{1, 6294407001/10784001536, -73807449337/673853574240, -47184261087409/1576817363721600, -94210576904551/25229077819545600},{1,47665961355/62850508952,-8094188961413/31418422898940,-17528291232939607/147038219167039200,-5780449858046290723/267021406007343187200}},
		{{1 ,-151/300, 71723/153300, 52375073/358722000,2425799989/81429894000},{0, 0,-1172011/153300, -214263247/358722000,5298687019/81429894000},{0, 0, 0, -9131/2925,-2196853/4249440},{0,0,0,0,-63679/136200},{0,0,0,0,0}},
		{{1,0,0,0,0},{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0},{0,0,0,0,1}},
		{0,1/4, 1/2, 3/4, 1}
	]},
	{"DIIRKSM 1", Glm[
		{{2/5, 0},{12/25, 2/5}},
		{{12/25, 2/5},{0,1}},
		{{1,1/10},{1 ,3/25}},
		{{1 ,3/25},{0, 0}},
		{{1,0},{0,1}},
		{1/2,1}
	]},
	{"DIIRKSM 2", Glm[
		{{4/9, 0, 0},{29/72, 4/9, 0},{1295/522 ,-248/261,4/9}},
		{{9827/4698, -644/783, 44/81},{292/261, -98/261, 13/18}, {-3/58,1/29,1/4}},
		{{1,-4/9, 0},{1 ,-25/72 ,-7/36},{1, -509/522, 277/261}},
		{{1 ,-3817/4698, 1729/2349},{0, -27/58,27/29},{0, -27/116, 27/58}},
		{{1,0,0},{0,1,0},{0,0,1/2}},
		{0,1/2,1}
	]},
	{"DIIRKSM 3", Glm[
		{{1/4, 0, 0, 0},{5/6, 1/4, 0,0},{109057/33000 ,1701/2200,1/4, 0},{368999/154000, 21071/30800, 11/56, 1/4}},
		{{8885797/5832000, 694913/1166400, 7543/23328, 637/3888},{-102869/72900, -4013/7290, 245/1458, 917/972},{75523/8100, -149/810, -767/324, 133/54},{70087/6750, 1223/1350, -56/27, 14/9}},
		{{1,-1/4, 0, 0},{1 ,-3/4 ,-1/18, -5/108},{1, -20137/5500, -4003/9900, -17509/59400},{1, -24319/9625, -3357/15400, -22171/92400}},
		{{1 ,-260267/162000, -90971/583200, -424213/3499200},{0, 7493/4050,1867/7290,5651/43740},{0, -2078/225, -262/405, -1423/1215},{0, -4039/375, -641/675, -4873/4050}},
		{{1,0,0,0},{0,1,0,0},{0,0,1/2,0},{0,0,0,1/6}},
		{0,1/3,2/3,1}
	]},
	{"DIIRKSM 4", Glm[
		{{1/4, 0, 0, 0,0},{47/64, 1/4, 0,0,0},{24197/14476,678/3619,1/4, 0,0},{7102302807/1544183872, 987465/24127873, 10395/26668, 1/4, 0},{-117251104/55207845, \[Minus]27818059/55207845, 7255/6102, -59/135, 1/4}},
		{{825449/430191, -1889207/860382, 19916/9153, -59/162, 1/6},{1422203/1433970,528694/716985,\[Minus]4249/3051,118/135,5/6},{\[Minus]18698713/716985, 30670112/716985, -99890/3051, 944/135, 2},{\[Minus]97429762/2150955, 146725568/2150955, \[Minus]445028/9153, 3776/405, 2},{\[Minus]13844474/716985, 19904536/716985, \[Minus]58216/3051, 472/135, 2/3}},
		{{1,-1/4, 0, 0, 0},{1 ,-47/64 ,-1/16, -1/32, -3/256},{1, \[Minus]11645/7238, -339/3619,- 5653/57904, -4297/57904},{1, -6995320711/1544183872, -85994121/386045968, -57910455/193022984,\[Minus]1871076171/6176735488 },{1, 579853229/220831380, 12065149/55207845, 9336821/49073640, 15415373/88332552}},
		{{1 ,-603461/860382, 116111/860382, -40393/382392,-19249/6883056},{0, \[Minus]748481/716985,33116/716985,-21913/318660, -90679/573588},{0, 5055197/716985, \[Minus]1532237/716985, 276353/159330, -14840/143397},{0, 30929528/2150955, \[Minus]7466528/2150955, 703186/238995, 134986/430191},{0,4635916/716985,\[Minus]987676/716985,96032/79665,34232/143397}},
		{{1,0,0,0,0},{0,1,0,0,0},{0,0,1/2,0,0},{0,0,0,1/6,0},{0,0,0,0,1/24}},
		{0,1/4,1/2,3/4,1}
	]},
	{"DIIRKSM 5", Glm[
		{{1/3, 0, 0,0,0,0},{-2176/28125, 1/3, 0,0,0,0},{-1834/9375 ,5/32,1/3,0,0,0},{-8158/28125, 235/624,-10/117, 1/3, 0,0},{-275806/253125,21175/5616, -590/117,65/27, 1/3, 0},{689/972, -22225/5184, 1175/162, -1025/324, -25/36, 1/3}},
		{{689/972, -22225/5184, 1175/162, -1025/324, -25/36, 1/3},{0, 0, 0,0,0,1},{101/24,-25/2,25/2, -25/3,25/8,3/2},{487/36, -4775/96, 3475/54, -2825/72, 25/4, 3/2},{589/32, -19575/256, 16175/144,-2225/32, 225/32, 9/8},{15,-36095/512, 355/3,-1215/16,45/8,27/40}},
		{{1,2/3, 1/3, 0, -1/3, -2/3},{1 ,15301/28125,7352/28125,976/9375,1024/28125,896/28125},{1, 91813/300000, 3797/37500,893/6250,2851/9375,4874/9375},{1,127913/1950000,-1493/56250,17479/121875,147376/365625,256664/365625},{1,-9417977/52650000,90349/506250,316003/1096875,1211077/3290625,2248418/3290625},{1,13219/15552, -233/1944,-41/324, 43/243, 68/243}},
		{{1 ,13219/15552, -233/1944, -41/324, 43/243, 68/243},{0, 0,0,0,0,0},{0, -1/2,0,0,0,0},{0, 2891/864,43/36,0,0,0},{0, 16375/2304,125/96,-9/16,0,0},{0, 52241/7680,-657/320,-591/160,-13/10,0}},
		{{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1/2,0,0,0},{0,0,0,1/6,0,0},{0,0,0,0,1/24,0},{0,0,0,0,0,1/120}},
		{1, 4/5,3/5, 2/5,1/5, 1}
	]},
	{"DIIRKSM 6", Glm[
		{{1/4, 0, 0,0,0,0,0},{-51875/2612736, 1/4, 0,0,0,0,0},{767/46656 ,-42/625,1/4,0,0,0,0},{-1074245/10450944, 3633/10000,-3/8, 1/4, 0,0,0},{-339338065/2842656768,226473/544000, -7215/17408,45/544, 1/4, 0,0},{-576409/194969600, 822537/27200000, -747/10880, -3681/10880, 153/200, 1/4,0},{-3577/5120, 10071/5000, -2187/1024, -297/64, 81/10, 5/2,1/4}},
		{{-3577/5120, 10071/5000, -2187/1024, -297/64, 81/10, 5/2,1/4},{0, 0, 0,0,0,0,1},{107/20,-18,45/2, 45/4,-18/5,-20,2},{43/2, -2088/25, 441/4, -63/2, 72, -160/3, 8/3},{599/15, -21456/125, 7785/32,-333/2, 1152/5, -80, 8/3},{3211/75,-631104/3125, 3129/10,-1668/5,9408/25,-256/3, 32/15},{827/30, -453024/3125, 507/2, -432, 2112/5,-640/9,64/45}},
		{{1,3/4, 1/2, 1/4,0, -1/4, -1/2},{1 ,1575971/2612736,414755/1306368,102275/870912,-11125/653184,-265625/2612736,-194375/1306368},{1, 13630177/29160000, 554717/2916000,20857/388800,-523/58320,-1651/46656,-1081/23328},{1,1292145653/6531840000,29081713/653184000,452573/87091200,-131447/13063680,-31487/10450944,168355/5225472},{1,-17242989907/355332096000,-468580247/35533209600,-1662667/4737761280,-7876607/710664192,-10125667/2842656768,53541623/1421328384},{1,-3297390027/24371200000, 42912433/2437120000,23336279/974848000, -163127/48742400, -150259/7798784,-476061/19496960},{1,\[Minus]10973/2500,\[Minus]45973/64000,-149/25600,-63/1280,-31/1024,83/512}},
		{{1,\[Minus]10973/2500,\[Minus]45973/64000,-149/25600,-63/1280,-31/1024,83/512},{0, 0,0,0,0,0,0},{0, 1/2,0,0,0,0,0},{0, \[Minus]11419/300,-29/5,0,0,0,0},{0, \[Minus]392533/4000,\[Minus]1859/200,69/40,0,0,0},{0, \[Minus]2124001/18750,7368/625,1567/125,148/75,0,0},{0,\[Minus]1597784/28125,119899/1875, 29387/750, 818/75, 3/2,0}},
		{{1,0,0,0,0,0,0},{0,1,0,0,0,0,0},{0,0,1/2,0,0,0,0},{0,0,0,1/6,0,0,0},{0,0,0,0,1/24,0,0},{0,0,0,0,0,1/120,0},{0,0,0,0,0,0,1/720}},
		{1, 5/6,2/3, 1/3,1/6, 1/2,1}
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
