(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["CSL`OdeUtils`RungeKutta`Methods`"];
CSL`OdeUtils`RungeKutta`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RungeKutta::usage = "Constructs a Runge-Kutta method";
RungeKuttaType::usage = "A string representation of the type of Runge-Kutta method";
RungeKuttaPrimary::usage = "Removes the embedded method from a Runge-Kutta method";
RungeKuttaEmbedded::usage = "Gets the embedded Runge-Kutta method";
RungeKuttaPairQ::usage = "Returns True if m is a Runge-Kutta method with an embedded method";
RungeKuttaCollocation::usage = "Constructs a collocated Runge-Kutta method";
RungeKuttaCompose::usage = "Builds a single Runge-Kutta method out of sub-methods";
RungeKuttaA::usage = "Gets the A coefficients of a Runge-Kutta method";
RungeKuttaDenseOutput::usage = "Gets the b coeffients as a function of \[FormalTheta] for a Runge-Kutta method";
RungeKuttaB::usage = "Gets the b coefficients of a Runge-Kutta method";
RungeKuttaC::usage = "Gets the c coefficients of a Runge-Kutta method";
RungeKuttaBHat::usage = "Gets the embedded coefficients of a Runge-Kutta method";
RungeKuttaStages::usage = "The number of stages in a Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];
Needs["CSL`OdeUtils`Internal`Composition`"];

TypeToTableau[type_] := Switch[type, "ERK", TableauExplicit, "ESDIRK", TableauEsdirk, "SDIRK", TableauSdirk, "DIRK", TableauDirk, _, TableauFirk];

LagrangeBasis[t_, c_, i_] := Product[(t - c[[l]]) / (c[[i]] - c[[l]]), {l, DeleteCases[Range[Length[c]], i]}];

RkCompose[m_] := RungeKutta[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RungeKuttaA[m[[i, 1]]],
		i > j, m[[j, 2]] * ConstantArray[RungeKuttaB[m[[j, 1]]], RungeKuttaStages[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RungeKuttaDenseOutput[First[#]] &, m]],
	Catenate[Map[Last[#] * RungeKuttaC[First[#]] &, m] + FoldList[#1 + Last[#2] * Total[RungeKuttaB[First[#2]]] &, 0, Most[m]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RungeKuttaPairQ], Catenate[Map[Last[#] * RungeKuttaBHat[First[#]] &, m]], Unevaluated[Sequence[]]]
];

Erk2Fam[c2_, d1_] := RungeKutta[{{0,0},{c2,0}}, {(2 c2-1)/(2 c2),1/(2 c2)}, {0,c2}, {d1, 1-d1}];
Erk3Fam[c2_, c3_, d1_] := RungeKutta[
	{{0,0,0},{c2,0,0},{(c3 (3 (-1+c2) c2+c3))/(c2 (-2+3 c2)),((c2-c3) c3)/(c2 (-2+3 c2)),0}},
	{(2-3 c2-3 c3+6 c2 c3)/(6 c2 c3),(2-3 c3)/(6 c2^2-6 c2 c3),(-2+3 c2)/(6 c2 c3-6 c3^2)},
	{0,c2,c3},
	{d1,(1-2 c3+2 c3 d1)/(2 (c2-c3)),(1-2 c2+2 c2 d1)/(2 (-c2+c3))}
];


(* ::Section:: *)
(*Package Definitions*)


RungeKutta[s_Integer, OptionsPattern[{Type -> "FIRK"}]] := RungeKutta[TypeToTableau[OptionValue[Type]][s], Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];
RungeKutta[A_?TableauQ] := RungeKutta[A, Last[A]];
RungeKutta[A_?TableauQ, b_] := RungeKutta[A, b, Total[A, {2}]];
RungeKutta[HoldPattern[RungeKutta[A_, b_, c_, ___]], bHat_] := RungeKutta[A, b, c, bHat];

AddComposition[RungeKutta, RungeKuttaCompose, RkCompose];

RungeKutta /: HoldPattern[x_ * RungeKutta[A_, b_, c_]] := RungeKutta[A, x * b, c];
RungeKutta /: HoldPattern[x_ * RungeKutta[A_, b_, c_, bHat_]] := RungeKutta[A, x * b, c, x * bHat];

RungeKutta /: HoldPattern[RungeKutta[A1_, b1_, c1_, bHat1_] + RungeKutta[A2_, b2_, c2_, bHat2_]] := RungeKutta[ArrayFlatten[{{A1, 0}, {0, A2}}], Join[b1, b2], Join[c1, c2], Join[bHat1, bHat2]];
RungeKutta /: HoldPattern[RungeKutta[A1_, b1_, c1_, ___] + RungeKutta[A2_, b2_, c2_, ___]] := RungeKutta[ArrayFlatten[{{A1, 0}, {0, A2}}], Join[b1, b2], Join[c1, c2]];

RungeKuttaType[HoldPattern[RungeKutta[A_, __]]] := Which[TableauExplicitQ[A], "ERK", TableauEsdirkQ[A], "ESDIRK", TableauSdirkQ[A], "SDIRK", TableauDirkQ[A], "DIRK", True, "FIRK"];

RungeKuttaPrimary[HoldPattern[RungeKutta[A_, b_, c_, ___]]] := RungeKutta[A, b, c];

RungeKuttaEmbedded[HoldPattern[RungeKutta[A_, _, c_, bHat_]]] := RungeKutta[A, bHat, c];

RungeKuttaPairQ[HoldPattern[RungeKutta[_, _, _, _]]] := True;
RungeKuttaPairQ[_] := False;

RungeKuttaCollocation[c_List?VectorQ] := RungeKutta[
	Table[Integrate[LagrangeBasis[t, c, j], {t, 0, c[[i]]}], {i, Length[c]}, {j, Length[c]}],
	Table[Integrate[LagrangeBasis[t, c, i], {t, 0, \[FormalTheta]}], {i, Length[c]}],
	c
];

RungeKuttaA[HoldPattern[RungeKutta[A_, __]]] := A;

RungeKuttaDenseOutput[RungeKutta[_, b_, __]] := b;

RungeKuttaB[HoldPattern[RungeKutta[_, b_, _, bHat_:Null]], embedded_:False]:= If[embedded, bHat, b /. \[FormalTheta] -> 1];

RungeKuttaC[HoldPattern[RungeKutta[_, _, c_, ___]]] := c;

RungeKuttaBHat[HoldPattern[RungeKutta[_, _, _, bHat_]]] := bHat;

RungeKuttaStages[HoldPattern[RungeKutta[A_, __]]] := Length[A];

RungeKutta /: Length[HoldPattern[RungeKutta[A_, __]]] := Length[A];

RungeKutta /: Variables[HoldPattern[RungeKutta[a___]]] := Variables[{a}];

RungeKutta /: MakeBoxes[HoldPattern[RungeKutta[A_, b_, c_, bHat_:Nothing]], format_] := GridBox[
	Join[Map[MakeBoxes[#, format] &, MapThread[Prepend, {A, c}], {2}], Map[Prepend[#, ""] &, Map[MakeBoxes[#, format] &, {b /. \[FormalTheta] -> 1, bHat}, {2}]]],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[A] - 1], True]
];


(* ::Section:: *)
(*Catalog*)


AddCatalog[
	RungeKutta,
	(*Explicit*)
	{"Euler", "Euler's Method", "Forward Euler", "Explicit Euler", RungeKutta[{{0}}, {\[FormalTheta]}]},
	{"ERK 2(1)2P", "2 Stage Explicit, Order 2", Erk2Fam[Subscript[\[FormalC], 2], Subscript[\[FormalD], 1]]},
	{"Heun", "Heun's Method", "Explicit Trapezoid", Erk2Fam[1, 1]},
	{"Ralston's 2nd Order Method", "Ralston 2", Erk2Fam[2/3, 1]},
	{"ERK 3(2)3P", "3 Stage Explicit, Order 3", Erk3Fam[Subscript[\[FormalC], 2], Subscript[\[FormalC], 3], Subscript[\[FormalD], 1]]},
	{"Ralston's 3rd Order Method", "Ralston 3", Erk3Fam[1/2, 3/4, 1/40]},
	{"Bogacki-Shampine", "ode23", RungeKutta[RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,3/4,0,0},{2/9,1/3,4/9,0}}], {7/24,1/4,1/3,1/8}]},
	{"RK4", "Classiscal", "Classical Runge-Kutta Method", "The Runge-Kutta Method", RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,1/2,0,0},{0,0,1,0}}, {1/6,1/3,1/3,1/6}]},
	{"ERK 4(3)5", RungeKutta[
		RungeKutta[{{0,0,0,0,0},{2/5,0,0,0,0},{-3/20,3/4,0,0,0},{19/44,-15/44,10/11,0,0},{11/72,25/72,25/72,11/72,0}}],
		{1251515/8970912,3710105/8970912,2519695/8970912,61105/8970912,119041/747576}
	]},
	{"Fehlberg's 4th Order Method", "Felhberg 4", RungeKutta[
		RungeKutta[
			{{0,0,0,0,0,0},{1/4,0,0,0,0,0},{3/32,9/32,0,0,0,0},{1932/2197,-7200/2197,7296/2197,0,0,0},{439/216,-8,3680/513,-845/4104,0,0},{-8/27,2,-3544/2565,1859/4104,-11/40,0}},
			{25/216,0,1408/2565,2197/4104,-1/5,0}
		],
		{16/135,0,6656/12825,28561/56430,-9/50,2/55}
	]},
	{"RKDP", "DOPRI", "Dormand-Prince", RungeKutta[
		RungeKutta[{{0,0,0,0,0,0,0},{1/5,0,0,0,0,0,0},{3/40,9/40,0,0,0,0,0},{44/45,-56/15,32/9,0,0,0,0},{19372/6561,\[Minus]25360/2187,64448/6561,\[Minus]212/729,0,0,0},{9017/3168,\[Minus]355/33,46732/5247,49/176,\[Minus]5103/18656,0,0},{35/384,0,500/1113,125/192,\[Minus]2187/6784,11/84,0}}],
		{5179/57600,0,7571/16695,393/640,\[Minus]92097/339200,187/2100,1/40}
	]},
	{"BS(4,5)", RungeKutta[
		RungeKutta[{{0,0,0,0,0,0,0,0},{1/6,0,0,0,0,0,0,0},{2/27,4/27,0,0,0,0,0,0},{183/1372,-162/343,1053/1372,0,0,0,0,0},{68/297,-4/11,42/143,1960/3861,0,0,0,0},{597/22528,81/352,63099/585728,58653/366080,4617/20480,0,0,0},{174197/959244,-30942/79937,8152137/19744439,666106/1039181,-29421/29068,482048/414219,0,0},{587/8064,0,4440339/15491840,24353/124800,387/44800,2152/5985,7267/94080,0}}],
		{73229/979776,0,2150079/7745920,28742371/136468800,-2537/201600,1626736/4363065,180606751/2183267520,-3293/556956}
	]},
	
	(*Implicit*)
	{"Backward Euler", "Implicit Euler", RungeKutta[{{1}}]},
	{"Implicit Midpoint", RungeKutta[{{1/2}}, {1}]},
	{"Implicit Trapezoidal", RungeKutta[{{0, 0},{1/2,1/2}}]},
	{"SDIRK 2(1)2", RungeKutta[RungeKutta[{{1-1/Sqrt[2],0},{1/Sqrt[2],1-1/Sqrt[2]}}], {3/5,2/5}]},
	{"SDIRK 2()2N", RungeKutta[{{1/4,0},{1/2,1/4}}, {1/2,1/2}]},
	{"SDIRK 2(1)2A", RungeKutta[RungeKutta[{{1/4,0},{7/12,1/4}},{4/7,3/7}], {52/87,35/87}]},
	{"ESDIRK 2(1)3", RungeKutta[RungeKutta[{{0,0,0},{1-Sqrt[2]/2,1-Sqrt[2]/2,0},{Sqrt[2]/4,Sqrt[2]/4,1-Sqrt[2]/2}}], {3/10,3/10,2/5}]},
	{"ESDIRK 2()3A", RungeKutta[{{0,0,0},{1/4,1/4,0},{1/8 (1+Sqrt[2]),1/8 (1+Sqrt[2]),1/4}},{1-1/Sqrt[2],1-1/Sqrt[2],-1+Sqrt[2]}]},
	{"SDIRK 3()2", RungeKutta[{{1/6*(3+Sqrt[3]),0},{-1/Sqrt[3],1/6*(3+Sqrt[3])}}, {1/2,1/2}, {1/6*(3+Sqrt[3]),1/6*(3-Sqrt[3])}]},
	{"ESDIRK 3()3", RungeKutta[
		{{0,0,0},{1/6*(3+Sqrt[3]),1/6*(3+Sqrt[3]),0},{1/96*(9-5 Sqrt[3]),1/96*(15-19 Sqrt[3]),1/6*(3+Sqrt[3])}},
		{1/52*(9+Sqrt[3]),1/44*(13-7 Sqrt[3]),4/143*(19+5 Sqrt[3])},
		{0,1+1/Sqrt[3],1/12*(9-Sqrt[3])}
	]},
	{"SDIRK 3(2)3", RungeKutta[
		{{1+Root[-4-9 #1+6 #1^3&,2], 0, 0},{Root[2-9 #1+24 #1^3&,2], 1+Root[-4-9 #1+6 #1^3&,2], 0},{Root[-7+36 #1-54 #1^2+24 #1^3&,3],Root[-8+27 #1^2+12 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2]}},
		{Root[-7+36 #1-54 #1^2+24 #1^3&,3],Root[-8+27 #1^2+12 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2]},
		{1+Root[-4-9 #1+6 #1^3&,2],Root[-17+63 #1-72 #1^2+24 #1^3&,2],1},
		{Root[1-6 #1+3 #1^2+4 #1^3&,3],Root[-2+12 #1-15 #1^2+4 #1^3&,1],0}
	]},
	{"SDIRK 3(2)3P", RungeKutta[
		{{\[FormalGamma],0,0},{(2-6 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])/(3+6 (-2+\[FormalGamma]) \[FormalGamma]),\[FormalGamma],0},{(-1+4 \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),-((3 (1+2 (-2+\[FormalGamma]) \[FormalGamma])^2)/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])),\[FormalGamma]}},
		{(-1+4 \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),-((3 (1+2 (-2+\[FormalGamma]) \[FormalGamma])^2)/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])),\[FormalGamma]},
		{\[FormalGamma],(2-9 \[FormalGamma]+6 \[FormalGamma]^2)/(3+6 (-2+\[FormalGamma]) \[FormalGamma]),1},
		{(-1-6 (-1+\[FormalGamma]) \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),(3 (-1+2 \[FormalGamma]) (1+2 (-2+\[FormalGamma]) \[FormalGamma]))/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),0}
	]},
	{"SDIRK 3(2)3A", RungeKutta[RungeKutta[{{1/3,0,0},{1/6,1/3,0},{5/6,-5/12,1/3}}, {6/5,-1,4/5}], {15/13,-12/13,10/13}]},
	{"ESDIRK 3(2)4", RungeKutta[
		{{0,0,0,0},{1+Root[-4-9 #1+6 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2],0,0},{1/384 Root[2539044-51786 #1+162 #1^2+#1^3&,3],1/384 Root[-415332-8586 #1+126 #1^2+#1^3&,2],1+Root[-4-9 #1+6 #1^3&,2],0},{Root[-43+636 #1-3000 #1^2+4448 #1^3&,2],Root[1-684 #1+3816 #1^2+7968 #1^3&,1],Root[32768-73728 #1+6768 #1^2+34611 #1^3&,3],1+Root[-4-9 #1+6 #1^3&,2]}},
		{Root[-43+636 #1-3000 #1^2+4448 #1^3&,2],Root[1-684 #1+3816 #1^2+7968 #1^3&,1],Root[32768-73728 #1+6768 #1^2+34611 #1^3&,3],1+Root[-4-9 #1+6 #1^3&,2]},
		{0,2 (1+Root[-4-9 #1+6 #1^3&,2]),1/24 (18+Root[-144-54 #1+#1^3&,2]),1},
		{Root[19177-289836 #1+1044864 #1^2+213504 #1^3&,2],Root[-95821+549972 #1+1050624 #1^2+382464 #1^3&,2],Root[-1270016+3346992 #1-1961496 #1^2+103833 #1^3&,2],Root[434-3519 #1+4752 #1^2+576 #1^3&,3]}
	]},
	{"ESDIRK 3(2)4P", RungeKutta[
		{{0,0,0,0},{\[FormalGamma],\[FormalGamma],0,0},{-((9+16 \[FormalGamma] (-12+\[FormalGamma] (95+3 \[FormalGamma] (-117+8 \[FormalGamma] (26+3 \[FormalGamma] (-7+2 \[FormalGamma]))))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),-(((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),\[FormalGamma],0},{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]}},
		{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]},
		{0,2 \[FormalGamma],(3+4 \[FormalGamma] (-5+6 \[FormalGamma]))/(4+24 (-1+\[FormalGamma]) \[FormalGamma]),1},
		{(-3+2 \[FormalGamma] (33+\[FormalGamma] (-280+\[FormalGamma] (1166+\[FormalGamma] (-2530+3 \[FormalGamma] (957-2 \[FormalGamma] (283+4 \[FormalGamma] (-25+6 \[FormalGamma]))))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-3+2 \[FormalGamma] (31+\[FormalGamma] (-252+\[FormalGamma] (1028+\[FormalGamma] (-2242+3 \[FormalGamma] (873-4 \[FormalGamma] (137+12 (-4+\[FormalGamma]) \[FormalGamma])))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),(16 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-2+\[FormalGamma] (21+\[FormalGamma] (-68+\[FormalGamma] (79+3 \[FormalGamma] (-11+4 \[FormalGamma]))))))/((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((3 \[FormalGamma]^2 (-1+\[FormalGamma] (4+(-2+\[FormalGamma]) \[FormalGamma])))/(1+6 (-1+\[FormalGamma]) \[FormalGamma]))}
	]},
	{"ESDIRK 3(2)4A", RungeKutta[
		RungeKutta[{{0,0,0,0},{1/3,1/3,0,0},{79/192,65/192,1/3,0},{29/208,41/240,-28/195,1/3}}, {9/52,3/20,48/455,4/7}],
		{5/26,1/6,32/273,11/21}
	]},
	{"SDIRK 3(2)4", RungeKutta[
		RungeKutta[{{9/40,0,0,0},{163/520,9/40,0,0},{-6481433/8838675,87795409/70709400,9/40,0},{4032/9943,6929/15485,-(723/9272),9/40}}],
		{20/51,64477140871/138472716300,-1303583701/18463028840,1034014989/4858691800}
	]},
	{"ESDIRK 3(2)5", RungeKutta[
		{{0,0,0,0,0},{9/40,9/40,0,0,0},{9/80 (1+Sqrt[2]),9/80 (1+Sqrt[2]),9/40,0,0},{1/80 (8+7 Sqrt[2]),1/80 (8+7 Sqrt[2]),-(7/40) (-1+Sqrt[2]),9/40,0},{(-1181+1187 Sqrt[2])/2835,(-1181+1187 Sqrt[2])/2835,-((2374 (-1+Sqrt[2]))/2835),5827/7560,9/40}},
		{(-1181+1187 Sqrt[2])/2835,(-1181+1187 Sqrt[2])/2835,-((2374 (-1+Sqrt[2]))/2835),5827/7560,9/40},
		{0,9/20,9/40*(2+Sqrt[2]),3/5,1},
		{18/101,18/101,-8673046/(1515*(-32822+35625*Sqrt[2])),(5128050998831+1058874840048*Sqrt[2])/8853645249960,81*(1004412917+104076552 Sqrt[2])/421602154760}
	]},
	{"SDIRK 4()3", RungeKutta[
		{{1/2+Cos[\[Pi]/18]/Sqrt[3],0,0},{-(Cos[\[Pi]/18]/Sqrt[3]),1/2+Cos[\[Pi]/18]/Sqrt[3],0},{1/4 Csc[\[Pi]/9]^2,-(Cot[\[Pi]/18]/Sqrt[3]),1/2+Cos[\[Pi]/18]/Sqrt[3]}},
		{1/2-Sin[(2 \[Pi])/9]/Sqrt[3],(2 Sin[(2 \[Pi])/9])/Sqrt[3],1/2-Sin[(2 \[Pi])/9]/Sqrt[3]},
		{1/2+Cos[\[Pi]/18]/Sqrt[3],1/2,-(1/16) Sec[\[Pi]/18]^2 Sec[\[Pi]/9]}
	]},
	{"SDIRK 4(3)4A", RungeKutta[
		RungeKutta[{{2/5,0,0,0},{-4/13,2/5,0,0},{369/26950,4251/26950,2/5,0},{2535561/7304000,165576021/796136000,-1846467/7237600,2/5}}, {469/432,19773/137776,-116963/70632,9130/6399}],
		{18/23,468299/2376636,-325409/270756,59960/49059}
	]},
	{"SDIRK 4(3)5", RungeKutta[
		RungeKutta[{{1/4,0,0,0,0},{13/20,1/4,0,0,0},{580/1287,-175/5148,1/4,0,0},{12698/37375,-201/2990,891/11500,1/4,0},{944/1365,-400/819,99/35,-575/252,1/4}}],
		{41911/60060,-83975/144144,3393/1120,-27025/11088,103/352}
	]},
	{"ESDIRK 4(3)6", RungeKutta[
		{{0,0,0,0,0,0},{1/4,1/4,0,0,0,0},{1/8 (1-Sqrt[2]),1/8 (1-Sqrt[2]),1/4,0,0,0},{1/64 (5-7 Sqrt[2]),1/64 (5-7 Sqrt[2]),7/32 (1+Sqrt[2]),1/4,0,0},{(-13796-54539 Sqrt[2])/125000,(-13796-54539 Sqrt[2])/125000,(506605+132109 Sqrt[2])/437500,(166 (-97+376 Sqrt[2]))/109375,1/4,0},{(1181-987 Sqrt[2])/13782,(1181-987 Sqrt[2])/13782,(47 (-267+1783 Sqrt[2]))/273343,-((16 (-22922+3525 Sqrt[2]))/571953),-((15625 (97+376 Sqrt[2]))/90749876),1/4}},
		{(1181-987 Sqrt[2])/13782,(1181-987 Sqrt[2])/13782,(47 (-267+1783 Sqrt[2]))/273343,-((16 (-22922+3525 Sqrt[2]))/571953),-((15625 (97+376 Sqrt[2]))/90749876),1/4},
		{0,1/2,1/4 (2-Sqrt[2]),5/8,26/25,1},
		{(263980-483197 Sqrt[2])/4515000,(263980-483197 Sqrt[2])/4515000,(483197 (1+Sqrt[2]))/2257500,293362/564375,-(1/12),10/43}
	]}
];


(* ::Section:: *)
(*Error Handling*)


RungeKutta::args = "RungeKutta called with `1` arguments; must have an A, b, c, and possibly \!\(\*OverscriptBox[\(b\), \(^\)]\).";
RungeKutta[args___] /; Not[3 <= Length[Unevaluated[args]] <= 4] := (Message[RungeKutta::args, Length[Unevaluated[args]]]; $Failed);
RungeKutta::squarea = "RungeKutta A coefficients must be a square matrix.";
RungeKutta[A_, __] /; !SquareMatrixQ[A] := (Print[Stack[]]; Message[RungeKutta::squarea]; $Failed);
RungeKutta::vector = "RungeKutta `1` coefficients must be a vector.";
RungeKutta[_, b_, __] /; !VectorQ[b] := (Message[RungeKutta::vector, "b"]; $Failed);
RungeKutta[_, _, c_, ___] /; !VectorQ[c] := (Message[RungeKutta::vector, "c"]; $Failed);
RungeKutta[_, _, _, bHat_] /; !VectorQ[bHat] := (Message[RungeKutta::vector, "\!\(\*OverscriptBox[\(b\), \(^\)]\)"]; $Failed);
RungeKutta::length = "RungeKutta coefficients must have compatible lengths.";
RungeKutta[args___] /; ArrayDepth[{args}] != 2 := (Message[RungeKutta::length]; $Failed);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
