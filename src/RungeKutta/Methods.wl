(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`Methods`"];


CSL`OdeUtils`RungeKutta`Methods::usage = "Package containing functions for creating Runge-Kutta methods";

RungeKutta::usage = "Constructs a Runge-Kutta method";
RungeKuttaEmbedded::usage = "Gets the embedded Runge-Kutta method";
RungeKuttaPairQ::usage = "Returns True if m is a Runge-Kutta method with an embedded method";
RungeKuttaCollocation::usage = "Constructs a collocated Runge-Kutta method";
RungeKuttaCompose::usage = "Builds a single Runge-Kutta method out of sub-methods";
RungeKuttaA::usage = "Gets the A coefficients of a Runge-Kutta method";
RungeKuttaB::usage = "Gets the b coefficients of a Runge-Kutta method";
RungeKuttaC::usage = "Gets the c coefficients of a Runge-Kutta method";
RungeKuttaD::usage = "Gets the embedded coefficients of a Runge-Kutta method";
RungeKuttaStages::usage = "The number of stages in a Runge-Kutta method";


Begin["`Private`"];
Needs["CSL`OdeUtils`Tableaus`"];
Needs["CSL`OdeUtils`Internal`Catalog`"];
Needs["CSL`OdeUtils`Internal`Composition`"];

LagrangeBasis[t_, c_, i_] := Product[(t - c[[l]]) / (c[[i]] - c[[l]]), {l, DeleteCases[Range[Length[c]], i]}];

RkCompose[m_] := RungeKutta[
	ArrayFlatten[Table[Which[
		i == j, m[[i, 2]] * RungeKuttaA[m[[i, 1]]],
		i >= j, m[[j, 2]] * ConstantArray[RungeKuttaB[m[[j, 1]]], RungeKuttaStages[m[[i, 1]]]],
		True, 0
	], {i, Length[m]}, {j, Length[m]}]],
	Catenate[Map[Last[#] * RungeKuttaB[First[#]] &, m]],
	Catenate[Map[Last[#] * RungeKuttaC[First[#]] &, m] + FoldList[Plus, 0, m[[1 ;; -2, 2]]]],
	(*Can embedded methods be handled better?*)
	If[AllTrue[m[[All, 1]], RungeKuttaPairQ], Catenate[Map[Last[#] * RungeKuttaD[First[#]] &, m]], Unevaluated[Sequence[]]]
];

erk2fam[c2_, d1_] := RungeKutta[{{0,0},{c2,0}}, {(2 c2-1)/(2 c2),1/(2 c2)}, {0,c2}, {d1, 1-d1}];
erk3fam[c2_, c3_, d1_] := RungeKutta[
	{{0,0,0},{c2,0,0},{(c3 (3 (-1+c2) c2+c3))/(c2 (-2+3 c2)),((c2-c3) c3)/(c2 (-2+3 c2)),0}},
	{(2-3 c2-3 c3+6 c2 c3)/(6 c2 c3),(2-3 c3)/(6 c2^2-6 c2 c3),(-2+3 c2)/(6 c2 c3-6 c3^2)},
	{0,c2,c3},
	{d1,(1-2 c3+2 c3 d1)/(2 (c2-c3)),(1-2 c2+2 c2 d1)/(2 (-c2+c3))}
];

RkCheck[A_, x__] := TableauQ[A] && AllTrue[{x}, VectorQ] && SameQ[Length /@ {x}];


RungeKutta[A_?TableauQ, b_] := RungeKutta[A, b, Total[A, {2}]];
RungeKutta[A_?TableauQ] := RungeKutta[A, Last[A]];
RungeKutta[s_Integer] := RungeKutta[TableauFirk[s], Table[Subscript[\[FormalB], i], {i, s}], Table[Subscript[\[FormalC], i], {i, s}]];
RungeKutta[RungeKutta[A_, b_, c_, ___], d_] := RungeKutta[A, b, c, d];

AddComposition[RungeKutta, RungeKuttaCompose, RkCompose];

RungeKutta /: HoldPattern[Times[x_, RungeKutta[A_, b_, c_]]] := RungeKutta[A, x * b, c];
RungeKutta /: HoldPattern[Times[x_, RungeKutta[A_, b_, c_, d_]]] := RungeKutta[A, x * b, c, x * d];

RungeKutta /: HoldPattern[Plus[RungeKutta[A1_, b1_, c1_, d1_], RungeKutta[A2_, b2_, c2_, d2_]]] := RungeKutta[ArrayFlatten[{{A1, 0}, {0, A2}}], Join[b1, b2], Join[c1, c2], Join[d1, d2]];
RungeKutta /: HoldPattern[Plus[RungeKutta[A1_, b1_, c1__], RungeKutta[A2_, b2_, c2__]]] := RungeKutta[ArrayFlatten[{{A1, 0}, {0, A2}}], Join[b1, b2], Join[c1, c2]];

HoldPattern[RungeKuttaEmbedded[RungeKutta[A_, _, c_, d_]]] := RungeKutta[A, d, c];

(* Maybe could make the pattern itself public *)
RungeKuttaPairQ[m_] := MatchQ[m, HoldPattern[RungeKutta[_, _, _, _]]];

RungeKuttaCollocation[c_List?VectorQ] := RungeKutta[
	Table[Integrate[LagrangeBasis[t, c, j], {t, 0, c[[i]]}], {i, Length[c]}, {j, Length[c]}],
	Table[Integrate[LagrangeBasis[t, c, i], {t, 0, 1}], {i, Length[c]}],
	c
];

HoldPattern[RungeKuttaA[RungeKutta[A_, __]]] := A;

HoldPattern[RungeKuttaB[RungeKutta[_, b_, __]]] := b;

HoldPattern[RungeKuttaC[RungeKutta[_, _, c_, ___]]] := c;

HoldPattern[RungeKuttaD[RungeKutta[_, _, _, d_]]] := d;

HoldPattern[RungeKuttaStages[RungeKutta[A_, __]]] := Length[A];

RungeKutta /: MakeBoxes[RungeKutta[A_List, b_List, c_List, d_List:Nothing], format_] := GridBox[
	Join[MapThread[Prepend, {Map[MakeBoxes, A, {2}], MakeBoxes /@ c}], Map[Prepend[MakeBoxes /@ #, ""] &, {b, d}]],
	ColumnLines -> {True, False},
	RowLines -> Append[ConstantArray[False, Length[A] - 1], True]
];

AddCatalog[
	RungeKutta,
	(*Explicit*)
	{"Euler", "Euler's Method", "Forward Euler", "Explicit Euler", RungeKutta[{{0}}, {1}]},
	{"ERK 2(1)2P", "2 Stage Explicit, Order 2", erk2fam[Subscript[\[FormalC], 2], Subscript[\[FormalD], 1]]},
	{"Heun", "Heun's Method", "Explicit Trapezoid", erk2fam[1, 1]},
	{"Ralston's 2nd Order Method", "Ralston 2", erk2fam[2/3, 1]},
	{"ERK 3(2)3P", "3 Stage Explicit, Order 3", erk3fam[Subscript[\[FormalC], 2], Subscript[\[FormalC], 3], Subscript[\[FormalD], 1]]},
	{"Ralston's 3rd Order Method", "Ralston 3", erk3fam[1/2, 3/4, 1/40]},
	{"Bogacki-Shampine", "ode23", RungeKutta[RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,3/4,0,0},{2/9,1/3,4/9,0}}], {7/24,1/4,1/3,1/8}]},
	{"RK4", "Classiscal", "Classical Runge-Kutta Method", "The Runge-Kutta Method", RungeKutta[{{0,0,0,0},{1/2,0,0,0},{0,1/2,0,0},{0,0,1,0}}, {1/6,1/3,1/3,1/6}]},
	{"ERK 4(3)", RungeKutta[
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
	
	(*Implicit*)
	{"Backward Euler", "Implicit Euler", RungeKutta[{{1}}]},
	{"Implicit Midpoint", RungeKutta[{{1/2}}, {1}]},
	{"Implicit Trapezoidal", RungeKutta[{{0, 0},{1/2,1/2}}]},
	{"SDIRK 2(1)2", RungeKutta[RungeKutta[{{1-1/Sqrt[2],0},{1/Sqrt[2],1-1/Sqrt[2]}}], {3/5,2/5}]},
	{"SDIRK 2(1)2N", RungeKutta[RungeKutta[{{1/4,0},{1/2,1/4}}], {1/2,1/2}]},
	{"SDIRK 2(1)2A", RungeKutta[RungeKutta[{{1/4,0},{7/12,1/4}},{4/7,3/7}], {52/87,35/87}]},
	{"ESDIRK 2(1)3", RungeKutta[RungeKutta[{{0,0,0},{1-Sqrt[2]/2,1-Sqrt[2]/2,0},{Sqrt[2]/4,Sqrt[2]/4,1-Sqrt[2]/2}}], {3/10,3/10,2/5}]},
	{"ESDIRK 2(1)3A", RungeKutta[RungeKutta[{{0,0,0},{1/4,1/4,0},{1/4,1/6,1/4}},{1/4,0,3/4}],{9/34,1/34,12/17}]},
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
		{{0,0,0,0},{Root[-1+9 #1-18 #1^2+6 #1^3&,2],1+Root[-4-9 #1+6 #1^3&,2],0,0},{1/384 Root[2539044-51786 #1+162 #1^2+#1^3&,3],1/384 Root[-415332-8586 #1+126 #1^2+#1^3&,2],1+Root[-4-9 #1+6 #1^3&,2],0},{Root[-43+636 #1-3000 #1^2+4448 #1^3&,2],Root[1-684 #1+3816 #1^2+7968 #1^3&,1],Root[32768-73728 #1+6768 #1^2+34611 #1^3&,3],1+Root[-4-9 #1+6 #1^3&,2]}},
		{Root[-43+636 #1-3000 #1^2+4448 #1^3&,2],Root[1-684 #1+3816 #1^2+7968 #1^3&,1],Root[32768-73728 #1+6768 #1^2+34611 #1^3&,3],1+Root[-4-9 #1+6 #1^3&,2]},
		{0,2 (1+Root[-4-9 #1+6 #1^3&,2]),1/24 (18+Root[-144-54 #1+#1^3&,2]),1},
		{Root[19177-289836 #1+1044864 #1^2+213504 #1^3&,2],Root[-95821+549972 #1+1050624 #1^2+382464 #1^3&,2],Root[-1270016+3346992 #1-1961496 #1^2+103833 #1^3&,2],Root[434-3519 #1+4752 #1^2+576 #1^3&,3]}
	]},
	{"ESDIRK 3()4P", RungeKutta[
		{{0,0,0,0},{\[FormalGamma],\[FormalGamma],0,0},{-((9+16 \[FormalGamma] (-12+\[FormalGamma] (95+3 \[FormalGamma] (-117+8 \[FormalGamma] (26+3 \[FormalGamma] (-7+2 \[FormalGamma]))))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),-(((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),\[FormalGamma],0},{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]}},
		{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]},
		{0,2 \[FormalGamma],(3+4 \[FormalGamma] (-5+6 \[FormalGamma]))/(4+24 (-1+\[FormalGamma]) \[FormalGamma]),1},
		{(-3+2 \[FormalGamma] (33+\[FormalGamma] (-280+\[FormalGamma] (1166+\[FormalGamma] (-2530+3 \[FormalGamma] (957-2 \[FormalGamma] (283+4 \[FormalGamma] (-25+6 \[FormalGamma]))))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-3+2 \[FormalGamma] (31+\[FormalGamma] (-252+\[FormalGamma] (1028+\[FormalGamma] (-2242+3 \[FormalGamma] (873-4 \[FormalGamma] (137+12 (-4+\[FormalGamma]) \[FormalGamma])))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),(16 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-2+\[FormalGamma] (21+\[FormalGamma] (-68+\[FormalGamma] (79+3 \[FormalGamma] (-11+4 \[FormalGamma]))))))/((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((3 \[FormalGamma]^2 (-1+\[FormalGamma] (4+(-2+\[FormalGamma]) \[FormalGamma])))/(1+6 (-1+\[FormalGamma]) \[FormalGamma]))}
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
	]}
];
RungeKutta[args___] /; Not[3 <= Length[Unevaluated[args]] <= 4 && RkCheck[args]] := $Failed;


End[];


EndPackage[];
