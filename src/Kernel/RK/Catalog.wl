(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


erk2Fam[c2_, d1_] := RK[{{0,0},{c2,0}}, {(2 c2-1)/(2 c2),1/(2 c2)}, {0,c2}, {d1, 1-d1}];


erk3Fam[c2_, c3_, d1_] := RK[
	{{0,0,0},{c2,0,0},{(c3 (3 (-1+c2) c2+c3))/(c2 (-2+3 c2)),((c2-c3) c3)/(c2 (-2+3 c2)),0}},
	{(2-3 c2-3 c3+6 c2 c3)/(6 c2 c3),(2-3 c3)/(6 c2^2-6 c2 c3),(-2+3 c2)/(6 c2 c3-6 c3^2)},
	{0,c2,c3},
	{d1,(1-2 c3+2 c3 d1)/(2 (c2-c3)),(1-2 c2+2 c2 d1)/(2 (-c2+c3))}
];


(* ::Section:: *)
(*Package Definitions*)


AddCatalog[
	RK,
	(*Explicit*)
	(*Order 1*)
	{"Euler's Method", "Forward Euler", "Explicit Euler", "FE", RK[{{0}}, {\[FormalTheta]}, {0}]},
	(*Order 2*)
	{"2 Stage Explicit, Order 2", "ERK 2(1)2P", erk2Fam[Subscript[\[FormalC], 2], Subscript[\[FormalD], 1]]},
	{"Heun's Method", "Explicit Trapezoidal Rule", erk2Fam[1, 1]},
	{"Ralston 2", erk2Fam[2/3, 1]},
	{"Explicit Midpoint Rule", erk2Fam[1/2, 1]},
	(*Order 3*)
	{"ERK 3(2)3P", "3 Stage Explicit, Order 3", erk3Fam[Subscript[\[FormalC], 2], Subscript[\[FormalC], 3], Subscript[\[FormalD], 1]]},
	{"Ralston 3", erk3Fam[1/2, 3/4, 1/40]},
	{"Bogacki-Shampine", "ode23", RK[RK[{{0,0,0,0},{1/2,0,0,0},{0,3/4,0,0},{2/9,1/3,4/9,0}}], {7/24,1/4,1/3,1/8}]},
	(*Order 4*)
	{"RK4", "Classical Runge-Kutta Method", "The Runge-Kutta Method", RK[{{0,0,0,0},{1/2,0,0,0},{0,1/2,0,0},{0,0,1,0}}, {1/6,1/3,1/3,1/6}]},
	{"ERK 4(3)5", RK[
		RK[{{0,0,0,0,0},{2/5,0,0,0,0},{-3/20,3/4,0,0,0},{19/44,-15/44,10/11,0,0},{11/72,25/72,25/72,11/72,0}}],
		{1251515/8970912,3710105/8970912,2519695/8970912,61105/8970912,119041/747576}
	]},
	{"Fehlberg's 4th Order Method", "Fehlberg 4", RK[
		RK[
			{{0,0,0,0,0,0},{1/4,0,0,0,0,0},{3/32,9/32,0,0,0,0},{1932/2197,-7200/2197,7296/2197,0,0,0},{439/216,-8,3680/513,-845/4104,0,0},{-8/27,2,-3544/2565,1859/4104,-11/40,0}},
			{25/216,0,1408/2565,2197/4104,-1/5,0}
		],
		{16/135,0,6656/12825,28561/56430,-9/50,2/55}
	]},
	{"RKDP", "DOPRI", "Dormand-Prince", RK[
		RK[{{0,0,0,0,0,0,0},{1/5,0,0,0,0,0,0},{3/40,9/40,0,0,0,0,0},{44/45,-56/15,32/9,0,0,0,0},{19372/6561,\[Minus]25360/2187,64448/6561,\[Minus]212/729,0,0,0},{9017/3168,\[Minus]355/33,46732/5247,49/176,\[Minus]5103/18656,0,0},{35/384,0,500/1113,125/192,\[Minus]2187/6784,11/84,0}}],
		{5179/57600,0,7571/16695,393/640,\[Minus]92097/339200,187/2100,1/40}
	]},
	{"BS(4,5)", RK[
		RK[{{0,0,0,0,0,0,0,0},{1/6,0,0,0,0,0,0,0},{2/27,4/27,0,0,0,0,0,0},{183/1372,-162/343,1053/1372,0,0,0,0,0},{68/297,-4/11,42/143,1960/3861,0,0,0,0},{597/22528,81/352,63099/585728,58653/366080,4617/20480,0,0,0},{174197/959244,-30942/79937,8152137/19744439,666106/1039181,-29421/29068,482048/414219,0,0},{587/8064,0,4440339/15491840,24353/124800,387/44800,2152/5985,7267/94080,0}}],
		{2479/34992,0,123/416,612941/3411720,43/1440,2272/6561,79937/1113912,3293/556956}
	]},
	
	(*Implicit*)
	(*Order 1*)
	{"Backward Euler", "Implicit Euler", RK[{{1}}]},
	(*Order 2*)
	{"Implicit Midpoint", RK[{{1/2}}, {1}]},
	{"Implicit Trapezoidal", RK[{{0, 0},{1/2,1/2}}]},
	{"SDIRK 2(1)2", RK[RK[{{1-1/Sqrt[2],0},{1/Sqrt[2],1-1/Sqrt[2]}}], {3/5,2/5}]},
	{"SDIRK 2()2N", RK[{{1/4,0},{1/2,1/4}}, {1/2,1/2}]},
	{"SDIRK 2(1)2A", RK[RK[{{1/4,0},{7/12,1/4}},{4/7,3/7}], {52/87,35/87}]},
	{"ESDIRK 2(1)3", RK[RK[{{0,0,0},{1-Sqrt[2]/2,1-Sqrt[2]/2,0},{Sqrt[2]/4,Sqrt[2]/4,1-Sqrt[2]/2}}], {3/10,3/10,2/5}]},
	{"ESDIRK 2()3A", RK[{{0,0,0},{1/4,1/4,0},{1/8 (1+Sqrt[2]),1/8 (1+Sqrt[2]),1/4}},{1-1/Sqrt[2],1-1/Sqrt[2],-1+Sqrt[2]}]},
	(*Order 3*)
	{"SDIRK 3()2", RK[{{1/6*(3+Sqrt[3]),0},{-1/Sqrt[3],1/6*(3+Sqrt[3])}}, {1/2,1/2}, {1/6*(3+Sqrt[3]),1/6*(3-Sqrt[3])}]},
	{"ESDIRK 3()3", RK[
		{{0,0,0},{1/6*(3+Sqrt[3]),1/6*(3+Sqrt[3]),0},{1/96*(9-5 Sqrt[3]),1/96*(15-19 Sqrt[3]),1/6*(3+Sqrt[3])}},
		{1/52*(9+Sqrt[3]),1/44*(13-7 Sqrt[3]),4/143*(19+5 Sqrt[3])},
		{0,1+1/Sqrt[3],1/12*(9-Sqrt[3])}
	]},
	{"SDIRK 3(2)3", RK[
		{{Root[-1+9 #-18 #^2+6 #^3&,2], 0, 0},{Root[2-9 #+24 #^3&,2], Root[-1+9 #-18 #^2+6 #^3&,2], 0},{Root[-7+36 #-54 #^2+24 #^3&,3],Root[-8+27 #^2+12 #^3&,2],Root[-1+9 #-18 #^2+6 #^3&,2]}},
		{Root[-7+36 #-54 #^2+24 #^3&,3],Root[-8+27 #^2+12 #^3&,2],Root[-1+9 #-18 #^2+6 #^3&,2]},
		{Root[-1+9 #-18 #^2+6 #^3&,2],Root[-17+63 #-72 #^2+24 #^3&,2],1},
		{Root[1-6 #+3 #^2+4 #^3&,3],Root[-2+12 #-15 #^2+4 #^3&,1],0}
	]},
	{"SDIRK 3(2)3P", RK[
		{{\[FormalGamma],0,0},{(2-6 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])/(3+6 (-2+\[FormalGamma]) \[FormalGamma]),\[FormalGamma],0},{(-1+4 \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),-((3 (1+2 (-2+\[FormalGamma]) \[FormalGamma])^2)/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])),\[FormalGamma]}},
		{(-1+4 \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),-((3 (1+2 (-2+\[FormalGamma]) \[FormalGamma])^2)/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma])),\[FormalGamma]},
		{\[FormalGamma],(2-9 \[FormalGamma]+6 \[FormalGamma]^2)/(3+6 (-2+\[FormalGamma]) \[FormalGamma]),1},
		{(-1-6 (-1+\[FormalGamma]) \[FormalGamma])/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),(3 (-1+2 \[FormalGamma]) (1+2 (-2+\[FormalGamma]) \[FormalGamma]))/(-4+12 (-2+\[FormalGamma]) (-1+\[FormalGamma]) \[FormalGamma]),0}
	]},
	{"SDIRK 3(2)3A", RK[RK[{{1/3,0,0},{1/6,1/3,0},{5/6,-5/12,1/3}}, {6/5,-1,4/5}], {15/13,-12/13,10/13}]},
	{"ESDIRK 3(2)4", RK[
		{{0,0,0,0},{Root[-1+9*#-18*#^2+6*#^3&,2],Root[-1+9*#-18*#^2+6*#^3&,2],0,0},{Root[70529-552384*#+663552*#^2+1572864*#^3&,3],Root[-11537-91584*#+516096*#^2+1572864*#^3&,2],Root[-1+9*#-18*#^2+6*#^3&,2], 0},{Root[-43+636*#-3000*#^2+4448*#^3&,2],Root[1-684*#+3816*#^2+7968*#^3&,1],Root[32768-73728*#+6768*#^2+34611*#^3&,3],Root[-1+9*#-18*#^2+6*#^3&,2]}},
		{Root[-43+636 #-3000 #^2+4448 #^3&,2],Root[1-684 #+3816 #^2+7968 #^3&,1],Root[32768-73728 #+6768 #^2+34611 #^3&,3],Root[-1+9 #-18 #^2+6 #^3&,2]},
		{0,Root[-4+18 #-18 #^2+3 #^3&,2],Root[-139+612 #-864 #^2+384*#^3&,2],1},
		{Root[19177-289836 #+1044864 #^2+213504 #^3&,2],Root[-95821+549972 #+1050624 #^2+382464 #^3&,2],Root[-1270016+3346992 #-1961496 #^2+103833 #^3&,2],Root[434-3519 #+4752 #^2+576 #^3&,3]}
	]},
	{"ESDIRK 3(2)4P", RK[
		{{0,0,0,0},{\[FormalGamma],\[FormalGamma],0,0},{-((9+16 \[FormalGamma] (-12+\[FormalGamma] (95+3 \[FormalGamma] (-117+8 \[FormalGamma] (26+3 \[FormalGamma] (-7+2 \[FormalGamma]))))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),-(((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))/(64 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma])^2)),\[FormalGamma],0},{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]}},
		{(-1+6 \[FormalGamma] (-1+2 \[FormalGamma]) (-3+8 \[FormalGamma]))/(12 \[FormalGamma] (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-1+6 \[FormalGamma])/(12 \[FormalGamma] (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((16 (1+6 (-1+\[FormalGamma]) \[FormalGamma])^3)/(3 (3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma]))))),\[FormalGamma]},
		{0,2 \[FormalGamma],(3+4 \[FormalGamma] (-5+6 \[FormalGamma]))/(4+24 (-1+\[FormalGamma]) \[FormalGamma]),1},
		{(-3+2 \[FormalGamma] (33+\[FormalGamma] (-280+\[FormalGamma] (1166+\[FormalGamma] (-2530+3 \[FormalGamma] (957-2 \[FormalGamma] (283+4 \[FormalGamma] (-25+6 \[FormalGamma]))))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (3+4 \[FormalGamma] (-5+6 \[FormalGamma]))),(-3+2 \[FormalGamma] (31+\[FormalGamma] (-252+\[FormalGamma] (1028+\[FormalGamma] (-2242+3 \[FormalGamma] (873-4 \[FormalGamma] (137+12 (-4+\[FormalGamma]) \[FormalGamma])))))))/(4 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),(16 \[FormalGamma] (1+6 (-1+\[FormalGamma]) \[FormalGamma]) (-2+\[FormalGamma] (21+\[FormalGamma] (-68+\[FormalGamma] (79+3 \[FormalGamma] (-11+4 \[FormalGamma]))))))/((3+4 \[FormalGamma] (-5+6 \[FormalGamma])) (-3+4 \[FormalGamma] (7+6 \[FormalGamma] (-3+2 \[FormalGamma])))),-((3 \[FormalGamma]^2 (-1+\[FormalGamma] (4+(-2+\[FormalGamma]) \[FormalGamma])))/(1+6 (-1+\[FormalGamma]) \[FormalGamma]))}
	]},
	{"ESDIRK 3(2)4A", RK[
		RK[{{0,0,0,0},{1/3,1/3,0,0},{79/192,65/192,1/3,0},{29/208,41/240,-28/195,1/3}}, {9/52,3/20,48/455,4/7}],
		{5/26,1/6,32/273,11/21}
	]},
	{"SDIRK 3(2)4", RK[
		RK[{{9/40,0,0,0},{163/520,9/40,0,0},{-6481433/8838675,87795409/70709400,9/40,0},{4032/9943,6929/15485,-(723/9272),9/40}}],
		{20/51,64477140871/138472716300,-1303583701/18463028840,1034014989/4858691800}
	]},
	{"ESDIRK 3(2)5", RK[
		{{0,0,0,0,0},{9/40,9/40,0,0,0},{9/80 (1+Sqrt[2]),9/80 (1+Sqrt[2]),9/40,0,0},{1/80 (8+7 Sqrt[2]),1/80 (8+7 Sqrt[2]),-(7/40) (-1+Sqrt[2]),9/40,0},{(-1181+1187 Sqrt[2])/2835,(-1181+1187 Sqrt[2])/2835,-((2374 (-1+Sqrt[2]))/2835),5827/7560,9/40}},
		{(-1181+1187 Sqrt[2])/2835,(-1181+1187 Sqrt[2])/2835,-((2374 (-1+Sqrt[2]))/2835),5827/7560,9/40},
		{0,9/20,9/40*(2+Sqrt[2]),3/5,1},
		{(-4800247+5547709*Sqrt[2])/16519545,(-4800247+5547709*Sqrt[2])/16519545,(-11095418*(-1+Sqrt[2]))/16519545,30698249/44052120,49563/233080}
	]},
	(*Order 4*)
	{"SDIRK 4()3", RK[
		{{1/2+Cos[\[Pi]/18]/Sqrt[3],0,0},{-(Cos[\[Pi]/18]/Sqrt[3]),1/2+Cos[\[Pi]/18]/Sqrt[3],0},{1/4 Csc[\[Pi]/9]^2,-(Cot[\[Pi]/18]/Sqrt[3]),1/2+Cos[\[Pi]/18]/Sqrt[3]}},
		{1/2-Sin[(2 \[Pi])/9]/Sqrt[3],(2 Sin[(2 \[Pi])/9])/Sqrt[3],1/2-Sin[(2 \[Pi])/9]/Sqrt[3]},
		{1/2+Cos[\[Pi]/18]/Sqrt[3],1/2,-(1/16) Sec[\[Pi]/18]^2 Sec[\[Pi]/9]}
	]},
	{"SDIRK 4(3)4A", RK[
		RK[{{2/5,0,0,0},{-4/13,2/5,0,0},{369/26950,4251/26950,2/5,0},{2535561/7304000,165576021/796136000,-1846467/7237600,2/5}}, {469/432,19773/137776,-116963/70632,9130/6399}],
		{18/23,468299/2376636,-325409/270756,59960/49059}
	]},
	{"SDIRK 4(3)5", RK[
		RK[{{1/4,0,0,0,0},{13/20,1/4,0,0,0},{580/1287,-175/5148,1/4,0,0},{12698/37375,-201/2990,891/11500,1/4,0},{944/1365,-400/819,99/35,-575/252,1/4}}],
		{41911/60060,-83975/144144,3393/1120,-27025/11088,103/352}
	]},
	{"ESDIRK 4(3)6", RK[
		{{0,0,0,0,0,0},{1/4,1/4,0,0,0,0},{1/8 (1-Sqrt[2]),1/8 (1-Sqrt[2]),1/4,0,0,0},{1/64 (5-7 Sqrt[2]),1/64 (5-7 Sqrt[2]),7/32 (1+Sqrt[2]),1/4,0,0},{(-13796-54539 Sqrt[2])/125000,(-13796-54539 Sqrt[2])/125000,(506605+132109 Sqrt[2])/437500,(166 (-97+376 Sqrt[2]))/109375,1/4,0},{(1181-987 Sqrt[2])/13782,(1181-987 Sqrt[2])/13782,(47 (-267+1783 Sqrt[2]))/273343,-((16 (-22922+3525 Sqrt[2]))/571953),-((15625 (97+376 Sqrt[2]))/90749876),1/4}},
		{(1181-987 Sqrt[2])/13782,(1181-987 Sqrt[2])/13782,(47 (-267+1783 Sqrt[2]))/273343,-((16 (-22922+3525 Sqrt[2]))/571953),-((15625 (97+376 Sqrt[2]))/90749876),1/4},
		{0,1/2,1/4 (2-Sqrt[2]),5/8,26/25,1},
		{(263980-483197 Sqrt[2])/4515000,(263980-483197 Sqrt[2])/4515000,(483197 (1+Sqrt[2]))/2257500,293362/564375,-(1/12),10/43}
	]},
	{"ESDIRK 4(3)6_2", RK[
		{{0,0,0,0,0,0},{31/125,31/125,0,0,0,0},{-(31/250) (-1+Sqrt[2]),-(31/250) (-1+Sqrt[2]),31/125,0,0,0},{(3207616125-4988550383 Sqrt[2])/45111758000,(3207616125-4988550383 Sqrt[2])/45111758000,(4988550383 (1+Sqrt[2]))/22555879000,31/125,0,0},{(-729065961807334734137573747751384321692059515-7229797438298932089798744078731136686432666464 Sqrt[2])/15883351089549903543862546352747403666058900000,(-729065961807334734137573747751384321692059515-7229797438298932089798744078731136686432666464 Sqrt[2])/15883351089549903543862546352747403666058900000,(4964486648895151063308624564369368520423830177+713857900853717644964740935949278615467860142 Sqrt[2])/3970837772387475885965636588186850916514725000,(7445991146322952463 (-12369156285316982883770963+25136200148914094351205060 Sqrt[2]))/256183082089514573288105586334635543000950000,31/125,0},{-((5 (-2110932693093868691+1533194354071026441 Sqrt[2]))/115884336229314693036),-((5 (-2110932693093868691+1533194354071026441 Sqrt[2]))/115884336229314693036),(2555323923451710735 (-6048740713661289+77098206002085287 Sqrt[2]))/734804127342049063755551824449399454,-((2482601908 (-23284603896189378453336101+3192366177568222221235500 Sqrt[2]))/93820559435419590143821229478375875),-((35152 (12369156285316982883770963+25136200148914094351205060 Sqrt[2]))/17211893037724327971040208859255),31/125}},
		{-((5 (-2110932693093868691+1533194354071026441 Sqrt[2]))/115884336229314693036),-((5 (-2110932693093868691+1533194354071026441 Sqrt[2]))/115884336229314693036),(2555323923451710735 (-6048740713661289+77098206002085287 Sqrt[2]))/734804127342049063755551824449399454,-((2482601908 (-23284603896189378453336101+3192366177568222221235500 Sqrt[2]))/93820559435419590143821229478375875),-((35152 (12369156285316982883770963+25136200148914094351205060 Sqrt[2]))/17211893037724327971040208859255),31/125},
		{0,62/125,-(31/125) (-2+Sqrt[2]),1043/1706,1361/1300,1},
		{-((5 (-3743792079157123468417051182578387632169878179354150461267+7509743559116763398399558359471407607553530927617221979267 Sqrt[2]))/413376289881165562538761775786449559325285303832269846248532),-((5 (-3743792079157123468417051182578387632169878179354150461267+7509743559116763398399558359471407607553530927617221979267 Sqrt[2]))/413376289881165562538761775786449559325285303832269846248532),(45 (-376277745468527465422510390122213691763133259285715613501949388500967567+3163493344542794387356529339171613197177759833334498601157633509846792961 Sqrt[2]))/374450463568820670691422289271600739465960214523014943280481963159474592014,-((7447805724 (-30796595204391489186725409354838611944447511436524210438959361229+5356699458820776186626674127832742775701764587931530568670417000 Sqrt[2]))/334671587515020313058435611647145553127481669198682013163613352423157176125),-((70304 (-608559036406671511493579787123141035449916017191970764740645921+6774703650384819863408854827742638658022061927449270086215349230 Sqrt[2]))/8771046714429658204519525353602763401062944506545342520924050909897455),-((186 (-633901541446634473498387641109037387477+59924918445688678723193282134274406250 Sqrt[2]))/445893188988853813318851605160991651935875)}
	]},
	{"ESDIRK 4(3)7", RK[
		{{0,0,0,0,0,0,0},{1/8,1/8,0,0,0,0,0},{1/16 (1-Sqrt[2]),1/16 (1-Sqrt[2]),1/8,0,0,0,0},{1/16 (4+Sqrt[2]),1/16 (4+Sqrt[2]),1/8 (-1-Sqrt[2]),1/8,0,0,0},{(-2138105410754942387-4077318812232505987 Sqrt[2])/22002524772175105452,(-2138105410754942387-4077318812232505987 Sqrt[2])/22002524772175105452,(1881765828213863329+1097776492009321329 Sqrt[2])/3667087462029184242,(223 (49895406236802899+14062588990216000 Sqrt[2]))/44005049544350210904,1/8,0,0},{(2330316699762358854485279+275894650810372619660509 Sqrt[2])/11645407056206806138486944,(2330316699762358854485279+275894650810372619660509 Sqrt[2])/11645407056206806138486944,(31126722736428210227983778413-124903827358124693038297195647 Sqrt[2])/3359406859638082366332331422576,-((13 (-41042201831586681979370941+23644311061130826219079180 Sqrt[2]))/5193851547068235537765177024),(173745 (4769264701145763978019+3378306303803472802828 Sqrt[2]))/11984110662384149662937126336,1/8,0},{-((32 (2504867737+2792974425 Sqrt[2]))/515281350525),-((32 (2504867737+2792974425 Sqrt[2]))/515281350525),-((23833381760 (-17955954715+5247856139 Sqrt[2]))/267336321615850756583),(-365656294557259+368102857317300 Sqrt[2])/298760127034395,-((1640558367 (-23065376881459+14359612510400 Sqrt[2]))/5834046500811190835800),(250047 (-12536047608293+7995354787300 Sqrt[2]))/552172575294937025,1/8}},
		{-((32 (2504867737+2792974425 Sqrt[2]))/515281350525),-((32 (2504867737+2792974425 Sqrt[2]))/515281350525),-((23833381760 (-17955954715+5247856139 Sqrt[2]))/267336321615850756583),(-365656294557259+368102857317300 Sqrt[2])/298760127034395,-((1640558367 (-23065376881459+14359612510400 Sqrt[2]))/5834046500811190835800),(250047 (-12536047608293+7995354787300 Sqrt[2]))/552172575294937025,1/8},
		{0,1/4,1/8 (2-Sqrt[2]),1/2,395/567,89/126,1},
		{-((2 (264365681132454253047539522819713823227114374613038161926370596461398341584432933030421819+416897051001881111276855240085634142945451998289277094467639012048688377709007768528233865 Sqrt[2]))/7054298466987034558161402579699308132249718295213946575586971547366073769340890076380009125),-((2 (264365681132454253047539522819713823227114374613038161926370596461398341584432933030421819+416897051001881111276855240085634142945451998289277094467639012048688377709007768528233865 Sqrt[2]))/7054298466987034558161402579699308132249718295213946575586971547366073769340890076380009125),-((4 (-1315172776168078538150885030543223955320746092023976173538278514020407450718729825667165065819397331+503794097420973870012890256826564896702271973860072764045916062471600665625783952698339395327168389 Sqrt[2]))/3659884453072500069470648674709260609247600737658235897987616840760107046517850297080899617720924895),(-5001150876259886735284717298121692814747059025542737873317893266331159549533508729277108556723+4983791817424078162167633273013458144363749563550069577481819539024822636161198367855913403220 Sqrt[2])/4090082251159082636821981215709658855078386667565046224525326103162849571463848066285129290675,-((412929657 (-13705978205121046175458288203051652572031250165383850306634274730013718224575425440904885961+8629832580020167949471738422003240917483313073551857001145669345116536450373038028984272640 Sqrt[2]))/814991758427011169560100293223739618170479072075889268648956543837430300336001356132883801402311500),(729 (-1300379320047156606908977145829376297182991590962472041181588694384846462257051842784970644257+833991901288288735406216987273548421340401047065029913308299882610235747739267739654909590180 Sqrt[2]))/154272372711537278353672029043357012893320721974440658599355177904463596805598738927994250849625,19/140}
	]},
	(*Order 5*)
	{"ESDIRK 5(4)8", RK[
		{{0,0,0,0,0,0,0,0},{1/7,1/7,0,0,0,0,0,0},{1/14 (1+Sqrt[2]),1/14 (1+Sqrt[2]),1/7,0,0,0,0,0},{(6900-3391 Sqrt[2])/11774,(6900-3391 Sqrt[2])/11774,(3391 (-1+Sqrt[2]))/5887,1/7,0,0,0,0},{(2898290599879029649123142170601124314194420983357326-1102141983778246429344843802960680605890416743222145 Sqrt[2])/7451989721283837168502226088765279246722993844058880,(2898290599879029649123142170601124314194420983357326-1102141983778246429344843802960680605890416743222145 Sqrt[2])/7451989721283837168502226088765279246722993844058880,(-1156876346818749988524055493343596000261169931461041+1084888760645913785690527074470413796795288020842493 Sqrt[2])/3725994860641918584251113044382639623361496922029440,(397793 (-217958208746070833752221240400483042093233725+43372364853913074524480643174381673622031364 Sqrt[2]))/3725994860641918584251113044382639623361496922029440,1/7,0,0,0},{(9613824244190702765347147580024090557121708054676739309553277914498495639723562024917342-5826300658108371601312245519318703512309458461735112329168916312725883724046254099365001 Sqrt[2])/6122188848300493907967407961853347805676604260368983013775734757994186227861668984238080,(9613824244190702765347147580024090557121708054676739309553277914498495639723562024917342-5826300658108371601312245519318703512309458461735112329168916312725883724046254099365001 Sqrt[2])/6122188848300493907967407961853347805676604260368983013775734757994186227861668984238080,(-40252676651996695669201561643469506083465712325559718260145472251009048141703401880538919041+23991810817873259388464773254262129153111024296368825616706059465883322980860588038933200949 Sqrt[2])/15847285833825828480773635509257390794993890127965112531158489421067951050819930165700270080,-((278371 (-933172707804918669699653664423723887166404919367880223866666997795925784178390474993+901823351937197270013169399931824854202522367341572236512159706163677953587918693788 Sqrt[2]))/1447897662623066809234291982978316756042516907577264482757961270265625042889284714772305920),(14888843069 (-35767220986555330532391000353947000334+595832541479474679188917055439155995017 Sqrt[2]))/15763155358205353725754055779131666021129371899328,1/7,0,0},{(8748669237135351323958627345635015669582802368431886516710787383655022383937671842128362317084840876+490805042714523143028117037729025259575231330589691342309785597112750711564398869291156972175352081 Sqrt[2])/80783032309868648466360924890865686710401776157819388328338561757045148771084437600571070161730758374,(8748669237135351323958627345635015669582802368431886516710787383655022383937671842128362317084840876+490805042714523143028117037729025259575231330589691342309785597112750711564398869291156972175352081 Sqrt[2])/80783032309868648466360924890865686710401776157819388328338561757045148771084437600571070161730758374,(4863954461865280546672022797211485519620812043789025354786097990557310437898915513257711486297303279820595225+3070322227631004041595196741798622356490548372740414016442345455478762420198479084587482887629863544938512091 Sqrt[2])/19130979265099217139836428820525653385433013654009737361589682642354000909803080632976636822531816185926996411,(178292 (2956426498946954882427146400196412432114851255016804572757661284418314049887339432915309354387568337+9407517717789896185826787411421403014915216576655614434323211362033020078226433892993995126554001433 Sqrt[2]))/6323816943764982604919432741844302254220316640298338447424835122184129813523646402029104228865526361654281,-((6190569600 (949507024249937775683045011785949588121747359402374510714+1028185674366719166917179791315307768505793227715062971915 Sqrt[2]))/17576321104504878697044539819121514035601347963532490470491546925549),-((2229591541095536640 (86438362801027329798045134572495842098846+5552149991058853491959902349827504006065515 Sqrt[2]))/163450340051231796671754493317936824570160776031521557738076431),1/7,0},{(7 (78122963998831398+7291501484615491 Sqrt[2]))/6549206427786543840,(7 (78122963998831398+7291501484615491 Sqrt[2]))/6549206427786543840,-((51040510392308437 (621530911918+552543167735 Sqrt[2]))/159374773025236421819548757520),-((348689533 (-2243353729153278753820+113098479527870880901 Sqrt[2]))/1005171204044666768226726153600),(25745372 (-9060920107628868626522+5258754826229930572547 Sqrt[2]))/1015163428193340210320222218455,(316940672 (-391400711407702267763070+10136725570428786358601 Sqrt[2]))/533853761969074944524234832240615,(3939040643 (839845713929269547329580+89703237649888547251567 Sqrt[2]))/5772023340401947124972495653603200,1/7}},
		{(7 (78122963998831398+7291501484615491 Sqrt[2]))/6549206427786543840,(7 (78122963998831398+7291501484615491 Sqrt[2]))/6549206427786543840,-((51040510392308437 (621530911918+552543167735 Sqrt[2]))/159374773025236421819548757520),-((348689533 (-2243353729153278753820+113098479527870880901 Sqrt[2]))/1005171204044666768226726153600),(25745372 (-9060920107628868626522+5258754826229930572547 Sqrt[2]))/1015163428193340210320222218455,(316940672 (-391400711407702267763070+10136725570428786358601 Sqrt[2]))/533853761969074944524234832240615,(3939040643 (839845713929269547329580+89703237649888547251567 Sqrt[2]))/5772023340401947124972495653603200,1/7},
		{0,2/7,1/7 (2+Sqrt[2]),150/203,27/46,473/532,30/83,1},
		{(36495693632826090457138845616996623794342806733352236260740271062271011236752099335846945579862178581442280163581599537826002535214145437882220422149326421731061068747114-505422384008627178147105702442768667675173483873155001796634411796422967785256498255884650153985045820802505849300379143014275665694028400034556973414510531420292701747 Sqrt[2])/(9717253013381395680 (31855756520966302730850715722535907161579292362332006518799530595733577407023278205333713645776071927034586371554719912801094079968993489159247038760798+3756089776667781398372040522489892417966033712954776370152607157959323179298308747138080451686996970545031588660564057636042668998355140335077809479605 Sqrt[2])),(36495693632826090457138845616996623794342806733352236260740271062271011236752099335846945579862178581442280163581599537826002535214145437882220422149326421731061068747114-505422384008627178147105702442768667675173483873155001796634411796422967785256498255884650153985045820802505849300379143014275665694028400034556973414510531420292701747 Sqrt[2])/(9717253013381395680 (31855756520966302730850715722535907161579292362332006518799530595733577407023278205333713645776071927034586371554719912801094079968993489159247038760798+3756089776667781398372040522489892417966033712954776370152607157959323179298308747138080451686996970545031588660564057636042668998355140335077809479605 Sqrt[2])),(-22187139074404681047720668233743113178523714356786893393114458181595961700175481298067841130596110902986306534355224200547714134694408171922387602375226234460261027940581705880707-12148221855595568805337827073296206117115328631970316574877245617706119288349950355460777827863581054153922392893634135126950133247260071824120985821944536985363014591752751561620 Sqrt[2])/68180446162512159825170861441619875828264539069699362979924571549100632477897874844969085709779647884414304733276415819021602885531775634672760611025237251561490015757194817292560,-((24389 (-86169613561025736699155226419915238139234273864696427965174079931633717075180010956323359740697041143141698019202574785760231246937839940416168491387591285292054763604492345695+10258305190489407252088985071799758869128335141058425855444919532694274897877537007744424979781775662724752027527961406488059599053243645109012948525855013937645886143807596523 Sqrt[2]))/3010082079014856333251804796492889092653286613718983371847973920491249496346245302886846400057133131910257493615891135507927885318315337969555469852744841906175132943979071584425600),(12167 (-3657281865672306374146847585879354181555599964835670005567104302472705582854723843327296653713501696378505498847282533964916897384048142237294418652265562826838066233785473044+7859660102039423875346905751555710181580721369379502452906944484860415236985015039701727834665754815137324825584241907317741683203154705041752556890060131154340654307617220839 Sqrt[2]))/434286393939695063488110213185528822727677218519978464400125161664922389475507881400123447583817099684668280633532261798855143978464516773940812839226740955864527178933979243016615,(39617584 (-9260502758961458339880807677445067795593027703884356695790684459664560598889100934064811309866572969972041544356887138337735121142025491602451182480879185685909688794372876160+590913302241231404376037532453659336638868060926876596559945769579122118172588287752765395206171428715948157648952826222981958287660167240437315340788472328399367554384704707 Sqrt[2]))/1598676559029607494390761646331605068758739691568617478444419227583484226581744993628500447398998253156962844081641205366852797195080329329684706265646575692102214204520386597585201665,(3939040643 (133652025883664605541444614553130860945419604077560960314216887892222654214216253820787525151272381204239630535792612262327217258077842333126127910370674068832845+13907240164519640126783998399733503600584447371022584171891913940611941561975330492521998381972779734233747605030326697808450347498706269779427797433786415161791 Sqrt[2]))/937855491280775543087235146288737494461998631965799676773805251011024609795854120949290268299940160812954650885067919349964978873464024415027039714859840923974690155798400,36/233}
	]}
];
