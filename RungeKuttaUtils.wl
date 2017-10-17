(* ::Package:: *)

BeginPackage["CSL`RungeKuttaUtils`"];


CSL`RungeKuttaUtils::usage = "Package containing utility functions for analyzing Runge-Kutta methods";

TableauPattern::usage = "Creates a Butcher tableau from a pattern matrix.  A False in the pattern matrix indicates a 0, a True indicates an indexed entry, and anything else will be copied";
TableauZeros::usage = "Creates a Butcher tableau of zeros";
TableauExplicit::usage = "Creates an explicit Butcher tableau";
TableauFIRK::usage = "Creates a fully implicit Butcher tableau";
TableauDIRK::usage = "Creates a DIRK Butcher tableau";
TableauEDIRK::usage = "Creates a DIRK Butcher tableau with the first stage explicit";
TableauSDIRK::usage = "Creates an SDIRK Butcher tableau";
TableauESDIRK::usage = "Creates an SDIRK Butcher tableau with the first stage explicit";

RKStability::usage = "The linear stability function for a Runge-Kutta method";
RKP::usage = "The numerator of the linear stability function";
RKQ::usage = "The denominator of the linear stability function";
RKE::usage = "The E-polynomial to test for I-stability";
RKInternalStability::usage = "The linear stability function for the stages of a Runge-Kutta method";
RKStabilityPlot::usage = "Plots the region of linear stability";

RKReplace::usage = "Replaces coefficients in expressions from the NumericalDifferentialEquationAnalysis package";

RKMetricA::usage = "The 2-norm of the principal error";
RKMetricB::usage = "The ratio of the embedded second error terms' norm to leading error terms' norm";
RKMetricC::usage = "The ratio of the norm of the difference in second error terms to the norm of embedded leading error terms";
RKMetricD::usage = "The maximum entry in the Butcher tableau by absolute value";
RKMetricE::usage = "The ratio of the second error terms' norm to leading error terms' norm";

RKCatalog::usage = "An association containing the coefficents for optimized Runge-Kutta methods";


Begin["`Private`"];
Needs["NumericalDifferentialEquationAnalysis`"];


RowSum[A_] := A.ConstantArray[1, {Length[A], 1}];


TableauPattern[pattern_, entry_:Global`a] := MapIndexed[Switch[#1, True, Subscript[entry, First[#2], Last[#2]], False, 0, _, #1] &, pattern, {2}];
TableauZeros[s_Integer, t_Integer, entry_:Global`a] := ConstantArray[0, {s, t}];
TableauExplicit[s_Integer, t_Integer, entry_:Global`a] := LowerTriangularize[Table[Subscript[entry, i,j], {i, s}, {j, t}], -1];
TableauFIRK[s_Integer, t_Integer, entry_:Global`a] := Table[Subscript[entry, i,j], {i, s}, {j, t}];
TableauDIRK[s_Integer, t_Integer, entry_:Global`a] := LowerTriangularize[Table[Subscript[entry, i,j], {i, s}, {j, t}]];
TableauEDIRK[s_Integer, t_Integer, entry_:Global`a] := Table[If[i==1 || j>i, 0, Subscript[entry, i,j]], {i, s}, {j, t}];
TableauSDIRK[s_Integer, t_Integer, entry_:Global`a, diagEntry_:Global`\[Gamma]] := LowerTriangularize[Table[If[i==j, diagEntry, Subscript[entry, i,j]], {i, s}, {j, t}]];
TableauESDIRK[s_Integer, t_Integer, entry_:Global`a, diagEntry_:Global`\[Gamma]] := LowerTriangularize[Table[Switch[i, 1, 0, j, diagEntry, _, Subscript[entry, i,j]], {i, s}, {j, t}]];


RKStability[z_, A_, b_] := (1+z*Total[b\[Transpose].Inverse[IdentityMatrix[Dimensions[A]]-z*A], 2]);
RKP[z_,A_,b_] := Det[IdentityMatrix[Dimensions[A]]-z*A+z*ConstantArray[Flatten[b],Length[A]]];
RKQ[z_,A_] := Det[IdentityMatrix[Dimensions[A]]-z*A];
RKE[y_,A_,b_] := RKQ[I*y]*RKQ[-I*y]-RKP[I*y]*RKP[-I*y];
RKInternalStability[z_, A_] := RKQ[z,A].ConstantArray[1, {Length[A], 1}];
RKStabilityPlot[A_, b_, x:{xMin_:-6, xMax_:2}:{}, y:{yMin_:-4, yMax_:4}:{}, args___] := RegionPlot[Abs[RKStability[realPart+imagPart*I, A, b]] <= 1, {realPart, xMin, xMax}, {imagPart, yMin, yMax}, args];


RKReplace[expr_, A_, b_] := With[{
		Asubs = MapIndexed[Subscript[\[FormalA], First[#2], Last[#2]]->#1 &, A, {2}],
		bsubs = MapIndexed[Subscript[\[FormalB], First[#2]]->#1 &, b, {2}],
		csubs = MapIndexed[Subscript[\[FormalC], First[#2]]->#1 &, RowSum[A], {2}]
	},
	ReplaceAll[expr, Flatten[{Asubs, bsubs, csubs}]]
];


RealNorm[x_]:=Sqrt[Total[x^2,-1]];
RKMetricA[A_, b_, p_] := With[{
		s = Length[A]
	},
	RealNorm[RKReplace[ButcherPrincipalError[p-1, s], A, b]]
];

RKMetricB[A_, bHat_, pHat_] := RKMetricA[A, bHat, pHat] / RKMetricA[A, bHat, pHat - 1];

RKMetricC[A_, b_, bHat_, pHat_] := With[{
		s = Length[A]
	},
	RealNorm[RKReplace[ButcherPrincipalError[pHat-1, s], A, b] - RKReplace[ButcherPrincipalError[pHat-1, s], A, bHat]] / RKMetricA[A, bHat, pHat - 1]
];

RKMetricD[A_, b_, bHat_:{}] := Max[Abs[A], Abs[b], Abs[RowSum[A]], Abs[bHat]];

RKMetricE[A_, b_, bHat_, pHat_] := RKMetricA[A, b, pHat] / RKMetricA[A, bHat, pHat - 1];


(*MethodAssociation[A_, b_, bHat_:{}] := (
	method=<|"A"->A, "b"->b|>;
	If[Length[bHat] == 0, method, AppendTo[method, "\!\(\*OverscriptBox[\(b\), \(^\)]\)"->bHat]]
);

RKCatalog=<|
(*First Order Methods*)
"ERK1()1"->MethodAssociation[{{0}}, {{1}}],
"SDIRK1()1"->MethodAssociation[{{0}}, {{1}}],
(*Second Order Methods*)
"ERK2(1)2"->MethodAssociation[{
 {0, 0},
 {2/3, 0}
}, {
 {1/4},
 {3/4}
}, {
 {1},
 {0}
}],
"ERK2(1)3"->MethodAssociation[{
 {0, 0, 0},
 {2/3, 0, 0},
 {1/4, 3/4, 0}
}, {
 {1/4},
 {3/4},
 {0}
}, {
 {11/15},
 {1/10},
 {1/6}
}],
"SDIRK2(1)2"->With[{\[Gamma]=(2-Sqrt[2])/2}, MethodAssociation[{
 {\[Gamma], 0},
 {1-\[Gamma], \[Gamma]}
}, {
 {1-\[Gamma]},
 {\[Gamma]}
}, {
 {3/5},
 {2/5}
}]],
"ESDIRK2(1)3"->With[{\[Gamma]=1-1/Sqrt[2], \[Eta]=1/(2 Sqrt[2])}, MethodAssociation[{
 {0, 0, 0},
 {\[Gamma], \[Gamma], 0},
 {\[Eta], \[Eta], \[Gamma]}
}, {
 {\[Eta]},
 {\[Eta]},
 {\[Gamma]}
}, {
 {3/10},
 {3/10},
 {4/10}
}]],
(*Thrid Order Methods*)
"ERK3(2)3"->MethodAssociation[{
 {0, 0, 0},
 {1/2, 0, 0},
 {0, 3/4, 0}
}, {
 {2/9},
 {1/3},
 {4/9}
}, {
 {1/40},
 {37/40},
 {1/20}
}],
"ERK3(2)4"->MethodAssociation[{
 {0, 0, 0, 0},
 {1/2, 0, 0, 0},
 {0, 3/4, 0, 0},
 {2/9, 1/3, 4/9, 0}
}, {
 {2/9},
 {1/3},
 {4/9},
 {0}
}, {
 {7/24},
 {1/4},
 {1/3},
 {1/8}
}],
"SDIRK3(2)3"->MethodAssociation[{
 {Root[-1+9 #1-18 #1^2+6 #1^3&,2], 0, 0},
 {Root[2-9 #1+24 #1^3&,2], Root[-1+9 #1-18 #1^2+6 #1^3&,2], 0},
 {Root[-7+36 #1-54 #1^2+24 #1^3&,3], Root[-8+27 #1^2+12 #1^3&,2], Root[-1+9 #1-18 #1^2+6 #1^3&,2]}
}, {
 {Root[-7+36 #1-54 #1^2+24 #1^3&,3]},
 {Root[-8+27 #1^2+12 #1^3&,2]},
 {Root[-1+9 #1-18 #1^2+6 #1^3&,2]}
}, {
 {Root[1-6 #1+3 #1^2+4 #1^3&,3]},
 {Root[-2+12 #1-15 #1^2+4 #1^3&,1]},
 {0}
}],
"ESDIRK3(2)4"->MethodAssociation[{
 {0, 0, 0, 0},
 {Root[-1+9 #1-18 #1^2+6 #1^3&,2], Root[-1+9 #1-18 #1^2+6 #1^3&,2], 0, 0},
 {Root[104281-933300 #1+1665000 #1^2+1500000 #1^3&,3], Root[-3267-35100 #1+45000 #1^2+500000 #1^3&,2], Root[-1+9 #1-18 #1^2+6 #1^3&,2], 0},
 {Root[-241+3564 #1-13608 #1^2+7776 #1^3&,2], Root[-107-2196 #1+3528 #1^2+11616 #1^3&,1], Root[31250-101250 #1+42525 #1^2+29403 #1^3&,3], Root[-1+9 #1-18 #1^2+6 #1^3&,2]}
}, {
 {Root[-241+3564 #1-13608 #1^2+7776 #1^3&,2]},
 {Root[-107-2196 #1+3528 #1^2+11616 #1^3&,1]},
 {Root[31250-101250 #1+42525 #1^2+29403 #1^3&,3]},
 {Root[-1+9 #1-18 #1^2+6 #1^3&,2]}
}, {
 {Root[9257-166428 #1+742608 #1^2+46656 #1^3&,2]},
 {Root[-15877+18900 #1+71496 #1^2+34848 #1^3&,2]},
 {Root[-77515625+261545625 #1-164948400 #1^2+5645376 #1^3&,2]},
 {Root[434-3519 #1+4752 #1^2+576 #1^3&,3]}
}],
(*Fourth Order Methods*)
"ERK4()4"->MethodAssociation[{
 {0, 0, 0, 0},
 {2/5, 0, 0, 0},
 {-3/20, 3/4, 0, 0},
 {19/44, -15/44, 40/44, 0}
}, {
 {11/72},
 {25/72},
 {25/72},
 {11/72}
}],
"ERK4(3)5"->MethodAssociation[{
 {0, 0, 0, 0, 0},
 {2/5, 0, 0, 0, 0},
 {-3/20, 3/4, 0, 0, 0},
 {19/44, -15/44, 10/11, 0, 0},
 {11/72, 25/72, 25/72, 11/72, 0}
}, {
 {11/72},
 {25/72},
 {25/72},
 {11/72},
 {0}
}, {
 {1251515/8970912},
 {3710105/8970912},
 {2519695/8970912},
 {61105/8970912},
 {119041/747576}
}],
"SDIRK4(3)5"->MethodAssociation[{
 {1/4, 0, 0, 0, 0},
 {1/2, 1/4, 0, 0, 0},
 {17/50, -1/25, 1/4, 0, 0},
 {371/1360, -137/2720, 15/544, 1/4, 0},
 {25/24, -49/48, 125/16, -85/12, 1/4}
}, {
 {25/24},
 {-49/48},
 {125/16},
 {-85/12},
 {1/4}
}, {
 {973/960},
 {-2203/1920},
 {1015/128},
 {-85/12},
 {23/80}
}],
"ESDIRK4(3)6"->MethodAssociation[{
 {0, 0, 0, 0, 0, 0},
 {1/4, 1/4, 0, 0, 0, 0},
 {1/8 (1-Sqrt[2]), 1/8 (1-Sqrt[2]), 1/4, 0, 0, 0},
 {1/400 (67+50 Sqrt[2]-Sqrt[7 (2827+1500 Sqrt[2])]), 1/400 (67+50 Sqrt[2]-Sqrt[7 (2827+1500 Sqrt[2])]), 1/200 (-77-55 Sqrt[2]+Sqrt[36479+25470 Sqrt[2]]), 1/4, 0, 0},
 {1/240 (36-47 Sqrt[2]-7 Sqrt[186+64 Sqrt[2]]), 1/240 (36-47 Sqrt[2]-7 Sqrt[186+64 Sqrt[2]]), (3497+3413 Sqrt[2]+7 Sqrt[390783+276278 Sqrt[2]])/10680, (7 (187+110 Sqrt[2]+Sqrt[548669-136860 Sqrt[2]]))/10680, 1/4, 0},
 {1/42 (18+Sqrt[2]-Sqrt[186+64 Sqrt[2]]), 1/42 (18+Sqrt[2]-Sqrt[186+64 Sqrt[2]]), (-187-199 Sqrt[2]+Sqrt[390783+276278 Sqrt[2]])/1869, (187+110 Sqrt[2]+Sqrt[548669-136860 Sqrt[2]])/1869, -(3/28), 1/4}
}, {
 {1/42 (18+Sqrt[2]-Sqrt[186+64 Sqrt[2]])},
 {1/42 (18+Sqrt[2]-Sqrt[186+64 Sqrt[2]])},
 {(-187-199 Sqrt[2]+Sqrt[390783+276278 Sqrt[2]])/1869},
 {(187+110 Sqrt[2]+Sqrt[548669-136860 Sqrt[2]])/1869},
 {-(3/28)},
 {1/4}
}, {
 {-(1/32)},
 {-(1/32)},
 {(4788+4497 Sqrt[2]-Sqrt[344173082-228700008 Sqrt[2]])/17088},
 {(97199-47886 Sqrt[2]+Sqrt[7494059143-2444946678 Sqrt[2]])/179424},
 {(1322+75 Sqrt[2]-5 Sqrt[7074526-4914300 Sqrt[2]])/6048},
 {(38-15 Sqrt[2]+Sqrt[7074526-4914300 Sqrt[2]])/1728}
}]
|>;*)


End[];


EndPackage[];
