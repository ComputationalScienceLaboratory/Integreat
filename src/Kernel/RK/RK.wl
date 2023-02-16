(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


BeginPackage["Integreat`RK`"];


RK
RKCollocation
RKA
RKB
RKDenseOutput
RKC
RKBHat
RKStages
RKPrimary
RKEmbedded
RKPairQ
RKExtrapolate
RKCompose


RKOrderConditions
RKSimplifyingAssumptionB
RKSimplifyingAssumptionC
RKSimplifyingAssumptionD
RKOrder
RKErrorA
RKErrorB
RKErrorC
RKErrorD
RKErrorE


RKLinearStability
RKLinearStabilityPlot
RKOrderStarPlot
RKLinearStabilityP
RKLinearStabilityQ
RKEPolynomial
RKIStable
RKAStable


RKAlgebraicStabilityMatrix
RKAlgebraicallyStableQ
RKAbsoluteMonotonicityRadius


RKDissipationError
RKDissipationOrder
RKDispersionError
RKDispersionOrder


RKReflection
RKTranspose


RKDJReduce
RKDJReducibleQ


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


Needs["Integreat`BTrees`"];
Needs["Integreat`Internal`MathUtils`"];
Needs["Integreat`Internal`Catalog`"];
Needs["Integreat`Internal`Composition`"];


one[rk_, OptionsPattern[RKB]] := one[rk, OptionValue[Embedded], OptionValue[Stage], OptionValue[DenseOutput]];
one[_, True, _, _] := 1;
one[rk_, False, i_Integer, _] := RKC[rk][[i]];
one[_, False, None, True] := \[FormalTheta];
one[_, False, None, False] := 1;
one[_, False, None, do_] := do;


<<Integreat`RK`Core`;
<<Integreat`RK`Catalog`;
<<Integreat`RK`Validation`;
<<Integreat`RK`OrderConditions`;
<<Integreat`RK`LinearStability`;
<<Integreat`RK`NonlinearStability`;
<<Integreat`RK`DissipationDispersion`;
<<Integreat`RK`Symmetry`;
<<Integreat`RK`Simplify`;


End[];


EndPackage[];
