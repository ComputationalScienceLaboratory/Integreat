(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


SetAttributes[rkOrderConditions, Listable];
rkOrderConditions[rk_, t_, opts___] := (phi[First[t], RKA[rk], RKB[rk, opts], RKC[rk]] - SafePow[one[rk, opts], BTreeOrder[t]] / BTreeGamma[t]) / BTreeSigma[t];


phi[\[FormalY], __] := 1;
phi[\[FormalF], _, b_, _] := Total[b];
phi[\[FormalF][t_], a_, b_, c_] := b . phiBranches[t, a, c];


phiBranches[\[FormalF], _, c_] := c;
phiBranches[Power[t_, p_], a_, c_] := phiBranches[t, a, c]^p;
phiBranches[t_Times, a_, c_] := Map[phiBranches[#, a, c] &, t];
phiBranches[\[FormalF][t_], a_, c_] := a . phiBranches[t, a, c];


(* ::Section:: *)
(*Package Definitions*)


RKOrderConditions[rk_RK, p:_Integer?Positive | {_Integer?NonNegative}, opts:OptionsPattern[RKB]] := rkOrderConditions[rk, BTree[p], opts];
RKOrderConditions[rk_RK, t_BTree, opts:OptionsPattern[RKB]] := rkOrderConditions[rk, t, opts];


Options[RKSimplifyingAssumptionB] = {Embedded -> False, DenseOutput -> False};
RKSimplifyingAssumptionB[rk_RK, {p_Integer?Positive}, opts:OptionsPattern[]] := RKB[rk, opts] . SafePow[RKC[rk], p - 1] - SafePow[one[rk, opts], p] / p;
RKSimplifyingAssumptionB[rk_RK, p_Integer?Positive, opts:OptionsPattern[]] := Table[RKSimplifyingAssumptionB[rk, {k}, opts], {k, p}];


Options[RKSimplifyingAssumptionC] = {Stage -> All};
RKSimplifyingAssumptionC[rk_RK, {p_Integer?Positive}, OptionsPattern[]] := With[{
		stages = OptionValue[Stage],
		c = RKC[rk]
	},
	RKA[rk][[stages]] . SafePow[c, p - 1] - SafePow[c[[stages]], p] / p
];
RKSimplifyingAssumptionC[rk_RK, p_Integer?Positive, opts:OptionsPattern[]] := Table[RKSimplifyingAssumptionC[rk, {k}, opts], {k, p}];


RKSimplifyingAssumptionD[rk_RK, {p_Integer?Positive}, opts:OptionsPattern[RKB]] := With[{
		b = RKB[rk, opts],
		c = RKC[rk]
	},
	(b * SafePow[c, p - 1]) . RKA[rk] - b * (SafePow[one[rk, opts], p] - SafePow[c, p]) / p
];
RKSimplifyingAssumptionD[rk_RK, p_Integer?Positive, opts:OptionsPattern[RKB]] := Table[RKSimplifyingAssumptionD[rk, {k}, opts], {k, p}];


RKOrder[rk_RK, opts:OptionsPattern[RKB]] := CountZeros[RKOrderConditions[rk, {#}, opts] &] - 1;


RKErrorA[rk_RK, p_Integer?NonNegative, opts:OptionsPattern[RKB]] := Norm[RKOrderConditions[rk, {p}, opts]];
RKErrorA[rk_RK, opts:OptionsPattern[RKB]] := RKErrorA[rk, RKOrder[rk, opts] + 1, opts];


RKErrorB[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat, Embedded -> True] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorB[rk_?RKPairQ] := RKErrorB[rk, RKOrder[rk] + 1];


RKErrorC[rk_?RKPairQ, pHat_Integer?Positive] := Norm[
		RKOrderConditions[rk, {pHat}] - RKOrderConditions[RKEmbedded[rk], {pHat}]
	] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorC[rk_?RKPairQ] := RKErrorC[rk, RKOrder[rk] + 1];


RKErrorD[rk_RK] := Max[Abs[List @@ rk]];


RKErrorE[rk_?RKPairQ, pHat_Integer?Positive] := RKErrorA[rk, pHat] / RKErrorA[rk, pHat - 1, Embedded -> True];
RKErrorE[rk_?RKPairQ] := RKErrorE[rk, RKOrder[rk] + 1];
