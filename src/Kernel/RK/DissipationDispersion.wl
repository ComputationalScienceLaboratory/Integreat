(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


errOrder[err_?ZeroQ] := Infinity;
errOrder[err_] := CountZeros[SeriesCoefficient[err, {nu, 0, #}] &] - 1;


(* ::Section:: *)
(*Package Definitions*)


RKDissipationError[rk_RK, nu_, opts:OptionsPattern[RKB]] := 1 - ComplexExpand[Abs[RKLinearStability[rk, nu * I, opts]]];


RKDissipationOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDissipationError[rk, nu, opts]];


RKDispersionError[rk_RK, nu_, opts:OptionsPattern[RKB]] := one[rk, opts] * nu - ComplexExpand[Arg[RKLinearStability[rk, nu * I, opts]], TargetFunctions -> {Re, Im}];


RKDispersionOrder[rk_RK, opts:OptionsPattern[RKB]] := errOrder[RKDispersionError[rk, nu, opts]];
