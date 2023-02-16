(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


RKReflection[rk:HoldPattern[RK[A_, b_, c_, d___]]] := RK[ConstantArray[RKB[rk], Length[c]] - A, b, 1 - c, d];


RKTranspose[rk:HoldPattern[RK[A_, bDO_, c_, d___]]] /; NoneTrue[RKB[rk], ZeroQ] := With[{
		s = RKStages[rk],
		b = RKB[rk]
	},
	RK[Transpose[b * A] / b, bDO, 1 - c, d]
];
