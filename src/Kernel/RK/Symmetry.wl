(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


RKReflection[rk:HoldPattern[RK[A_, b_, c_, d___]]] := RK[ConstantArray[RKB[rk], Length[b]] - A, b, 1 - c, d];


RKTranspose[rk:HoldPattern[RK[A_, bdo_, c_, d___]]] /; NoneTrue[RKB[rk], ZeroQ] := With[{
		s = RKStages[rk],
		b = RKB[rk]
	},
	RK[Table[b[[j]] * A[[j, i]] / b[[i]], {i, s}, {j, s}], bdo, 1 - c, d]
];
RK /: Transpose[rk_RK] := RKTranspose[rk];
