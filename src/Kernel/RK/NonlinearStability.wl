(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


rkAlgMat[A_, b_, s_] := Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}];

rkMonotonicityMat[A_, b_, s_, r_] := Inverse[IdentityMatrix[s + 1] - ArrayFlatten[{{r * A, 0}, {{r * b}, 0}}]] . ArrayFlatten[{{A, 1}, {{b}, 1}}];


(* ::Section:: *)
(*Package Definitions*)


RKAlgebraicStabilityMatrix[rk_RK, opts:OptionsPattern[RKB]] := rkAlgMat[RKA[rk], RKB[rk, opts], RKStages[rk]];


RKAlgebraicallyStableQ[rk_RK, opts:OptionsPattern[RKB]] := And[
	Element[RKB[rk], NonNegativeReals],
	PositiveSemidefiniteMatrixQ[RKAlgebraicStabilityMatrix[rk, opts]]
];


RKAbsoluteMonotonicityRadius[rk_RK, opts:OptionsPattern[RKB]] := PiecewiseExpand[Max[
	Quiet[
		MaxValue[{r, rkMonotonicityMat[RKA[rk], RKB[rk, opts], RKStages[rk], -r] >= 0}, r],
		MaxValue::infeas
	],
	0
]];
