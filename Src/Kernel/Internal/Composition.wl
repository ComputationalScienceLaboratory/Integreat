(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Internal`Composition`"];

AddComposition::usage = "AddComposition[type, op, composer] defines op and overloads the power function for type to provide composision via composer.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


(* ::Section:: *)
(*Package Definitions*)


AddComposition[type_Symbol, op_Symbol, composer_] := (
	op[args__:{_type, _}] := composer[List[args]];
	op[args__type] := With[{
			m = List[args]
		},
		composer[Map[{#, 1 / Length[m]} &, m]]
	];
	m1_type[m2_type] := composer[{{m1, 1/2}, {m2, 1/2}}];
	type /: Power[a_type, d_Integer?Positive] := composer[ConstantArray[{a, 1 / d}, d]];
);


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
