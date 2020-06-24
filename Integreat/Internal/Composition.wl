(* ::Package:: *)

BeginPackage["Integreat`Internal`Composition`"];


Integreat`Internal`Composition::usage = "An internal package for generating composite methods";

AddComposition::usage = "Defines functions and overloads the power function for generic method composition";


Begin["`Private`"];


AddComposition[type_Symbol, op_Symbol, composer_] := (
	op[args___:{_type, _}] := composer[List[args]];
	op[args___type] := With[{
			m = List[args]
		},
		composer[Map[{#, 1 / Length[m]} &, m]]
	];
	m1_type[m2_type] := composer[{{m1, 1/2}, {m2, 1/2}}];
	type /: Power[a_type, d_Integer?Positive] := composer[ConstantArray[{a, 1 / d}, d]];
);


End[];
EndPackage[];
