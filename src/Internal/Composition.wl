(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Internal`Composition`"];


CSL`OdeUtils`Internal`Composition::usage = "An internal package for generating composite methods";

AddComposition::usage = "Defines functions and overloads the power function for generic method composition";


Begin["`Private`"];


AddComposition[type_Symbol, op_Symbol, composer_] := (
	op[args__:{_type, _}] := composer[List[args]];
	op[args__type] := With[{
			m = List[args]
		},
		composer[Map[{#, 1 / Length[m]} &, m]]
	];
	m1_type[m2_type] := composer[{{m1, 1/2}, {m2, 1/2}}];
	type /: Power[a_type, d_Integer /; d > 0] := composer[ConstantArray[{a, 1 / d}, d]];
);


End[];


EndPackage[];
