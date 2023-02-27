(* ::Package:: *)

BeginPackage["Integreat`Internal`Composition`"];


AddComposition


Begin["`Private`"];


AddComposition[type_Symbol, op_Symbol, composer_] := (
	op[args:{{_type, _}..}] := composer[args];
	op[{args__type}] := composer[Map[{#, 1 / Length[m]} &, args]];
	m1_type[m2_type] := composer[{{m1, 1}, {m2, 1}}];
	type /: Power[a_type, p_Integer?Positive] := composer[ConstantArray[{a, 1 / p}, p]];
);


End[];


EndPackage[];
