(* ::Package:: *)

BeginPackage["Integreat`Internal`MathUtils`"];


Integreat`Internal`MathUtils::usage = "Package containing math utilities";
Pow::usage = "Power but with 0^y=1 for y<=0";


Begin["`Private`"];


SetAttributes[Pow, {Listable, NumericFunction, OneIdentity}];
Pow[0|0.0, _?NonPositive] := 1;
Pow[x_, y_] := x^y;


End[];
EndPackage[];
