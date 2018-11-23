(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Trees`"];


CSL`OdeUtils`Trees::usage = "Package containing functions for ";


Begin["`Private`"];


SetAttributes[NTreeHeight, Listable];
NTreeHeight[t_Symbol] := 1;
NTreeHeight[t_Times] := Max[NTreeHeight[List @@ t]];
NTreeHeight[Power[t_, _]] := NTreeHeight[t];
NTreeHeight[t_?(Head[#] === Symbol &


End[];


EndPackage[];
