(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Tableaus`"];


CSL`OdeUtils`Tableaus::usage = "Package containing functions for generating tableaus with general or specific structure";

TableauPattern::usage = "Creates a Butcher tableau from a pattern matrix.  A False in the pattern matrix indicates a 0, a True indicates an indexed entry, and anything else will be copied";
TableauZeros::usage = "Creates a Butcher tableau of zeros";
TableauExplicit::usage = "Creates an explicit Butcher tableau";
TableauFIRK::usage = "Creates a fully implicit Butcher tableau";
TableauDIRK::usage = "Creates a DIRK Butcher tableau";
TableauEDIRK::usage = "Creates a DIRK Butcher tableau with the first stage explicit";
TableauSDIRK::usage = "Creates an SDIRK Butcher tableau";
TableauESDIRK::usage = "Creates an SDIRK Butcher tableau with the first stage explicit";


Begin["`Private`"];


TableauPattern[pattern_List/;MatrixQ[pattern], entry_:\[FormalA]] := MapIndexed[Switch[#1, True, Subscript[entry, First[#2], Last[#2]], False, 0, _, #1] &, pattern, {2}];

TableauZeros[{s_Integer, t_Integer}, entry_:\[FormalA]] := ConstantArray[0, {s, t}];
TableauZeros[s_Integer, entry_:\[FormalA]] := TableauZeros[{s, s}, entry];

TableauExplicit[{s_Integer, t_Integer}, entry_:\[FormalA]] := LowerTriangularize[Table[Subscript[entry, i,j], {i, s}, {j, t}], -1];
TableauExplicit[s_Integer, entry_:\[FormalA]] := TableauExplicit[{s, s}, entry];

TableauFIRK[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[Subscript[entry, i,j], {i, s}, {j, t}];
TableauFIRK[s_Integer, entry_:\[FormalA]] := TableauFIRK[{s, s}, entry];

TableauDIRK[{s_Integer, t_Integer}, entry_:\[FormalA]] := LowerTriangularize[Table[Subscript[entry, i,j], {i, s}, {j, t}]];
TableauDIRK[s_Integer, entry_:\[FormalA]] := TableauDIRK[{s, s}, entry];

TableauEDIRK[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[If[i==1 || j>i, 0, Subscript[entry, i,j]], {i, s}, {j, t}];
TableauEDIRK[s_Integer, entry_:\[FormalA]] := TableauEDIRK[{s, s}, entry];

TableauSDIRK[{s_Integer, t_Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := LowerTriangularize[Table[If[i==j, diagEntry, Subscript[entry, i,j]], {i, s}, {j, t}]];
TableauSDIRK[s_Integer, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := TableauSDIRK[{s, s}, entry, diagEntry];

TableauESDIRK[s_Integer, t_Integer, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := LowerTriangularize[Table[Switch[i, 1, 0, j, diagEntry, _, Subscript[entry, i,j]], {i, s}, {j, t}]];
TableauESDIRK[s_Integer, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := TableauESDIRK[{s, s}, entry, diagEntry];


End[];


EndPackage[];
