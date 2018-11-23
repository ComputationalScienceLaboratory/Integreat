(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Tableaus`"];


CSL`OdeUtils`Tableaus::usage = "Package containing functions for generating tableaus with general or specific structure";

TableauPattern::usage = "Creates a Butcher tableau from a pattern matrix.  A False in the pattern matrix indicates a 0, a True indicates an indexed entry, and anything else will be copied";
TableauQ::usage = "Returns True if input is a Butcher tableau, and False otherwise";
TableauZeros::usage = "Creates a Butcher tableau of zeros";
TableauZerosQ::usage = "Returns True if input is a Butcher tableau of zeros, and False otherwise";
TableauExplicit::usage = "Creates an explicit Butcher tableau";
TableauExplicitQ::usage = "Returns True if input is an explicit Butcher tableau, and False otherwise";
TableauFirk::usage = "Creates a fully-implicit Butcher tableau";
TableauFirkQ::usage = "Returns True if input is a fully-implicit Butcher tableau, and False otherwise";
TableauDirk::usage = "Creates a DIRK Butcher tableau";
TableauDirkQ::usage = "Returns True if input is a DIRK Butcher tableau, and False otherwise";
TableauEdirk::usage = "Creates a DIRK Butcher tableau with the first stage explicit";
TableauEdirkQ::usage = "Returns True if input is an EDIRK Butcher tableau, and False otherwise";
TableauSdirk::usage = "Creates an SDIRK Butcher tableau";
TableauSdirkQ::usage = "Returns True if input is an SDIRK Butcher tableau, and False otherwise";
TableauEsdirk::usage = "Creates an SDIRK Butcher tableau with the first stage explicit";
TableauEsdirkQ::usage = "Returns True if input is an ESDIRK Butcher tableau, and False otherwise";
TableauDiagonal::usage = "Creates a diagonal Butcher tableau";
TableauDiagonalQ::usage = "Returns True if input is a diagonal Butcher tableau, and False otherwise";


Begin["`Private`"];


TableauPattern[pattern_List?MatrixQ, entry_:\[FormalA]] := MapIndexed[Switch[#1, True, Subscript[entry, First[#2], Last[#2]], False, 0, _, #1] &, pattern, {2}];

TableauQ[x_] := SquareMatrixQ[x];

TableauZeros[{s_Integer, t_Integer}] := ConstantArray[0, {s, t}];
TableauZeros[s_Integer] := TableauZeros[{s, s}];

TableauZerosQ[x_] := TableauQ[x] && AllTrue[x, # === 0 &, 2];

TableauExplicit[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[If[i > j, Subscript[entry, i, j], 0], {i, s}, {j, t}];
TableauExplicit[s_Integer, entry_:\[FormalA]] := TableauExplicit[{s, s}, entry];

TableauExplicitQ[x_] := TableauQ[x] && x === LowerTriangularize[x, -1];

TableauFirk[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[Subscript[entry, i,j], {i, s}, {j, t}];
TableauFirk[s_Integer, entry_:\[FormalA]] := TableauFirk[{s, s}, entry];

TableauFirkQ[x_] := TableauQ[x] && x =!= LowerTriangularize[x];

TableauDirk[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[If[i < j, 0, Subscript[entry, i, j]], {i, s}, {j, t}];
TableauDirk[s_Integer, entry_:\[FormalA]] := TableauDirk[{s, s}, entry];

TableauDirkQ[x_] := TableauQ[x] && x === LowerTriangularize[x];

TableauEdirk[{s_Integer, t_Integer}, entry_:\[FormalA]] := Table[If[i == 1 || i < j, 0, Subscript[entry, i, j]], {i, s}, {j, t}];
TableauEdirk[s_Integer, entry_:\[FormalA]] := TableauEdirk[{s, s}, entry];

TableauEdirkQ[x_] := TableauDirkQ[x] && x[[1,1]] === 0;

TableauSdirk[{s_Integer, t_Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := LowerTriangularize[Table[If[i == j, diagEntry, Subscript[entry, i,j]], {i, s}, {j, t}]];
TableauSdirk[s_Integer, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := TableauSdirk[{s, s}, entry, diagEntry];

TableauSdirkQ[x_] := TableauDirkQ[x] && SameQ @@ Diagonal[x];

TableauEsdirk[{s_Integer, t_Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := Table[Which[i == 1, 0, i == j, diagEntry, i > j, Subscript[entry, i,j], True, 0], {i, s}, {j, t}];
TableauEsdirk[s_Integer, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := TableauEsdirk[{s, s}, entry, diagEntry];

TableauEsdirkQ[x_] := TableauEdirkQ[x] && SameQ @@ Rest[Diagonal[x]];

TableauDiagonal[{s_Integer, t_Integer}, diagEntry_:\[FormalGamma]] := diagEntry * IdentityMatrix[{s, t}];
TableauDiagonal[s_Integer, diagEntry_:\[FormalGamma]] := TableauDiagonal[{s, s}, diagEntry];

TableauDiagonalQ[x_] := TableauQ[x] && DiagonalMatrix[Diagonal[x]] === x


End[];


EndPackage[];
