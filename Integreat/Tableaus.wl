(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Tableaus`"];
Integreat`Tableaus::usage = "Functions for generating matrices (tableaus) with specific structures";

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


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

SetAttributes[Tableau, HoldFirst];
Tableau[expr_, s_Integer] := Table[expr, {i, s}, {j, s}];
Tableau[expr_, {s_Integer, t_Integer}] := Table[expr, {i, s}, {j, t}];


(* ::Section:: *)
(*Package Definitions*)


TableauZeros[s: _Integer | {_Integer, _Integer}] := Tableau[0, s];

TableauZerosQ[x_] := SquareMatrixQ[x] && MatchQ[x, {{0..}..}];

TableauExplicit[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := Tableau[If[i > j, Subscript[entry, i, j], 0], s];

TableauExplicitQ[x_] := SquareMatrixQ[x] && LowerTriangularMatrixQ[x, -1];

TableauFirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := Tableau[Subscript[entry, i, j], s];

TableauFirkQ[x_] := SquareMatrixQ[x] && !LowerTriangularMatrixQ[x];

TableauDirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := Tableau[If[i < j, 0, Subscript[entry, i, j]], s];

TableauDirkQ[x_] := SquareMatrixQ[x] && LowerTriangularMatrixQ[x] && !MatchQ[Diagonal[x], {0..}];

TableauEdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := Tableau[If[i == 1 || i < j, 0, Subscript[entry, i, j]], s];

TableauEdirkQ[x_] := TableauDirkQ[x] && x[[1,1]] === 0;

TableauSdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := Tableau[Which[i > j, Subscript[entry, i, j], i == j, diagEntry, True, 0], s];

TableauSdirkQ[x_] := TableauDirkQ[x] && SameQ @@ Diagonal[x];

TableauEsdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := Tableau[Which[i > j, Subscript[entry, i, j], i == j > 1, diagEntry, True, 0], s];

TableauEsdirkQ[x_] := TableauEdirkQ[x] && SameQ @@ Rest[Diagonal[x]];

TableauDiagonal[s: _Integer | {_Integer, _Integer}, diagEntry_:\[FormalGamma]] := Tableau[If[i == j, diagEntry, 0], s];

TableauDiagonalQ[x_] := SquareMatrixQ[x] && DiagonalMatrixQ[x];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
