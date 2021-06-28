(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Tableaus`"];
Integreat`Tableaus::usage = "This packages contains functions to create matrices (tableaus) and check if they have certains structures such as explicit or singly diagonally implicit.";

TableauZeros::usage =
	"TableauZeros[s] creates an s by s matrix of zeros.\n" <>
	"TableauZeros[{s, t}] creates an s by t matrix of zeros.";
TableauZerosQ::usage = "TableauZerosQ[expr] yields True if expr is a square matrix of zeros, and yields False otherwise.";
TableauExplicit::usage =
	"TableauExplicit[s] creates an s by s, strictly lower triangular matrix with subscripted elements.\n" <>
	"TableauExplicit[s, entry] creates an explicit tableau where the elements are entry with subscripts.\n" <>
	"TableauExplicit[{s, t}, \[Ellipsis]] creates an explicit tableau of size s by t.";
TableauExplicitQ::usage = "TableauExplicitQ[expr] yields True if expr is a square, strictly lower triangular matrix, and yields False otherwise.";
TableauFirk::usage =
	"TableauFirk[s] creates an s by s matrix with subscripted elements.\n" <>
	"TableauFirk[s, entry] creates a fully implicit tableau where the elements are entry with subscripts.\n" <>
	"TableauFirk[{s, t}, \[Ellipsis]] creates a fully implicit tableau of size s by t.";
TableauFirkQ::usage = "TableauFirkQ[expr] yields True if expr is a square matrix that is not lower triangular, and yields False otherwise.";
TableauDirk::usage =
	"TableauDirk[s] creates an s by s, lower triangular matrix with subscripted elements.\n" <>
	"TableauDirk[s, entry] creates a diagonally implicit tableau where the elements are entry with subscripts.\n" <>
	"TableauDirk[{s, t}, \[Ellipsis]] creates a diagonally implicit tableau of size s by t.";
TableauDirkQ::usage = "TableauDirkQ[expr] yields True if expr is a square, lower triangular matrix with a nonzero diagonal element, and yields False otherwise.";
TableauEdirk::usage =
	"TableauEdirk[s] creates an s by s, lower triangular matrix with the first row zeros and subscripted elements.\n" <>
	"TableauEdirk[s, entry] creates an explicit first stage diagonally implicit tableau where the elements are entry with subscripts.\n" <>
	"TableauEdirk[{s, t}, \[Ellipsis]] creates an explicit first stage diagonally implicit tableau of size s by t.";
TableauEdirkQ::usage = "TableauEdirkQ[expr] yields True if expr is a square, lower triangular matrix with the first diagonal element zero and one of the other diagonal elements nonzero, and yields False otherwise.";
TableauSdirk::usage =
	"TableauSdirk[s] creates an s by s, lower triangular matrix with a single diagonal value and subscripted elements below the diagonal.\n" <>
	"TableauSdirk[s, entry] creates a singly diagonally implicit tableau where the elements below the diagonal are entry with subscripts.\n" <>
	"TableauSdirk[s, entry, diagEntry] creates a singly diagonally implicit tableau where the diagonal elements are diagEntry, and the elements below the diagonal are entry with subscripts.\n" <>
	"TableauSdirk[{s, t}, \[Ellipsis]] creates a singly diagonally implicit tableau of size s by t.";
TableauSdirkQ::usage = "TableauSdirkQ[expr] yields True if expr is a square, lower triangular matrix with a single, nonzero diagonal value, and yields False otherwise.";
TableauEsdirk::usage =
	"TableauEsdirk[s] creates an s by s, lower triangular matrix with the first row zeros, a single diagonal value, and subscripted elements below the diagonal.\n" <>
	"TableauEsdirk[s, entry] creates an explicit first stage singly diagonally implicit tableau where the elements below the diagonal are entry with subscripts.\n" <>
	"TableauEsdirk[s, entry, diagEntry] creates an explicit first stage singly diagonally implicit tableau where the diagonal elements are diagEntry, and the elements below the diagonal are entry with subscripts.\n" <>
	"TableauEsdirk[{s, t}, \[Ellipsis]] creates an explicit first stage singly diagonally implicit tableau of size s by t.";
TableauEsdirkQ::usage = "TableauEsdirkQ[expr] yields True if expr is a square, lower triangular matrix with the first diagonal element zero and a single, nonzero value for rest of the diagonal, and yields False otherwise.";
TableauDiagonal::usage =
	"TableauDiagonal[s] creates an s by s, diagonal matrix with a single diagonal value.\n" <>
	"TableauDiagonal[s, diagEntry] creates a diagonal tableau where the diagonal elements are diagEntry.\n" <>
	"TableauDiagonal[{s, t}, \[Ellipsis]] creates a diagonal tableau of size s by t.";
TableauDiagonalQ::usage = "TableauDiagonalQ[expr] yields True if expr is a square, diagonal matrix, and yields False otherwise.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

SetAttributes[tableau, HoldFirst];
tableau[expr_, s_Integer] := Table[expr, {i, s}, {j, s}];
tableau[expr_, {s_Integer, t_Integer}] := Table[expr, {i, s}, {j, t}];


(* ::Section:: *)
(*Package Definitions*)


TableauZeros[s: _Integer | {_Integer, _Integer}] := tableau[0, s];

TableauZerosQ[expr_] := SquareMatrixQ[expr] && MatrixQ[expr, PossibleZeroQ];

TableauExplicit[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := tableau[If[i > j, Subscript[entry, i, j], 0], s];

TableauExplicitQ[expr_] := SquareMatrixQ[expr] && LowerTriangularMatrixQ[expr, -1];

TableauFirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := tableau[Subscript[entry, i, j], s];

TableauFirkQ[expr_] := SquareMatrixQ[expr] && !LowerTriangularMatrixQ[expr];

TableauDirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := tableau[If[i < j, 0, Subscript[entry, i, j]], s];

TableauDirkQ[expr_] := SquareMatrixQ[expr] && LowerTriangularMatrixQ[expr] && !VectorQ[Diagonal[expr], PossibleZeroQ];

TableauEdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA]] := tableau[If[i == 1 || i < j, 0, Subscript[entry, i, j]], s];

TableauEdirkQ[expr_] := TableauDirkQ[expr] && PossibleZeroQ[expr[[1, 1]]];

TableauSdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := tableau[Which[i > j, Subscript[entry, i, j], i == j, diagEntry, True, 0], s];

TableauSdirkQ[expr_] := TableauDirkQ[expr] && SameQ @@ Diagonal[expr];

TableauEsdirk[s: _Integer | {_Integer, _Integer}, entry_:\[FormalA], diagEntry_:\[FormalGamma]] := tableau[Which[i > j, Subscript[entry, i, j], i == j > 1, diagEntry, True, 0], s];

TableauEsdirkQ[expr_] := TableauEdirkQ[expr] && SameQ @@ Rest[Diagonal[expr]];

TableauDiagonal[s: _Integer | {_Integer, _Integer}, diagEntry_:\[FormalGamma]] := tableau[If[i == j, diagEntry, 0], s];

TableauDiagonalQ[expr_] := SquareMatrixQ[expr] && DiagonalMatrixQ[expr];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
