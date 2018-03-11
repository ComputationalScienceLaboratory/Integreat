#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*OdeUtils Installer*)


(* ::Text:: *)
(*This notebook will copy all OdeUtils files to the current user's Mathematica applications directory.  If OdeUtils is already installed, it will be overwritten by this version.*)


scriptDir = DirectoryName[ExpandFileName[First[$ScriptCommandLine]]];
srcDir = FileNameJoin[{scriptDir, "src"}];
cslDir = FileNameJoin[{$UserBaseDirectory, "Applications", "CSL"}];
packageDir = FileNameJoin[{cslDir, "OdeUtils"}];
Quiet[DeleteDirectory[packageDir, DeleteContents->True], {DeleteDirectory::nodir}];
Quiet[CreateDirectory[cslDir], {CreateDirectory::filex}];
CopyFile[srcDir, packageDir, OverwriteTarget->True];
success = Check[<<CSL`OdeUtils`; True, False];
Print["Installation " <> If[success, "Succeeded", "Failed"]];
