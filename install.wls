#!/usr/bin/env wolframscript
(* ::Package:: *)

name = "Integreat";
Which[
	Length[$ScriptCommandLine] == 1, (
		srcDir = FileNameJoin[{DirectoryName[ExpandFileName[First[$ScriptCommandLine]]], name}];
		PacletInstall[CreatePacletArchive[srcDir], ForceVersionInstall -> True];
		Print[name <> " installed to " <> $UserBasePacletsDirectory];
	),
	$ScriptCommandLine[[2]] == "uninstall", (
		PacletUninstall[name];
		Print[name <> " has been uninstalled"];
	),
    True, Print["Unrecognized argument"]
];
