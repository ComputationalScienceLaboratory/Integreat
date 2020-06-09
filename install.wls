#!/usr/bin/env wolframscript
(* ::Package:: *)

Which[
	Length[$ScriptCommandLine] == 1, (
		srcDir = FileNameJoin[{DirectoryName[ExpandFileName[First[$ScriptCommandLine]]], "Integreat"}];
		PacletInstall[CreatePacletArchive[srcDir], ForceVersionInstall -> True];
	),
	$ScriptCommandLine[[2]] == "uninstall", PacletUninstall["Integreat"],
    True, Print["Unrecognized argument"]
];
