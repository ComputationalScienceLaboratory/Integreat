#!/usr/bin/env wolframscript
(* ::Package:: *)

Which[
	Length[$ScriptCommandLine] == 1, (
		srcDir = FileNameJoin[{DirectoryName[ExpandFileName[First[$ScriptCommandLine]]], "Integreat"}];
		PacletInstall[PackPaclet[srcDir], IgnoreVersion -> True];
	),
	$ScriptCommandLine[[2]] == "uninstall", PacletUninstall["Integreat"],
    True, Print["Unrecognized argument"]
];
