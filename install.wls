#!/usr/bin/env wolframscript
(* ::Package:: *)

Which[
	Length[$ScriptCommandLine] == 1, (
		paclet = PacletInstall[
			CreatePacletArchive[FileNameJoin[{Directory[], "Src"}]],
			ForceVersionInstall -> True
		];
		Print["Installed to " <> paclet["Location"]];
	),
	$ScriptCommandLine[[2]] == "uninstall", (
		PacletUninstall["Integreat"];
	),
    True, Print["Unrecognized argument"]
];
