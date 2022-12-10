#!/usr/bin/env wolframscript
(* ::Package:: *)

help = "Usage:\n" <>
	"Integreat.wls build [--skip-docs]\n" <>
	"Integreat.wls install [--skip-docs]\n" <>
	"Integreat.wls docs\n" <>
	"Integreat.wls uninstall\n" <>
	"Integreat.wls clean";

If[Length[$ScriptCommandLine] < 2, (
	Print[help];
	Exit[];
)];

Needs["PacletTools`"];
integreatDir = DirectoryName[First[$ScriptCommandLine]];
srcDir = FileNameJoin[{integreatDir, "src"}];
buildDir = FileNameJoin[{integreatDir, "build"}];
docsDir = FileNameJoin[{integreatDir, "docs"}];

build[] := If[
	MemberQ[$ScriptCommandLine, "--skip-docs"],
	CreatePacletArchive,
	PacletTools`PacletBuild[##]["PacletArchive"] &
][srcDir, buildDir];

Switch[$ScriptCommandLine[[2]],
	"build", build[],
	"install", PacletInstall[build[], ForceVersionInstall -> True],
	"docs", PacletDocumentationBuild[srcDir, docsDir, "HTML"],
	"uninstall", PacletUninstall["Integreat"],
	"clean", DeleteDirectory[buildDir, DeleteContents -> True],
	_, Print["Unrecognized command\n\n" <> help];
];
