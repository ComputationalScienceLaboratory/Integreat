(* ::Package:: *)

(* ::Text:: *)
(*This script generates the logo for Integreat.  The logo.svg must be manually edited to change the units from points to pixels, then it can be run through an SVG optimizer.*)


<<Integreat`;

rk = Rk[{{"", "", ""}, {"t", "", ""}, {"g", "r", ""}}, {"e", "a", "t"}, {"I", "n", "e"}];
g = Graphics[Style[Text[rk], FontSize -> 24, FontColor -> RGBColor["#1E79C3"]], ImageSize -> {128, 128}]
Export[NotebookDirectory[] <> "logo.svg", g];
