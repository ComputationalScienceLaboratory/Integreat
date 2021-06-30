(* ::Package:: *)

<<Integreat`;
SetDirectory[NotebookDirectory[]];

rk = Rk[{{"", "", ""}, {"t", "", ""}, {"g", "r", ""}}, {"e", "a", "t"}, {"I", "n", "e"}];
g = Graphics[Style[Text[rk], FontSize -> Scaled[0.19]], ImageSize -> Small]
Export["logo.svg", g];
