(* ::Package:: *)

<<Integreat`;

rk = Rk[{{"", "", ""}, {"t", "", ""}, {"g", "r", ""}}, {"e", "a", "t"}, {"I", "n", "e"}];
g = Graphics[Style[Text[rk], FontSize -> 48, FontColor -> RGBColor["#1E79C3"]], ImageSize -> 256]
Export[NotebookDirectory[] <> "logo.svg", g];
