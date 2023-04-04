(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


(*Inner sub tree function for only formal expansions*)
innerBSubtrees[t_]:=Complement[DeleteDuplicates[subTrees[t]], {t}];
 
 (*Expand out power terms in a list to individual terms*)
listPowerRemove[x_]:=Flatten[Replace[x,{Power[y_,p_] :>ConstantArray[y,p]},{0,1}]];

(*Flatten a Forest Space into a flat List *)
linearCombToList[x_Plus]:=Flatten[Replace[List@@x,{Times[c_Integer,y_] :>ConstantArray[y,c]},{0,1}]];
linearCombToList[Times[y_Integer,x_]]:=ConstantArray[x,y];
linearCombToList[x_]:={x};

(*Generalized Outer Product on two list with a Cofactor Expansion applied where there are no alternating signs*)
outerACE[op_,t_List /;Length[t] ==2,k_List /;Length[k] ==2]:=op[t[[1]],k[[1]]]*op[t[[2]],k[[2]]] + op[t[[1]],k[[2]]]*op[t[[2]],k[[1]]];
outerACE[op_,t_List,k_List] := Sum[op[First[t],k[[i]]]*outerACE[op,Drop[t,1],Delete[k,i]],{i,1,Length[k]}];

(*Given a list of trees with powers in entries get the factorial of each power and product*)
factorialProd[t_]:=Times@@(#!&/@Replace[Cases[t,_Power,{0,1}],{Power[y_,p_] :>p},{0,1}]);

(*Given a Polynomial it will map a function to each varaible*)
PolynomialMap[f_,poly_]:=FromCoefficientRules[CoefficientRules[poly],f/@Variables[poly]];


getRoot[t_Symbol]:=t;
getRoot[t_Subscript]:=t;
getRoot[t_Power]:=0;
getRoot[t_Times]:=0;
getRoot[t_Plus]:=0;
getRoot[t_]:=Head[t];


getChildren[_Symbol | _Subscript | _Power | _Times | _Plus]:=0;
getChildren[t_[h_]]:=h;


subTrees[k_]:={k};
subTrees[k_[t_]]:=Append[Map[k,subTrees[t]],k];
subTrees[t_Times]:=Times@@@Most[Tuples[Map[Append[subTrees[#],1]&,List@@t]]];
subTrees[t_^p_]:=Times@@@Most[Tuples[Append[subTrees[t],1],p]];


prune[t_,t_]:=1;
prune[t_,k_]:=0;
prune[t_,1]:=t;
prune[t_[u_],t_]:=u;
prune[t_[u_],t_[g_]]:=prune[u,g];
prune[t_Times, k_]:=Sum[prune[t[[i]],k]*(Length[t] - 1)!*Delete[t,i],{i,1,Length[t]}];
prune[t_,k_Times]:=0;
prune[t_Times, k_Times]:=With[{tpad = listPowerRemove[List@@t], kpad =listPowerRemove[List@@k] },If[Length[tpad]>=Length[kpad],(1/factorialProd[List@@k])*outerACE[prune,tpad,PadRight[kpad,Length[tpad],1]],0]];
prune[t_^p_,k_Times]:=With[{kpad = listPowerRemove[List@@k]},If[p>=Length[kpad],(p! /factorialProd[List@@k] )*Product[prune[t,kpad[[i]]],{i,1,Length[kpad]}]*prune[t,1]^(p-Length[kpad]),0]];
prune[t_Times,k_^q_]:=With[{tpad=listPowerRemove[List@@t]},If[Length[tpad]>=q,(1/q!)*outerACE[prune,tpad,PadRight[ConstantArray[k,q],Length[tpad],1]],0]];
prune[t_^p_,k_]:=p*prune[t,k]*prune[t,1]^(p-1);
prune[t_^p_,k_^q_]:=If[p>=q,Binomial[p,q]*prune[t,k]^(q)*prune[t,1]^(p-q),0];


contract[t_,k_]:=If[getRoot[t]===getRoot[k],t,0];
contract[t_,k_[u_]]:=If[getRoot[t]===k,Total[#*innerTreeSub[ Flatten[linearCombFlattenToList[Expand[prune[t,#]]]],u]&/@innerBSubtrees[t],2],0];
innerContract[tl_,u_]:=contract[#,u]&/@tl;
contract[t_Times,k_]:=0;
contract[t_,k_Times]:=0;
contract[t_Times,k_[u_]]:=0;
contract[t_[w_],k_Times]:=0;
contract[t_^p_,k_]:=0;
contract[t_,k_^q_]:=0;
contract[t_^p_,k_[u_]]:=0;
contract[t_[w_],k_^q_]:=0;
contract[t_Times,k_Times]:=With[{tpad = listPowerRemove[List@@t],kpad=listPowerRemove[List@@k]},If[Length[tpad]==Length[kpad],(1/factorialProd[List@@k])*outerACE[contract,tpad,kpad],0]];
contract[t_Times,k_^q_]:=With[{tpad=listPowerRemove[List@@t]},If[Length[tpad]==q,Product[contract[tpad[[i]],k],{i,1,Length[tpad]}],0]];
contract[t_^p_,k_Times]:=With[{kpad = listPowerRemove[List @@k]},If[p==Length[kpad],(p!/factorialProd[List@@k])*Product[contract[t,kpad[[i]]],{i,1,Length[kpad]}],0]];
contract[t_^p_,k_^q_]:=If[p==q,contract[t,k]^p,0];


(* ::Section:: *)
(*Package Definitions*)


BTreePrune[h_[t_,p___],h_[t_,p___]]:=1;
BTreePrune[h_[t_,p___],h_[\[FormalY],p___]]:=h[t,p];
BTreePrune[h_[t_,p___],h_[k_,p___]]:=With[{poly = Expand[prune[t,k]]},If[NumericQ[poly],poly,PolynomialMap[h[#,p]&,poly]]];


BTreeContract[h_[\[FormalY],p___],h_[\[FormalY],p___]]:=h[\[FormalY],p];
BTreeContract[h_[t_,p___],h_[k_,p___]]:=With[{poly = Expand[contract[t,k]]},If[NumericQ[poly],poly,PolynomialMap[h[#,p]&,poly]]];


BTreeSubTrees[h_[t_,p___]]:=Map[h[#,p]&,Complement[DeleteDuplicates[subTrees[t]], {t}]];


BTreeRoot[h_[t_,p___]]:=h[getRoot[t],p];
SetAttributes[BTreeRoot, Listable];


BTreeChildren[h_[_Symbol | _Subscript,p___]]:=h[\[FormalY],p];
BTreeChildren[h_[t_[k_],p___]]:=PolynomialMap[h[#,p]&,Expand[getChildren[t]]];
SetAttributes[BTreeChildren, Listable];
