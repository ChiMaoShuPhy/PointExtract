(* ::Package:: *)

BeginPackage["PointExtract`"]

RgnSlct::usage="RgnSlct[\"img\",\"RgnVrtcs\"] stores the vertices of polygon-like region R in symbol named \"RgnVrtcs\", selected by user (mouse press). The points on the curves ( of image, \"img\" is the name) inside R will be extracted. "
DataExtrct::usage="DataExtrct[\"img\",RgnVrtcs,RsclF,thrshld,lngthcut] extract the pixcels coordinates of the curves within the region specified by vertices RgnVrtcs. RsclF rescales the coordinates to data set."

Begin["Private`"]

RgnSlct[imgname_String,RgnVrtcs_String]:=ToExpression[RgnVrtcs<>"={};DynamicModule[{p={}},EventHandler[Dynamic@Show["<>imgname<>",Graphics[{Blue,Line["<>RgnVrtcs<>"]}],Graphics[{Blue,Dashed,If[Length[p]>0,Line[{Last[p],Dynamic[MousePosition[\"Graphics\"]]}]]}],ImageSize\[Rule]600],{If[CurrentValue[\"ShiftKey\"],\"MouseDown\"\[RuleDelayed](AppendTo[p,MousePosition[\"Graphics\"]];"<>RgnVrtcs<>"=p)]}]]//Dynamic"]
DataExtrct[imgname_String,bndry_,RsclF_,thrshld_:0.5,lngthcut_:0]:=Module[{Rgn,PntsPstn,pxcl},Rgn=BoundaryDiscretizeRegion[Polygon[bndry]];PntsPstn=Position[Transpose[ImageData[Binarize[ToExpression[imgname],thrshld],DataReversed->True]],0];pxcl=Mean/@DeleteCases[GatherBy[Select[PntsPstn,#\[Element]Rgn&],#[[1]]&],_?(Length[#]<lngthcut&)];RsclF&/@pxcl]
End[];

EndPackage[];
