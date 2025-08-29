(* ::Package:: *)

BeginPackage["ModularPackage`"];


(* ::Chapter:: *)
(*Declarations*)


(* ::Section::Closed:: *)
(*Misc*)


ZeroMatrix::usage = "ZeroMatrix[n] gives the n\[Cross]n zero matrix.";


MatrixFormList::usage = "MatrixFormList[{mat1,...}] prints the list of mat1,... in MatrixForm.";


InvTrans::usage = "InvTrans[mat] gives the inverse transpose of the matrix mat.";


Dia::usage = "Dia[mat] gives the diagonal matrix consisting of only the diagonal of mat.";


(* ::Section::Closed:: *)
(*Riemann Matrices*)


RandomSO::usage = "RandomSO[n] gives a \"random\" special orthogonal matrix.";


RandomRiemannMatrix::usage = "RandomRiemannMatatrix[n] gives a \"random\" n\[Cross]n Riemann matrix.";


RiemannMatrixQ::usage = "RiemannMatrixQ[mat] gives True if mat is a Riemann matrix (symmetric and positive-definite imaginary part) and False otherwise.";


(* ::Section::Closed:: *)
(*Symplectic matrices General*)


SymplecticQ::usage = "CheckSymplectic[mat] gives True if mat is symplectic (i.e. mat^T J mat = J), False otherwise.";


Gamma12Q::usage = "Gamma12Q[mat] gives True if mat={{A,B},{C,D}} is in \[CapitalGamma]_{1,2} (i.e symplectic and all diagonal elements of A^T C and B^T D are even) False otherwise.";


(* ::Section::Closed:: *)
(*Generators of SP(2g,\[DoubleStruckCapitalZ]) and \[CapitalGamma]_{1,2}*)


SymplecticJ::usage = "SymplecticJ[g] gives the 2g\[Cross]2g symplectic matrix Subscript[J, 2g] = {{0,I},{-I,0}}.";


RandomDA::usage = "RandomDA[g] the 2g\[Cross]2g symplectic matrix D_A = {{A, 0},{0, A^-\[CapitalTau]}} with a \"random\" A\[Element]GL(g,\[DoubleStruckCapitalZ]). Options->Default: MaxEntry->Automatic";


RandomTB::usage = "RandomTB[g] gives the 2g\[Cross]2g symplectic matrix T_B ={{I,B},{0,I}} with a \"random\" symmetric B\[Element]\[DoubleStruckCapitalZ]^(g\[Cross]g). Options->Default: MaxEntry->10";


RandomTBeven::usage = "RandomTBEven[g] gives the 2g\[Cross]2g symplectic matrix T_B ={{I,B},{0,I}} with a \"random\" symmetric B\[Element]\[DoubleStruckCapitalZ]^(g\[Cross]g) with all diagonal elements even. Options->Default: MaxEntry->10";


RandomSC::usage = "RandomSC[g] gives the 2g\[Cross]2g symplectic matrix S_C ={{I,0},{C,I}} with a \"random\" symmetric C\[Element]\[DoubleStruckCapitalZ]^(g\[Cross]g). Options->Default: MaxEntry->10";


RandomSCeven::usage = "RandomSCEven[g] gives the 2g\[Cross]2g symplectic matrix S_C ={{I,0},{C,I}} with a \"random\" symmetric C\[Element]\[DoubleStruckCapitalZ]^(g\[Cross]g) with all diagonal elements even. Options->Default: MaxEntry->10";


(* ::Section::Closed:: *)
(*Elements of SP(2g,\[DoubleStruckCapitalZ]) and \[CapitalGamma]_{1,2}*)


RandomSymplectic::usage = "RandomSymplectic[g, len] gives a random symplectic 2g\[Cross]2g matrix as a product of len random generators";


RandomSymplecticGamma::usage = "RandomSymplecticGamma[g, len] gives a random 2g\[Cross]2g matrix in \[CapitalGamma]_{1,2} as a product of len random generators";


(* ::Section::Closed:: *)
(*Group action on \[ScriptCapitalH]_g and \[DoubleStruckCapitalC]^g\[Cross]\[ScriptCapitalH]_g, transformation of characters*)


GetSubMatrices::usage = "For a 2g\[Cross]2g matrix {{A,B},{C,D}} gives the list of g\[Cross]g matrices {A,B,C,D}";


ActionHg::usage = "ActionHg[sympl, riemat] applies the action of the symplectic 2g\[Cross]2g matrix sympl on the g\[Cross]g Riemann matrix riemat.";


ActionCgHg::usage = "ActionCgHg[sympl, tup] applies the action of the symplectic 2g\[Cross]2g matrix sympl on the tuple (zVec, riemat) containing zVec\[Element]\[DoubleStruckCapitalC]^g and a g\[Cross]g Riemann matrix riemat."; 


CharTransf::usage = "CharTransf[sympl, {char1,char2}] transforms the g-vectors char1,2 in the appropriate way as given in the general modular transformation formula.}"


(* ::Section::Closed:: *)
(*The index set Z_k*)


MISetConcatenate::usage = "MISetConcatenate[MISet, i] gives, for a multi-index set MISet and an integer i, the multi-index set MISet(i), consisting of all multi-indices in MISet concatenated with (i).";


MISetZList::usage = "MISetZList[n] gives the list {Z_1,...,Z_n} of all multi-index sets Z_k up to k=n, for n>=0.";


MISetZ::usage = "MISetZ[k] gives the multi-index set Z_k for k>= -1.";


(* ::Section::Closed:: *)
(*Lattice vector \[Lambda]*)


ConstructFk::usage = "ConstructFk[BList, k] gives the integer matrix F_k from prep doc 6 Eq. (13)";


ConstructGk::usage = "ConstructGk[BList, k] gives the integer matrix G_k from prep doc 6 Eq. (14)";


ConstructFGLists::usage = "ConstructFGLists[BList] gives the set {{F_1,...F_n},{G_1,...G_n}} constructed from the set set of symmetric integer martices BList={B_1,...B_n}.";


LatticeShiftVec::usage = "LatticeShiftVec[BList,j1,\[Tau]] gives the lattice vector \[Lambda]_M from Proposition 2.11 of prep doc 6.";


LatticeShiftVecTauPart::usage = "LatticeShiftVecTauPart[BList,j1] if the lattice shift vector \[Lambda] is written as \[Lambda]_I + \[Tau]\[Lambda]_\[Tau], gives the integer vector \[Lambda]_\[Tau].";


(* ::Chapter:: *)
(*Definitions*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Misc*)


ZeroMatrix[n_]:=ConstantArray[0,{n,n}]


MatrixFormList[matList_]:=Table[mat//MatrixForm,{mat,matList}]


InvTrans[mat_]:=Inverse[Transpose[mat]]


Dia[mat_]:=DiagonalMatrix[Diagonal[mat]]


(* ::Section::Closed:: *)
(*Riemann Matrices*)


RandomSO[n_]:=Module[
{
	A=RandomReal[{-1,1},{n,n}]
},
	MatrixExp[A-Transpose[A]]
]


RandomRiemannMatrix[n_] := Module[
{
	A=RandomReal[{-2,2},{n,n}],
	Diag=DiagonalMatrix[RandomReal[{0.0001,1},n]],
	Q=RandomSO[n]
},
	Module[
	{
		rePart=(A+Transpose[A])/2,
		imPart=Transpose[Q] . Diag . Q
	},
		(rePart+I imPart + Transpose[rePart+I imPart])/2 (* explicitly symmetrized to avoid SiegelTheta throwing wrong errors *)
	]
]


RiemannMatrixQ[mat_]:=SymmetricMatrixQ[mat]&&PositiveDefiniteMatrixQ[Im[mat]]


(* ::Section::Closed:: *)
(*Symplectic matrices General*)


SymplecticQ[mat_]:=Module[
{
	g=Length[mat]/2
},
	Transpose[mat] . SymplecticJ[g] . mat===SymplecticJ[g]
]


Gamma12Q[mat_]:=Module[
{
	abcd=GetSubMatrices[mat]
},
	AllTrue[Diagonal[Transpose[abcd[[1]]] . abcd[[3]]],EvenQ]&&AllTrue[Diagonal[Transpose[abcd[[2]]] . abcd[[4]]],EvenQ]
]


(* ::Section::Closed:: *)
(*Generators of SP(2g,\[DoubleStruckCapitalZ]) and \[CapitalGamma]_{1,2}*)


SymplecticJ[g_]:=Module[
{
	Ig=IdentityMatrix[g],
	Zerog=ConstantArray[0,{g,g}]
},
	ArrayFlatten[{{Zerog,Ig},{-Ig,Zerog}}]
]


RandomDA[g_, OptionsPattern[MaxEntry->Automatic]]:=Module[
{
	A=ResourceFunction["RandomUnimodularMatrix"][g,MaxEntry->OptionValue["MaxEntry"]]
},
	BlockDiagonalMatrix[{A,InvTrans[A]}]
]


RandomTB[g_, OptionsPattern[MaxEntry->10]]:=Module[
{
	mat=RandomInteger[{-OptionValue["MaxEntry"],OptionValue["MaxEntry"]},{g,g}],
	Ig=IdentityMatrix[g],
	Zerog=ConstantArray[0,{g,g}]
},
	ArrayFlatten[{{Ig,LowerTriangularize[mat] + Transpose[LowerTriangularize[mat,-1]]},{Zerog,Ig}}]
]


RandomTBeven[g_, OptionsPattern[MaxEntry->10]]:=Module[
{
	slt=LowerTriangularize[RandomInteger[{-OptionValue["MaxEntry"],OptionValue["MaxEntry"]},{g,g}],-1],
	Diag=DiagonalMatrix[2*RandomInteger[{-Floor[OptionValue["MaxEntry"]/2],Floor[OptionValue["MaxEntry"]/2]},g]],
	Ig=IdentityMatrix[g],
	Zerog=ConstantArray[0,{g,g}]
},
	ArrayFlatten[{{Ig,slt+Diag+Transpose[slt]},{Zerog,Ig}}]
]


RandomSC[g_, OptionsPattern[MaxEntry->10]]:=Transpose[RandomTB[g,MaxEntry->OptionValue["MaxEntry"]]]


RandomSCeven[g_, OptionsPattern[MaxEntry->10]]:=Transpose[RandomTBeven[g,MaxEntry->OptionValue["MaxEntry"]]]


(* ::Section::Closed:: *)
(*Elements of SP(2g,\[DoubleStruckCapitalZ]) and \[CapitalGamma]_{1,2}*)


RandomSymplectic[g_,len_]:=Module[
{
	generatorFamilies={SymplecticJ,RandomDA,RandomTB},
	word=RandomChoice[{1,2,3},len]
},
	Dot@@Table[generatorFamilies[[i]][g],{i,word}]
]


RandomSymplecticGamma[g_,len_]:=Module[
{
	generatorFamilies={SymplecticJ,RandomDA,RandomTBeven},
	word=RandomChoice[{1,2,3},len]
},
	Dot@@Table[generatorFamilies[[i]][g],{i,word}]
]


(* ::Section::Closed:: *)
(*Group  action on \[ScriptCapitalH]_g and \[DoubleStruckCapitalC]^g\[Cross]\[ScriptCapitalH]_g, transformation of characters*)


GetSubMatrices[mat_]:=Module[
{
	g=Length[mat]/2
},
	Module[
	{
		a=mat[[1;;g,1;;g]],
		b=mat[[1;;g,g+1;;2g]],
		c=mat[[g+1;;2g,1;;g]],
		d=mat[[g+1;;2g,g+1;;2g]]
	},
		{a,b,c,d}
	]
]


ActionHg[sympl_, riemat_]:=Module[
{
	abcd=GetSubMatrices[sympl],
	res=Null
},
	res=(abcd[[1]] . riemat+abcd[[2]]) . Inverse[abcd[[3]] . riemat+abcd[[4]]]; (* returning explicitly symmetrised result to avoid SiegelTheta throwing wrong errors *)
	(res+Transpose[res])/2
]


ActionCgHg[sympl_,tup_]:=Module[
{
	abcd=GetSubMatrices[sympl],
	zVec=tup[[1]],
	riemat=tup[[2]]
},
	{
	InvTrans[abcd[[3]] . riemat+abcd[[4]]] . zVec,
	ActionHg[sympl,riemat]
	}
]


CharTransf[sympl_, {char1_,char2_}]:=Module[
{
	abcd=GetSubMatrices[sympl],
	a,b,c,d
},
	{a,b,c,d}=abcd;
	{d . char1-c . char2,-b . char1+a . char2}+{Diagonal[c . Transpose[d]],Diagonal[a . Transpose[b]]}/2
]


(* ::Section::Closed:: *)
(*The index set Z_k*)


(* ::Text:: *)
(*From  prep  doc  6*)


MISetConcatenate[MISet_,i_]:=Table[Join[MI,{i}],{MI,MISet}]


MISetZList[n_]:=Module[
{
	lst={{{1}}} (* ={Subscript[Z, 1]} *)
},
	If[n<1 || Not[IntegerQ[n]],
		{
			Print[StringForm["Invalid argument n=`` in GetZList",n]];
			lst=Null;
		}
	];
	If[Not[lst===Null] && n>1,
		{
			lst=Join[lst,{{{},{1,2}}}]; (* now lst={Subscript[Z, 1],Subscript[Z, 2]} *)
			For[ii=3,ii<=n,ii++,
				lst=Join[lst,{Union[lst[[ii-2]],MISetConcatenate[lst[[ii-1]],ii]]}];
			]
		}
	];
	lst
]


MISetZ[k_]:=If[k==-1,{},If[k==0,{{}},Last[MISetZList[k]]]]


(* ::Section::Closed:: *)
(*Lattice vector \[Lambda]*)


(* ::Text:: *)
(*From  prep  doc  6 Eqs. (13, 14)*)


ConstructFk[BList_,k_]:=Module[
{
	g=Last[Dimensions[BList]],
	Z=MISetZ[k-1],
	Fk=Null
},
	If[k==0,
		Fk=ZeroMatrix[g]
	];
	If[k==1,
		Fk=IdentityMatrix[g]
	];
	If[k>1,
		Fk=(-1)^Floor[k/2]Plus@@Table[(-1)^Floor[Length[\[Alpha]]/2]Dot@@Replace[Table[BList[[i]],{i,\[Alpha]}],{}->{IdentityMatrix[g]}],{\[Alpha],Z}]
	];
	Fk
]


ConstructGk[BList_,k_]:=Module[
{
	g=Last[Dimensions[BList]],
	Z=MISetZ[k-2],
	Gk=Null
},
	If[k==IdentityMatrix[g],
		Gk=ZeroMatrix[g]
	];
	If[k==1,
		Gk=ZeroMatrix[g]
	];
	If[k==2,
		Gk=-IdentityMatrix[g]
	];
	If[k>2,
		Gk=(-1)^Floor[(k+1)/2]Plus@@Table[(-1)^Floor[Length[\[Alpha]]/2]Dot@@Replace[Table[BList[[i+1]],{i,\[Alpha]}],{}->{IdentityMatrix[g]}],{\[Alpha],Z}]
	];
	Gk
]


ConstructFGLists[BList_]:=Module[
{
	n=Length[BList],
	FList=Null,
	GList=Null
},
	For[k=1,k<=n,k++,
		{
			FList=(ConstructFk[BList,#]&)/@Range[1,n];
			GList=(ConstructGk[BList,#]&)/@Range[1,n];
		}
	];
	{FList, GList}
]


LatticeShiftVec[BList_,j1_,\[Tau]_]:=Module[
{
	n=Length[BList],
	diagList=Diagonal/@BList,
	FGList=ConstructFGLists[BList],
	\[Lambda]=Null
},
	If[j1==0,
		\[Lambda]=Plus@@Table[(FGList[[1,i]]+\[Tau] . FGList[[2,i]]) . diagList[[i]],{i,Range[n]}]
	];
	If[j1==1,
		\[Lambda]=Plus@@Table[(FGList[[2,i]]-\[Tau] . FGList[[1,i]]) . diagList[[i]],{i,Range[n]}]
	];
	\[Lambda]
]


LatticeShiftVecTauPart[BList_,j1_]:=Module[
{
	n=Length[BList],
	diagList=Diagonal/@BList,
	FGList=ConstructFGLists[BList],
	res=Null
},
	If[j1==0,
		res=Plus@@Table[FGList[[2,i]] . diagList[[i]],{i,Range[n]}]
	];
	If[j1==1,
		res=Plus@@Table[-FGList[[1,i]] . diagList[[i]],{i,Range[n]}]
	];
	res
]


(* ::Chapter::Closed:: *)
(*End*)


End[];


EndPackage[];
