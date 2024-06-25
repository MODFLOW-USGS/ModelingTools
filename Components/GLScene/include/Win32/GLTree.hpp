// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTree.pas' rev: 36.00 (Windows)

#ifndef GltreeHPP
#define GltreeHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLState.hpp>
#include <GLMaterial.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLPersistentClasses.hpp>
#include <XOpenGL.hpp>
#include <GLContext.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTreeLeaves;
class DELPHICLASS TGLTreeBranch;
class DELPHICLASS TGLTreeBranches;
class DELPHICLASS TGLTreeBranchNoise;
class DELPHICLASS TGLTree;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeLeaves : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	int FCount;
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectorlists::TAffineVectorList* FNormals;
	Glvectorlists::TAffineVectorList* FTexCoords;
	
public:
	__fastcall TGLTreeLeaves(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeLeaves();
	void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall AddNew(const Glvectorgeometry::TMatrix &matrix);
	void __fastcall Clear();
	__property TGLTree* Owner = {read=FOwner};
	__property int Count = {read=FCount, nodefault};
	__property Glvectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Glvectorlists::TAffineVectorList* Normals = {read=FNormals};
	__property Glvectorlists::TAffineVectorList* TexCoords = {read=FTexCoords};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranch : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTreeBranches* FOwner;
	TGLTreeBranch* FLeft;
	TGLTreeBranch* FCenter;
	TGLTreeBranch* FRight;
	TGLTreeBranch* FParent;
	int FBranchID;
	int FParentID;
	Glvectorgeometry::TMatrix FMatrix;
	Glvectorlists::TIntegerList* FLower;
	Glvectorlists::TIntegerList* FUpper;
	bool FCentralLeader;
	void __fastcall BuildBranch(TGLTreeBranchNoise* branchNoise, const Glvectorgeometry::TMatrix &matrix, float TexCoordY, float Twist, int Level);
	
public:
	__fastcall TGLTreeBranch(TGLTreeBranches* AOwner, TGLTreeBranch* AParent);
	__fastcall virtual ~TGLTreeBranch();
	__property TGLTreeBranches* Owner = {read=FOwner};
	__property TGLTreeBranch* Left = {read=FLeft};
	__property TGLTreeBranch* Center = {read=FCenter};
	__property TGLTreeBranch* Right = {read=FRight};
	__property TGLTreeBranch* Parent = {read=FParent};
	__property Glvectorgeometry::TMatrix matrix = {read=FMatrix};
	__property Glvectorlists::TIntegerList* Lower = {read=FLower};
	__property Glvectorlists::TIntegerList* Upper = {read=FUpper};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranches : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	Glvectorlists::TSingleList* FSinList;
	Glvectorlists::TSingleList* FCosList;
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectorlists::TAffineVectorList* FNormals;
	Glvectorlists::TAffineVectorList* FTexCoords;
	Glvectorlists::TIntegerList* FIndices;
	TGLTreeBranch* FRoot;
	int FCount;
	System::Classes::TList* FBranchCache;
	Glvectorlists::TIntegerList* FBranchIndices;
	void __fastcall BuildBranches();
	
public:
	__fastcall TGLTreeBranches(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeBranches();
	void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall Clear();
	__property TGLTree* Owner = {read=FOwner};
	__property Glvectorlists::TSingleList* SinList = {read=FSinList};
	__property Glvectorlists::TSingleList* CosList = {read=FCosList};
	__property Glvectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Glvectorlists::TAffineVectorList* Normals = {read=FNormals};
	__property Glvectorlists::TAffineVectorList* TexCoords = {read=FTexCoords};
	__property int Count = {read=FCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranchNoise : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float FBranchNoise;
	TGLTreeBranchNoise* FLeft;
	TGLTreeBranchNoise* FRight;
	TGLTreeBranchNoise* FCenter;
	TGLTreeBranchNoise* __fastcall GetLeft();
	TGLTreeBranchNoise* __fastcall GetCenter();
	TGLTreeBranchNoise* __fastcall GetRight();
	
public:
	__fastcall TGLTreeBranchNoise();
	__fastcall virtual ~TGLTreeBranchNoise();
	__property TGLTreeBranchNoise* Left = {read=GetLeft};
	__property TGLTreeBranchNoise* Center = {read=GetCenter};
	__property TGLTreeBranchNoise* Right = {read=GetRight};
	__property float branchNoise = {read=FBranchNoise};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTree : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	int FDepth;
	int FBranchFacets;
	float FLeafSize;
	float FBranchSize;
	float FBranchNoise;
	float FBranchAngleBias;
	float FBranchAngle;
	float FBranchTwist;
	float FBranchRadius;
	float FLeafThreshold;
	float FCentralLeaderBias;
	bool FCentralLeader;
	int FSeed;
	bool FAutoCenter;
	bool FAutoRebuild;
	float FCenterBranchConstant;
	TGLTreeLeaves* FLeaves;
	TGLTreeBranches* FBranches;
	TGLTreeBranchNoise* FNoise;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Glmaterial::TGLLibMaterialName FLeafMaterialName;
	Glmaterial::TGLLibMaterialName FLeafBackMaterialName;
	Glmaterial::TGLLibMaterialName FBranchMaterialName;
	bool FRebuildTree;
	Glvectorgeometry::TVector FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetDepth(const int Value);
	void __fastcall SetBranchFacets(const int Value);
	void __fastcall SetLeafSize(const float Value);
	void __fastcall SetBranchSize(const float Value);
	void __fastcall SetBranchNoise(const float Value);
	void __fastcall SetBranchAngleBias(const float Value);
	void __fastcall SetBranchAngle(const float Value);
	void __fastcall SetBranchTwist(const float Value);
	void __fastcall SetBranchRadius(const float Value);
	void __fastcall SetLeafThreshold(const float Value);
	void __fastcall SetCentralLeaderBias(const float Value);
	void __fastcall SetCentralLeader(const bool Value);
	void __fastcall SetSeed(const int Value);
	void __fastcall SetAutoCenter(const bool Value);
	void __fastcall SetAutoRebuild(const bool Value);
	void __fastcall SetCenterBranchConstant(const float Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetLeafMaterialName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetLeafBackMaterialName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetBranchMaterialName(const Glmaterial::TGLLibMaterialName Value);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TGLTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTree();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall StructureChanged();
	void __fastcall BuildMesh(Glvectorfileobjects::TGLBaseMesh* GLBaseMesh);
	void __fastcall RebuildTree();
	void __fastcall ForceTotalRebuild();
	void __fastcall Clear();
	void __fastcall GetExtents(Glvectorgeometry::TAffineVector &min, Glvectorgeometry::TAffineVector &max);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromFile(const System::UnicodeString aFileName);
	void __fastcall SaveToFile(const System::UnicodeString aFileName);
	__property TGLTreeLeaves* Leaves = {read=FLeaves};
	__property TGLTreeBranches* Branches = {read=FBranches};
	__property TGLTreeBranchNoise* Noise = {read=FNoise};
	
__published:
	__property int Depth = {read=FDepth, write=SetDepth, nodefault};
	__property int BranchFacets = {read=FBranchFacets, write=SetBranchFacets, nodefault};
	__property float LeafSize = {read=FLeafSize, write=SetLeafSize};
	__property float BranchSize = {read=FBranchSize, write=SetBranchSize};
	__property float branchNoise = {read=FBranchNoise, write=SetBranchNoise};
	__property float BranchAngleBias = {read=FBranchAngleBias, write=SetBranchAngleBias};
	__property float BranchAngle = {read=FBranchAngle, write=SetBranchAngle};
	__property float BranchTwist = {read=FBranchTwist, write=SetBranchTwist};
	__property float BranchRadius = {read=FBranchRadius, write=SetBranchRadius};
	__property float LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold};
	__property float CentralLeaderBias = {read=FCentralLeaderBias, write=SetCentralLeaderBias};
	__property bool CentralLeader = {read=FCentralLeader, write=SetCentralLeader, nodefault};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property bool AutoCenter = {read=FAutoCenter, write=SetAutoCenter, nodefault};
	__property bool AutoRebuild = {read=FAutoRebuild, write=SetAutoRebuild, nodefault};
	__property float CenterBranchConstant = {read=FCenterBranchConstant, write=SetCenterBranchConstant};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property Glmaterial::TGLLibMaterialName LeafMaterialName = {read=FLeafMaterialName, write=SetLeafMaterialName};
	__property Glmaterial::TGLLibMaterialName LeafBackMaterialName = {read=FLeafBackMaterialName, write=SetLeafBackMaterialName};
	__property Glmaterial::TGLLibMaterialName BranchMaterialName = {read=FBranchMaterialName, write=SetBranchMaterialName};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTree(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTREE)
using namespace Gltree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltreeHPP
