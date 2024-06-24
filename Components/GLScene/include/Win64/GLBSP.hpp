// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glbsp.pas' rev: 36.00 (Windows)

#ifndef GlbspHPP
#define GlbspHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.Math.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glmaterial.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glcolor.hpp>
#include <Glrendercontextinfo.hpp>
#include <Glvectortypes.hpp>
#include <Glpersistentclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbsp
{
//-- forward type declarations -----------------------------------------------
struct TBSPCullingSphere;
struct TBSPRenderContextInfo;
class DELPHICLASS TBSPClusterVisibility;
class DELPHICLASS TBSPMeshObject;
class DELPHICLASS TFGBSPNode;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TBSPCullingSphere
{
public:
	Glvectorgeometry::TVector position;
	float radius;
};


struct DECLSPEC_DRECORD TBSPRenderContextInfo
{
	
private:
	typedef System::DynamicArray<TBSPCullingSphere> _TBSPRenderContextInfo__1;
	
	
public:
	Glvectorgeometry::TVector cameraLocal;
	Glrendercontextinfo::PRenderContextInfo rci;
	System::Classes::TList* faceGroups;
	_TBSPRenderContextInfo__1 cullingSpheres;
};


enum DECLSPEC_DENUM TBSPRenderSort : unsigned char { rsNone, rsBackToFront, rsFrontToBack };

class PASCALIMPLEMENTATION TBSPClusterVisibility : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glvectorgeometry::PByteArray FData;
	int FSize;
	int FBytesPerCluster;
	int FCount;
	
protected:
	void __fastcall SetCount(int NumClusters);
	bool __fastcall GetVisibility(int Source, int Destination);
	void __fastcall SetVisibility(int Source, int Destination, const bool Value);
	
public:
	__fastcall TBSPClusterVisibility();
	__fastcall virtual ~TBSPClusterVisibility();
	void __fastcall SetData(System::PByte Source, int NumClusters);
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property bool Visibility[int src][int dst] = {read=GetVisibility, write=SetVisibility};
};


class PASCALIMPLEMENTATION TBSPMeshObject : public Glvectorfileobjects::TMeshObject
{
	typedef Glvectorfileobjects::TMeshObject inherited;
	
private:
	TBSPRenderSort FRenderSort;
	TBSPClusterVisibility* FClusterVisibility;
	bool FUseClusterVisibility;
	
public:
	__fastcall TBSPMeshObject(Glvectorfileobjects::TGLMeshObjectList* AOwner);
	__fastcall virtual ~TBSPMeshObject();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
	void __fastcall CleanupUnusedNodes();
	float __fastcall AverageDepth();
	TFGBSPNode* __fastcall FindNodeByPoint(const Glvectorgeometry::TVector &aPoint);
	__property TBSPRenderSort RenderSort = {read=FRenderSort, write=FRenderSort, nodefault};
	__property TBSPClusterVisibility* ClusterVisibility = {read=FClusterVisibility};
	__property bool UseClusterVisibility = {read=FUseClusterVisibility, write=FUseClusterVisibility, nodefault};
public:
	/* TMeshObject.Create */ inline __fastcall virtual TBSPMeshObject() : Glvectorfileobjects::TMeshObject() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBSPMeshObject(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMeshObject(reader) { }
	
};


class PASCALIMPLEMENTATION TFGBSPNode : public Glvectorfileobjects::TFGVertexIndexList
{
	typedef Glvectorfileobjects::TFGVertexIndexList inherited;
	
private:
	Glvectorgeometry::THmgPlane FSplitPlane;
	int FPositiveSubNodeIndex;
	int FNegativeSubNodeIndex;
	int FCluster;
	
protected:
	int __fastcall AddLerp(int iA, int iB, float fB, float fA);
	int __fastcall AddLerpIfDistinct(int iA, int iB, int iMid);
	
public:
	__fastcall virtual TFGBSPNode(Glvectorfileobjects::TGLFaceGroups* AOwner);
	__fastcall virtual ~TFGBSPNode();
	void __fastcall IsCulled(const TBSPRenderContextInfo &bsprci, bool &positive, bool &negative);
	void __fastcall CollectNoSort(TBSPRenderContextInfo &bsprci);
	void __fastcall CollectFrontToBack(TBSPRenderContextInfo &bsprci);
	void __fastcall CollectBackToFront(TBSPRenderContextInfo &bsprci);
	Glvectorgeometry::THmgPlane __fastcall FindSplitPlane(float triangleSplitCost = 1.000000E+00f, float triangleImbalanceCost = 5.000000E-01f);
	void __fastcall EvaluateSplitPlane(const Glvectorgeometry::THmgPlane &splitPlane, int &nbTriangleSplit, int &nbPositiveTriangles, int &nbNegativeTriangles);
	void __fastcall PerformSplit(const Glvectorgeometry::THmgPlane &splitPlane, const int maxTrianglesPerLeaf = 0x7fffffff);
	void __fastcall FixTJunctions(Glvectorlists::TIntegerList* const tJunctionsCandidates);
	__property Glvectorgeometry::THmgPlane splitPlane = {read=FSplitPlane, write=FSplitPlane};
	__property int PositiveSubNodeIndex = {read=FPositiveSubNodeIndex, write=FPositiveSubNodeIndex, nodefault};
	__property int NegativeSubNodeIndex = {read=FNegativeSubNodeIndex, write=FNegativeSubNodeIndex, nodefault};
	__property int Cluster = {read=FCluster, write=FCluster, nodefault};
public:
	/* TFGVertexIndexList.Create */ inline __fastcall virtual TFGBSPNode() : Glvectorfileobjects::TFGVertexIndexList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGBSPNode(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TFGVertexIndexList(reader) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbsp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBSP)
using namespace Glbsp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbspHPP
