// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLOctree.pas' rev: 36.00 (Windows)

#ifndef GloctreeHPP
#define GloctreeHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLGeometryBB.hpp>
#include <GLContext.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gloctree
{
//-- forward type declarations -----------------------------------------------
struct TOctreeTriangleInfo;
struct TOctreeNode;
class DELPHICLASS TGLOctree;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TProcInt)(int I);

typedef void __fastcall (__closure *TProcAffineAffineAffine)(const Glvectorgeometry::TAffineFltVector &V1, const Glvectorgeometry::TAffineFltVector &V2, const Glvectorgeometry::TAffineFltVector &V3);

typedef TOctreeTriangleInfo *POctreeTriangleInfo;

struct DECLSPEC_DRECORD TOctreeTriangleInfo
{
public:
	int Index;
	System::StaticArray<Glvectortypes::TVector3f, 3> Vertex;
};


typedef TOctreeNode *POctreeNode;

struct DECLSPEC_DRECORD TOctreeNode
{
	
private:
	typedef System::DynamicArray<int> _TOctreeNode__1;
	
	
public:
	Glvectorgeometry::TAffineFltVector MinExtent;
	Glvectorgeometry::TAffineFltVector MaxExtent;
	Glcontext::TGLListHandle* ListHandle;
	bool WillDraw;
	_TOctreeNode__1 TriArray;
	System::StaticArray<POctreeNode, 8> ChildArray;
};


class PASCALIMPLEMENTATION TGLOctree : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<POctreeNode> _TGLOctree__1;
	
	
protected:
	float __fastcall GetMidPoint(float Min, float Max);
	bool __fastcall PointInNode(const Glvectorgeometry::TAffineFltVector &Min, const Glvectorgeometry::TAffineFltVector &Max, const Glvectorgeometry::TAffineFltVector &APoint);
	bool __fastcall TriIntersectNode(const Glvectorgeometry::TAffineFltVector &MinExtent, const Glvectorgeometry::TAffineFltVector &MaxExtent, const Glvectorgeometry::TAffineFltVector &V1, const Glvectorgeometry::TAffineFltVector &V2, const Glvectorgeometry::TAffineFltVector &V3);
	bool __fastcall SphereInNode(const Glvectorgeometry::TAffineVector &MinExtent, const Glvectorgeometry::TAffineVector &MaxExtent, const Glvectorgeometry::TVector &C, float Radius);
	void __fastcall WalkTriToLeafx(POctreeNode Onode, const Glvectorgeometry::TAffineFltVector &V1, const Glvectorgeometry::TAffineFltVector &V2, const Glvectorgeometry::TAffineFltVector &V3);
	void __fastcall WalkPointToLeafx(POctreeNode ONode, const Glvectorgeometry::TAffineVector &P);
	void __fastcall WalkSphereToLeafx(POctreeNode Onode, const Glvectorgeometry::TVector &P, float Radius);
	void __fastcall WalkRayToLeafx(POctreeNode Onode, const Glvectorgeometry::TVector &P, const Glvectorgeometry::TVector &V);
	Glvectorgeometry::TAffineFltVector __fastcall GetExtent(const System::Byte *Flags, const System::NativeInt Flags_High, POctreeNode ParentNode);
	void __fastcall Refine(POctreeNode ParentNode, int Level);
	void __fastcall WalkPointToLeaf(POctreeNode ONode, const Glvectorgeometry::TAffineVector &P);
	void __fastcall WalkTriToLeaf(POctreeNode Onode, const Glvectorgeometry::TAffineVector &V1, const Glvectorgeometry::TAffineVector &V2, const Glvectorgeometry::TAffineVector &V3);
	void __fastcall WalkRayToLeaf(POctreeNode Onode, const Glvectorgeometry::TVector &P, const Glvectorgeometry::TVector &V);
	void __fastcall ConvertR4(POctreeNode ONode, const Glvectorgeometry::TAffineFltVector &Scale);
	void __fastcall CreateTree(int Depth);
	void __fastcall CutMesh();
	
public:
	Glvectorgeometry::TAffineFltVector WorldMinExtent;
	Glvectorgeometry::TAffineFltVector WorldMaxExtent;
	POctreeNode RootNode;
	int MaxOlevel;
	int NodeCount;
	int TriCountMesh;
	int TriCountOctree;
	int MeshCount;
	_TGLOctree__1 ResultArray;
	Glvectorlists::TAffineVectorList* TriangleFiler;
	void __fastcall WalkSphereToLeaf(POctreeNode Onode, const Glvectorgeometry::TVector &P, float Radius);
	void __fastcall InitializeTree(const Glvectorgeometry::TAffineVector &AWorldMinExtent, const Glvectorgeometry::TAffineVector &AWorldMaxExtent, Glvectorlists::TAffineVectorList* const ATriangles, const int ATreeDepth);
	void __fastcall DisposeTree();
	__fastcall virtual ~TGLOctree();
	bool __fastcall RayCastIntersect(const Glvectorgeometry::TVector &RayStart, const Glvectorgeometry::TVector &RayVector, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector IntersectNormal = (Glvectorgeometry::PVector)(0x0), POctreeTriangleInfo TriangleInfo = (POctreeTriangleInfo)(0x0));
	bool __fastcall SphereSweepIntersect(const Glvectorgeometry::TVector &RayStart, const Glvectorgeometry::TVector &RayVector, const float Velocity, const float Radius, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector IntersectNormal = (Glvectorgeometry::PVector)(0x0));
	bool __fastcall TriangleIntersect(const Glvectorgeometry::TAffineVector &V1, const Glvectorgeometry::TAffineVector &V2, const Glvectorgeometry::TAffineVector &V3);
	Glvectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingAABB(const Glgeometrybb::TAABB &ObjAABB);
	Glvectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingCube(const Glgeometrybb::TAABB &ObjAABB, const Glvectorgeometry::TMatrix &ObjToSelf, const Glvectorgeometry::TMatrix &SelfToObj);
	bool __fastcall AABBIntersect(const Glgeometrybb::TAABB &AABB, const Glvectorgeometry::TMatrix &M1to2, const Glvectorgeometry::TMatrix &M2to1, Glvectorlists::TAffineVectorList* Triangles = (Glvectorlists::TAffineVectorList*)(0x0));
public:
	/* TObject.Create */ inline __fastcall TGLOctree() : System::TObject() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gloctree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLOCTREE)
using namespace Gloctree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GloctreeHPP
