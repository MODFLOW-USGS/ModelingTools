// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLOctree.pas' rev: 35.00 (Windows)

#ifndef GloctreeHPP
#define GloctreeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
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

typedef void __fastcall (__closure *TProcAffineAffineAffine)(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);

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
	Glvectortypes::TVector3f MinExtent;
	Glvectortypes::TVector3f MaxExtent;
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
	bool __fastcall PointInNode(const Glvectortypes::TVector3f &Min, const Glvectortypes::TVector3f &Max, const Glvectortypes::TVector3f &APoint);
	bool __fastcall TriIntersectNode(const Glvectortypes::TVector3f &MinExtent, const Glvectortypes::TVector3f &MaxExtent, const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);
	bool __fastcall SphereInNode(const Glvectortypes::TVector3f &MinExtent, const Glvectortypes::TVector3f &MaxExtent, const Glvectortypes::TVector4f &C, float Radius);
	void __fastcall WalkTriToLeafx(POctreeNode Onode, const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);
	void __fastcall WalkPointToLeafx(POctreeNode ONode, const Glvectortypes::TVector3f &P);
	void __fastcall WalkSphereToLeafx(POctreeNode Onode, const Glvectortypes::TVector4f &P, float Radius);
	void __fastcall WalkRayToLeafx(POctreeNode Onode, const Glvectortypes::TVector4f &P, const Glvectortypes::TVector4f &V);
	Glvectortypes::TVector3f __fastcall GetExtent(const System::Byte *Flags, const int Flags_High, POctreeNode ParentNode);
	void __fastcall Refine(POctreeNode ParentNode, int Level);
	void __fastcall WalkPointToLeaf(POctreeNode ONode, const Glvectortypes::TVector3f &P);
	void __fastcall WalkTriToLeaf(POctreeNode Onode, const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);
	void __fastcall WalkRayToLeaf(POctreeNode Onode, const Glvectortypes::TVector4f &P, const Glvectortypes::TVector4f &V);
	void __fastcall ConvertR4(POctreeNode ONode, const Glvectortypes::TVector3f &Scale);
	void __fastcall CreateTree(int Depth);
	void __fastcall CutMesh();
	
public:
	Glvectortypes::TVector3f WorldMinExtent;
	Glvectortypes::TVector3f WorldMaxExtent;
	TOctreeNode *RootNode;
	int MaxOlevel;
	int NodeCount;
	int TriCountMesh;
	int TriCountOctree;
	int MeshCount;
	_TGLOctree__1 ResultArray;
	Glvectorlists::TAffineVectorList* TriangleFiler;
	void __fastcall WalkSphereToLeaf(POctreeNode Onode, const Glvectortypes::TVector4f &P, float Radius);
	void __fastcall InitializeTree(const Glvectortypes::TVector3f &AWorldMinExtent, const Glvectortypes::TVector3f &AWorldMaxExtent, Glvectorlists::TAffineVectorList* const ATriangles, const int ATreeDepth);
	void __fastcall DisposeTree();
	__fastcall virtual ~TGLOctree();
	bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &RayStart, const Glvectortypes::TVector4f &RayVector, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector IntersectNormal = (Glvectorgeometry::PVector)(0x0), POctreeTriangleInfo TriangleInfo = (POctreeTriangleInfo)(0x0));
	bool __fastcall SphereSweepIntersect(const Glvectortypes::TVector4f &RayStart, const Glvectortypes::TVector4f &RayVector, const float Velocity, const float Radius, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector IntersectNormal = (Glvectorgeometry::PVector)(0x0));
	bool __fastcall TriangleIntersect(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);
	Glvectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingAABB(const Glgeometrybb::TAABB &ObjAABB);
	Glvectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingCube(const Glgeometrybb::TAABB &ObjAABB, const Glvectortypes::TMatrix4f &ObjToSelf, const Glvectortypes::TMatrix4f &SelfToObj);
	bool __fastcall AABBIntersect(const Glgeometrybb::TAABB &AABB, const Glvectortypes::TMatrix4f &M1to2, const Glvectortypes::TMatrix4f &M2to1, Glvectorlists::TAffineVectorList* Triangles = (Glvectorlists::TAffineVectorList*)(0x0));
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
