// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGeometryBB.pas' rev: 34.00 (Windows)

#ifndef GlgeometrybbHPP
#define GlgeometrybbHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgeometrybb
{
//-- forward type declarations -----------------------------------------------
struct THmgBoundingBox;
struct TAABB;
struct TBSphere;
struct TClipRect;
//-- type declarations -------------------------------------------------------
typedef THmgBoundingBox *PHmgBoundingBox;

struct DECLSPEC_DRECORD THmgBoundingBox
{
public:
	System::StaticArray<Glvectortypes::TVector4f, 8> BBox;
};


struct DECLSPEC_DRECORD TAABB
{
public:
	Glvectortypes::TVector3f Min;
	Glvectortypes::TVector3f Max;
};


typedef TAABB *PAABB;

struct DECLSPEC_DRECORD TBSphere
{
public:
	Glvectortypes::TVector3f Center;
	float Radius;
};


struct DECLSPEC_DRECORD TClipRect
{
public:
	float Left;
	float Top;
	float Right;
	float Bottom;
};


enum DECLSPEC_DENUM TSpaceContains : unsigned char { ScNoOverlap, ScContainsFully, ScContainsPartially };

typedef System::StaticArray<Glvectortypes::TVector3f, 8> TAABBCorners;

typedef System::StaticArray<int, 4> TPlanIndices;

typedef System::StaticArray<System::StaticArray<int, 4>, 6> TPlanBB;

typedef System::StaticArray<int, 6> TDirPlan;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE THmgBoundingBox NullBoundingBox;
extern DELPHI_PACKAGE TPlanIndices CBBFront;
extern DELPHI_PACKAGE TPlanIndices CBBBack;
extern DELPHI_PACKAGE TPlanIndices CBBLeft;
extern DELPHI_PACKAGE TPlanIndices CBBRight;
extern DELPHI_PACKAGE TPlanIndices CBBTop;
extern DELPHI_PACKAGE TPlanIndices CBBBottom;
extern DELPHI_PACKAGE TPlanBB CBBPlans;
extern DELPHI_PACKAGE TDirPlan CDirPlan;
extern DELPHI_PACKAGE bool __fastcall BoundingBoxesAreEqual(const THmgBoundingBox &ABoundingBox1, const THmgBoundingBox &ABoundingBox2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall BoundingBoxesAreEqual(const PHmgBoundingBox ABoundingBox1, const PHmgBoundingBox ABoundingBox2)/* overload */;
extern DELPHI_PACKAGE THmgBoundingBox __fastcall AddBB(THmgBoundingBox &C1, const THmgBoundingBox &C2);
extern DELPHI_PACKAGE void __fastcall AddAABB(TAABB &Aabb, const TAABB &Aabb1);
extern DELPHI_PACKAGE void __fastcall SetBB(THmgBoundingBox &C, const Glvectortypes::TVector4f &V);
extern DELPHI_PACKAGE void __fastcall SetAABB(TAABB &Bb, const Glvectortypes::TVector4f &V);
extern DELPHI_PACKAGE void __fastcall BBTransform(THmgBoundingBox &C, const Glvectortypes::TMatrix4f &M);
extern DELPHI_PACKAGE void __fastcall AABBTransform(TAABB &Bb, const Glvectortypes::TMatrix4f &M);
extern DELPHI_PACKAGE void __fastcall AABBScale(TAABB &Bb, const Glvectortypes::TVector3f &V);
extern DELPHI_PACKAGE float __fastcall BBMinX(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxX(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMinY(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxY(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMinZ(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxZ(const THmgBoundingBox &C);
extern DELPHI_PACKAGE void __fastcall AABBInclude(TAABB &Bb, const Glvectortypes::TVector3f &P);
extern DELPHI_PACKAGE void __fastcall AABBFromSweep(TAABB &SweepAABB, const Glvectortypes::TVector4f &Start, const Glvectortypes::TVector4f &Dest, const float Radius);
extern DELPHI_PACKAGE TAABB __fastcall AABBIntersection(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE TAABB __fastcall BBToAABB(const THmgBoundingBox &ABB);
extern DELPHI_PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB)/* overload */;
extern DELPHI_PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB, const Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Glvectortypes::TVector3f &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Glvectortypes::TVector4f &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Glvectortypes::TVector3f &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Glvectortypes::TVector4f &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBBPoint(THmgBoundingBox &Bb, const Glvectortypes::TVector4f &Delta)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectAABBs(const TAABB &Aabb1, const TAABB &Aabb2, const Glvectortypes::TMatrix4f &M1To2, const Glvectortypes::TMatrix4f &M2To1)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsoluteXY(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsoluteXZ(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall AABBFitsInAABBAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall PointInAABB(const Glvectortypes::TVector3f &P, const TAABB &Aabb)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointInAABB(const Glvectortypes::TVector4f &P, const TAABB &Aabb)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PlaneIntersectAABB(const Glvectortypes::TVector3f &Normal, float D, const TAABB &Aabb);
extern DELPHI_PACKAGE Glvectorlists::TAffineVectorList* __fastcall PlaneAABBIntersection(const Glvectortypes::TVector4f &plane, const TAABB &AABB);
extern DELPHI_PACKAGE bool __fastcall TriangleIntersectAABB(const TAABB &Aabb, const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3);
extern DELPHI_PACKAGE void __fastcall ExtractAABBCorners(const TAABB &AABB, TAABBCorners &AABBCorners);
extern DELPHI_PACKAGE void __fastcall AABBToBSphere(const TAABB &AABB, TBSphere &BSphere);
extern DELPHI_PACKAGE void __fastcall BSphereToAABB(const TBSphere &BSphere, TAABB &AABB)/* overload */;
extern DELPHI_PACKAGE TAABB __fastcall BSphereToAABB(const Glvectortypes::TVector3f &Center, float Radius)/* overload */;
extern DELPHI_PACKAGE TAABB __fastcall BSphereToAABB(const Glvectortypes::TVector4f &Center, float Radius)/* overload */;
extern DELPHI_PACKAGE TSpaceContains __fastcall AABBContainsAABB(const TAABB &MainAABB, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall AABBContainsBSphere(const TAABB &MainAABB, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall PlaneContainsBSphere(const Glvectortypes::TVector3f &Location, const Glvectortypes::TVector3f &Normal, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall FrustumContainsBSphere(const Glvectorgeometry::TFrustum &Frustum, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall FrustumContainsAABB(const Glvectorgeometry::TFrustum &Frustum, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall BSphereContainsAABB(const TBSphere &MainBSphere, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall BSphereContainsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE bool __fastcall BSphereIntersectsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall ClipToAABB(const Glvectortypes::TVector3f &V, const TAABB &AABB);
extern DELPHI_PACKAGE void __fastcall IncludeInClipRect(TClipRect &ClipRect, float X, float Y);
extern DELPHI_PACKAGE TClipRect __fastcall AABBToClipRect(const TAABB &Aabb, const Glvectortypes::TMatrix4f &ModelViewProjection, int ViewportSizeX, int ViewportSizeY);
extern DELPHI_PACKAGE bool __fastcall RayCastAABBIntersect(const Glvectortypes::TVector4f &RayOrigin, const Glvectortypes::TVector4f &RayDirection, const TAABB &Aabb, /* out */ float &TNear, /* out */ float &TFar)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastAABBIntersect(const Glvectortypes::TVector4f &RayOrigin, const Glvectortypes::TVector4f &RayDirection, const TAABB &Aabb, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0))/* overload */;
}	/* namespace Glgeometrybb */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGEOMETRYBB)
using namespace Glgeometrybb;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgeometrybbHPP
