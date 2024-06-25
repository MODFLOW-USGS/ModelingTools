// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGeometryBB.pas' rev: 36.00 (Windows)

#ifndef GlgeometrybbHPP
#define GlgeometrybbHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
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
	Glvectorgeometry::TAffineVector Min;
	Glvectorgeometry::TAffineVector Max;
};


typedef TAABB *PAABB;

struct DECLSPEC_DRECORD TBSphere
{
public:
	Glvectorgeometry::TAffineVector Center;
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
extern DELPHI_PACKAGE void __fastcall SetBB(THmgBoundingBox &C, const Glvectorgeometry::TVector &V);
extern DELPHI_PACKAGE void __fastcall SetAABB(TAABB &Bb, const Glvectorgeometry::TVector &V);
extern DELPHI_PACKAGE void __fastcall BBTransform(THmgBoundingBox &C, const Glvectorgeometry::TMatrix &M);
extern DELPHI_PACKAGE void __fastcall AABBTransform(TAABB &Bb, const Glvectorgeometry::TMatrix &M);
extern DELPHI_PACKAGE void __fastcall AABBScale(TAABB &Bb, const Glvectorgeometry::TAffineVector &V);
extern DELPHI_PACKAGE float __fastcall BBMinX(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxX(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMinY(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxY(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMinZ(const THmgBoundingBox &C);
extern DELPHI_PACKAGE float __fastcall BBMaxZ(const THmgBoundingBox &C);
extern DELPHI_PACKAGE void __fastcall AABBInclude(TAABB &Bb, const Glvectorgeometry::TAffineVector &P);
extern DELPHI_PACKAGE void __fastcall AABBFromSweep(TAABB &SweepAABB, const Glvectorgeometry::TVector &Start, const Glvectorgeometry::TVector &Dest, const float Radius);
extern DELPHI_PACKAGE TAABB __fastcall AABBIntersection(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE TAABB __fastcall BBToAABB(const THmgBoundingBox &ABB);
extern DELPHI_PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB)/* overload */;
extern DELPHI_PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB, const Glvectorgeometry::TMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Glvectorgeometry::TAffineVector &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Glvectorgeometry::TVector &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Glvectorgeometry::TAffineVector &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Glvectorgeometry::TVector &Delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetBBPoint(THmgBoundingBox &Bb, const Glvectorgeometry::TVector &Delta)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectAABBs(const TAABB &Aabb1, const TAABB &Aabb2, const Glvectorgeometry::TMatrix &M1To2, const Glvectorgeometry::TMatrix &M2To1)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsoluteXY(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsoluteXZ(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall IntersectAABBsAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall AABBFitsInAABBAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern DELPHI_PACKAGE bool __fastcall PointInAABB(const Glvectorgeometry::TAffineVector &P, const TAABB &Aabb)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointInAABB(const Glvectorgeometry::TVector &P, const TAABB &Aabb)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PlaneIntersectAABB(const Glvectorgeometry::TAffineVector &Normal, float D, const TAABB &Aabb);
extern DELPHI_PACKAGE Glvectorlists::TAffineVectorList* __fastcall PlaneAABBIntersection(const Glvectorgeometry::THmgPlane &plane, const TAABB &AABB);
extern DELPHI_PACKAGE bool __fastcall TriangleIntersectAABB(const TAABB &Aabb, const Glvectorgeometry::TAffineVector &V1, const Glvectorgeometry::TAffineVector &V2, const Glvectorgeometry::TAffineVector &V3);
extern DELPHI_PACKAGE void __fastcall ExtractAABBCorners(const TAABB &AABB, TAABBCorners &AABBCorners);
extern DELPHI_PACKAGE void __fastcall AABBToBSphere(const TAABB &AABB, TBSphere &BSphere);
extern DELPHI_PACKAGE void __fastcall BSphereToAABB(const TBSphere &BSphere, TAABB &AABB)/* overload */;
extern DELPHI_PACKAGE TAABB __fastcall BSphereToAABB(const Glvectorgeometry::TAffineVector &Center, float Radius)/* overload */;
extern DELPHI_PACKAGE TAABB __fastcall BSphereToAABB(const Glvectorgeometry::TVector &Center, float Radius)/* overload */;
extern DELPHI_PACKAGE TSpaceContains __fastcall AABBContainsAABB(const TAABB &MainAABB, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall AABBContainsBSphere(const TAABB &MainAABB, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall PlaneContainsBSphere(const Glvectorgeometry::TAffineVector &Location, const Glvectorgeometry::TAffineVector &Normal, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall FrustumContainsBSphere(const Glvectorgeometry::TFrustum &Frustum, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE TSpaceContains __fastcall FrustumContainsAABB(const Glvectorgeometry::TFrustum &Frustum, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall BSphereContainsAABB(const TBSphere &MainBSphere, const TAABB &TestAABB);
extern DELPHI_PACKAGE TSpaceContains __fastcall BSphereContainsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE bool __fastcall BSphereIntersectsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern DELPHI_PACKAGE Glvectorgeometry::TAffineVector __fastcall ClipToAABB(const Glvectorgeometry::TAffineVector &V, const TAABB &AABB);
extern DELPHI_PACKAGE void __fastcall IncludeInClipRect(TClipRect &ClipRect, float X, float Y);
extern DELPHI_PACKAGE TClipRect __fastcall AABBToClipRect(const TAABB &Aabb, const Glvectorgeometry::TMatrix &ModelViewProjection, int ViewportSizeX, int ViewportSizeY);
extern DELPHI_PACKAGE bool __fastcall RayCastAABBIntersect(const Glvectorgeometry::TVector &RayOrigin, const Glvectorgeometry::TVector &RayDirection, const TAABB &Aabb, /* out */ float &TNear, /* out */ float &TFar)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastAABBIntersect(const Glvectorgeometry::TVector &RayOrigin, const Glvectorgeometry::TVector &RayDirection, const TAABB &Aabb, Glvectorgeometry::PVector IntersectPoint = (Glvectorgeometry::PVector)(0x0))/* overload */;
}	/* namespace Glgeometrybb */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGEOMETRYBB)
using namespace Glgeometrybb;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgeometrybbHPP
