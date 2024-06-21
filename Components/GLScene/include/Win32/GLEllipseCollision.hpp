// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLEllipseCollision.pas' rev: 36.00 (Windows)

#ifndef GlellipsecollisionHPP
#define GlellipsecollisionHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLVectorGeometry.hpp>
#include <GLOctree.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glellipsecollision
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TECPlane;
struct TECObjectInfo;
struct TECTriangle;
struct TECTriMesh;
struct TECFreeForm;
struct TECCollider;
struct TECContact;
struct TECCollisionPacket;
struct TECMovePack;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TECPlane : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<float, 4> Equation;
	Glvectorgeometry::TAffineVector Origin;
	Glvectorgeometry::TAffineVector Normal;
	void __fastcall MakePlane(const Glvectorgeometry::TAffineVector &nOrigin, const Glvectorgeometry::TAffineVector &nNormal)/* overload */;
	void __fastcall MakePlane(const Glvectorgeometry::TAffineVector &p1, const Glvectorgeometry::TAffineVector &p2, const Glvectorgeometry::TAffineVector &p3)/* overload */;
	bool __fastcall isFrontFacingTo(const Glvectorgeometry::TAffineVector &Direction);
	float __fastcall signedDistanceTo(const Glvectorgeometry::TAffineVector &Point);
public:
	/* TObject.Create */ inline __fastcall TECPlane() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TECPlane() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TECObjectInfo
{
public:
	Glvectorgeometry::TMatrix AbsoluteMatrix;
	bool Solid;
	bool IsDynamic;
	int ObjectID;
};


struct DECLSPEC_DRECORD TECTriangle
{
public:
	Glvectorgeometry::TAffineVector p1;
	Glvectorgeometry::TAffineVector p2;
	Glvectorgeometry::TAffineVector p3;
};


struct DECLSPEC_DRECORD TECTriMesh
{
	
private:
	typedef System::DynamicArray<TECTriangle> _TECTriMesh__1;
	
	
public:
	_TECTriMesh__1 Triangles;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECTriMesh> TECTriMeshList;

struct DECLSPEC_DRECORD TECFreeForm
{
	
private:
	typedef System::DynamicArray<Gloctree::POctreeNode> _TECFreeForm__1;
	
	
public:
	_TECFreeForm__1 OctreeNodes;
	Glvectorlists::TAffineVectorList* *triangleFiler;
	bool InvertedNormals;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECFreeForm> TECFreeFormList;

enum DECLSPEC_DENUM TECColliderShape : unsigned char { csEllipsoid, csPoint };

struct DECLSPEC_DRECORD TECCollider
{
public:
	Glvectorgeometry::TAffineVector Position;
	Glvectorgeometry::TAffineVector Radius;
	TECColliderShape Shape;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECCollider> TECColliderList;

struct DECLSPEC_DRECORD TECContact
{
public:
	Glvectorgeometry::TAffineVector Position;
	Glvectorgeometry::TAffineVector SurfaceNormal;
	float Distance;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECContact> TECContactList;

struct DECLSPEC_DRECORD TECCollisionPacket
{
public:
	Glvectorgeometry::TAffineVector velocity;
	Glvectorgeometry::TAffineVector normalizedVelocity;
	Glvectorgeometry::TAffineVector basePoint;
	bool foundCollision;
	float nearestDistance;
	int NearestObject;
	Glvectorgeometry::TAffineVector intersectionPoint;
	Glvectorgeometry::TAffineVector intersectionNormal;
};


struct DECLSPEC_DRECORD TECMovePack
{
public:
	TECTriMeshList TriMeshes;
	TECFreeFormList Freeforms;
	TECColliderList Colliders;
	Glvectorgeometry::TAffineVector Position;
	Glvectorgeometry::TAffineVector Velocity;
	Glvectorgeometry::TAffineVector Gravity;
	Glvectorgeometry::TAffineVector Radius;
	TECObjectInfo ObjectInfo;
	float CollisionRange;
	double UnitScale;
	System::Byte MaxRecursionDepth;
	TECCollisionPacket CP;
	System::Byte collisionRecursionDepth;
	Glvectorgeometry::TAffineVector ResultPos;
	int NearestObject;
	bool VelocityCollided;
	bool GravityCollided;
	Glvectorgeometry::TAffineVector GroundNormal;
	TECContactList Contacts;
};


typedef System::DynamicArray<TECTriangle> Glellipsecollision__2;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float cECCloseDistance;
extern DELPHI_PACKAGE Glellipsecollision__2 debug_tri;
extern DELPHI_PACKAGE Glvectorgeometry::TAffineVector __fastcall VectorDivide(const Glvectorgeometry::TAffineVector &v, const Glvectorgeometry::TAffineVector &divider);
extern DELPHI_PACKAGE void __fastcall CollideAndSlide(TECMovePack &MP);
extern DELPHI_PACKAGE void __fastcall CollideWithWorld(TECMovePack &MP, const Glvectorgeometry::TAffineVector &pos, const Glvectorgeometry::TAffineVector &vel, bool &HasCollided);
}	/* namespace Glellipsecollision */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLELLIPSECOLLISION)
using namespace Glellipsecollision;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlellipsecollisionHPP
