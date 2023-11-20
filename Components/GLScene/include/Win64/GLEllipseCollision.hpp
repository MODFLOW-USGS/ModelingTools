// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLEllipseCollision.pas' rev: 35.00 (Windows)

#ifndef GlellipsecollisionHPP
#define GlellipsecollisionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
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
class PASCALIMPLEMENTATION TECPlane : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<float, 4> Equation;
	Glvectortypes::TVector3f Origin;
	Glvectortypes::TVector3f Normal;
	void __fastcall MakePlane(const Glvectortypes::TVector3f &nOrigin, const Glvectortypes::TVector3f &nNormal)/* overload */;
	void __fastcall MakePlane(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3)/* overload */;
	bool __fastcall isFrontFacingTo(const Glvectortypes::TVector3f &Direction);
	float __fastcall signedDistanceTo(const Glvectortypes::TVector3f &Point);
public:
	/* TObject.Create */ inline __fastcall TECPlane() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TECPlane() { }
	
};


struct DECLSPEC_DRECORD TECObjectInfo
{
public:
	Glvectortypes::TMatrix4f AbsoluteMatrix;
	bool Solid;
	bool IsDynamic;
	int ObjectID;
};


struct DECLSPEC_DRECORD TECTriangle
{
public:
	Glvectortypes::TVector3f p1;
	Glvectortypes::TVector3f p2;
	Glvectortypes::TVector3f p3;
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
	Glvectortypes::TVector3f Position;
	Glvectortypes::TVector3f Radius;
	TECColliderShape Shape;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECCollider> TECColliderList;

struct DECLSPEC_DRECORD TECContact
{
public:
	Glvectortypes::TVector3f Position;
	Glvectortypes::TVector3f SurfaceNormal;
	float Distance;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECContact> TECContactList;

struct DECLSPEC_DRECORD TECCollisionPacket
{
public:
	Glvectortypes::TVector3f velocity;
	Glvectortypes::TVector3f normalizedVelocity;
	Glvectortypes::TVector3f basePoint;
	bool foundCollision;
	float nearestDistance;
	int NearestObject;
	Glvectortypes::TVector3f intersectionPoint;
	Glvectortypes::TVector3f intersectionNormal;
};


struct DECLSPEC_DRECORD TECMovePack
{
public:
	TECTriMeshList TriMeshes;
	TECFreeFormList Freeforms;
	TECColliderList Colliders;
	Glvectortypes::TVector3f Position;
	Glvectortypes::TVector3f Velocity;
	Glvectortypes::TVector3f Gravity;
	Glvectortypes::TVector3f Radius;
	TECObjectInfo ObjectInfo;
	float CollisionRange;
	double UnitScale;
	System::Byte MaxRecursionDepth;
	TECCollisionPacket CP;
	System::Byte collisionRecursionDepth;
	Glvectortypes::TVector3f ResultPos;
	int NearestObject;
	bool VelocityCollided;
	bool GravityCollided;
	Glvectortypes::TVector3f GroundNormal;
	TECContactList Contacts;
};


typedef System::DynamicArray<TECTriangle> Glellipsecollision__2;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float cECCloseDistance;
extern DELPHI_PACKAGE Glellipsecollision__2 debug_tri;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorDivide(const Glvectortypes::TVector3f &v, const Glvectortypes::TVector3f &divider);
extern DELPHI_PACKAGE void __fastcall CollideAndSlide(TECMovePack &MP);
extern DELPHI_PACKAGE void __fastcall CollideWithWorld(TECMovePack &MP, const Glvectortypes::TVector3f &pos, const Glvectortypes::TVector3f &vel, bool &HasCollided);
}	/* namespace Glellipsecollision */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLELLIPSECOLLISION)
using namespace Glellipsecollision;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlellipsecollisionHPP
