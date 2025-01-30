// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletSkeletonColliders.pas' rev: 36.00 (Windows)

#ifndef GLVerletSkeletonCollidersHPP
#define GLVerletSkeletonCollidersHPP

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
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVerletTypes.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glverletskeletoncolliders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSCVerletBase;
class DELPHICLASS TSCVerletSphere;
class DELPHICLASS TSCVerletCapsule;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletBase : public Glvectorfileobjects::TGLSkeletonCollider
{
	typedef Glvectorfileobjects::TGLSkeletonCollider inherited;
	
private:
	Glverlettypes::TVerletConstraint* FVerletConstraint;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Glverlettypes::TGLVerletWorld* VerletWorld);
	__property Glverlettypes::TVerletConstraint* VerletConstraint = {read=FVerletConstraint};
public:
	/* TGLSkeletonCollider.Create */ inline __fastcall virtual TSCVerletBase() : Glvectorfileobjects::TGLSkeletonCollider() { }
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletBase(Glvectorfileobjects::TGLSkeletonColliderList* AOwner) : Glvectorfileobjects::TGLSkeletonCollider(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletBase(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TGLSkeletonCollider(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletBase() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletSphere : public TSCVerletBase
{
	typedef TSCVerletBase inherited;
	
private:
	float FRadius;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	__fastcall virtual TSCVerletSphere();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Glverlettypes::TGLVerletWorld* VerletWorld);
	virtual void __fastcall AlignCollider();
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletSphere(Glvectorfileobjects::TGLSkeletonColliderList* AOwner) : TSCVerletBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletSphere(Glpersistentclasses::TVirtualReader* reader) : TSCVerletBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletSphere() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletCapsule : public TSCVerletBase
{
	typedef TSCVerletBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	__fastcall virtual TSCVerletCapsule();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Glverlettypes::TGLVerletWorld* VerletWorld);
	virtual void __fastcall AlignCollider();
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletCapsule(Glvectorfileobjects::TGLSkeletonColliderList* AOwner) : TSCVerletBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletCapsule(Glpersistentclasses::TVirtualReader* reader) : TSCVerletBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletCapsule() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall AddSCVerletConstriantsToVerletWorld(Glvectorfileobjects::TGLSkeletonColliderList* colliders, Glverlettypes::TGLVerletWorld* world);
}	/* namespace Glverletskeletoncolliders */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETSKELETONCOLLIDERS)
using namespace Glverletskeletoncolliders;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLVerletSkeletonCollidersHPP
