// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBehaviours.pas' rev: 36.00 (Windows)

#ifndef GlbehavioursHPP
#define GlbehavioursHPP

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
#include <GLVectorTypes.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLXCollection.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbehaviours
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDamping;
class DELPHICLASS TGLBInertia;
class DELPHICLASS TGLBAcceleration;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDamping : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
private:
	float FConstant;
	float FLinear;
	float FQuadratic;
	
public:
	__fastcall virtual TGLDamping(System::Classes::TPersistent* aOwner);
	__fastcall virtual ~TGLDamping();
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	double __fastcall Calculate(double speed, double deltaTime);
	System::UnicodeString __fastcall AsString(TGLDamping* const damping);
	void __fastcall SetDamping(const float constant = 0.000000E+00f, const float linear = 0.000000E+00f, const float quadratic = 0.000000E+00f);
	
__published:
	__property float Constant = {read=FConstant, write=FConstant};
	__property float Linear = {read=FLinear, write=FLinear};
	__property float Quadratic = {read=FQuadratic, write=FQuadratic};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBInertia : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	float FMass;
	Glcoordinates::TGLCoordinates* FTranslationSpeed;
	float FTurnSpeed;
	float FRollSpeed;
	float FPitchSpeed;
	TGLDamping* FTranslationDamping;
	TGLDamping* FRotationDamping;
	bool FDampingEnabled;
	
protected:
	void __fastcall SetTranslationSpeed(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetTranslationDamping(TGLDamping* const val);
	void __fastcall SetRotationDamping(TGLDamping* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall virtual TGLBInertia(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBInertia();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	void __fastcall ApplyTranslationAcceleration(const double deltaTime, const Glvectorgeometry::TVector &accel);
	void __fastcall ApplyForce(const double deltaTime, const Glvectorgeometry::TVector &Force);
	void __fastcall ApplyTorque(const double deltaTime, const float turnTorque, const float rollTorque, const float pitchTorque);
	void __fastcall MirrorTranslation();
	void __fastcall SurfaceBounce(const Glvectorgeometry::TVector &surfaceNormal, float restitution);
	
__published:
	__property float Mass = {read=FMass, write=FMass};
	__property Glcoordinates::TGLCoordinates* TranslationSpeed = {read=FTranslationSpeed, write=SetTranslationSpeed};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed};
	__property float RollSpeed = {read=FRollSpeed, write=FRollSpeed};
	__property float PitchSpeed = {read=FPitchSpeed, write=FPitchSpeed};
	__property bool DampingEnabled = {read=FDampingEnabled, write=FDampingEnabled, nodefault};
	__property TGLDamping* TranslationDamping = {read=FTranslationDamping, write=SetTranslationDamping};
	__property TGLDamping* RotationDamping = {read=FRotationDamping, write=SetRotationDamping};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBAcceleration : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	Glcoordinates::TGLCoordinates* FAcceleration;
	
protected:
	void __fastcall SetAcceleration(Glcoordinates::TGLCoordinates* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall virtual TGLBAcceleration(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBAcceleration();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates* Acceleration = {read=FAcceleration, write=FAcceleration};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetInertia(Glscene::TGLBaseSceneObject* const AGLSceneObject);
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetOrCreateInertia(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetOrCreateInertia(Glscene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLBAcceleration* __fastcall GetOrCreateAcceleration(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBAcceleration* __fastcall GetOrCreateAcceleration(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glbehaviours */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBEHAVIOURS)
using namespace Glbehaviours;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbehavioursHPP
