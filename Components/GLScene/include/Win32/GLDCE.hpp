// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDCE.pas' rev: 36.00 (Windows)

#ifndef GLDCEHPP
#define GLDCEHPP

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
#include <System.Types.hpp>
#include <GLScene.hpp>
#include <GLXCollection.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLDCEMisc.hpp>
#include <GLEllipseCollision.hpp>
#include <GLTerrainRenderer.hpp>
#include <GLCoordinates.hpp>
#include <GLBaseClasses.hpp>
#include <GLManager.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gldce
{
//-- forward type declarations -----------------------------------------------
struct TDCECollision;
class DELPHICLASS TGLDCEManager;
class DELPHICLASS TGLDCEStatic;
class DELPHICLASS TGLDCEDynamic;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDCEShape : unsigned char { csEllipsoid, csBox, csFreeform, csTerrain };

enum DECLSPEC_DENUM TDCECollisionSelection : unsigned char { ccsDCEStandard, ccsCollisionStandard, ccsHybrid };

struct DECLSPEC_DRECORD TDCECollision
{
public:
	Glvectorgeometry::TAffineVector Position;
	Glvectorgeometry::TAffineVector Normal;
	Glvectorgeometry::TAffineVector Bounce;
	bool Nearest;
	bool RootCollision;
	float Distance;
};


typedef void __fastcall (__closure *TDCECollisionEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* object1, Glscene::TGLBaseSceneObject* object2, const TDCECollision &CollisionInfo);

typedef void __fastcall (__closure *TDCEObjectCollisionEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* ObjectCollided, const TDCECollision &CollisionInfo);

class PASCALIMPLEMENTATION TGLDCEManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FStatics;
	System::Classes::TList* FDynamics;
	float FGravity;
	Glcoordinates::TGLCoordinates* FWorldDirection;
	float FWorldScale;
	float FMovimentScale;
	TDCECollisionSelection FStandardiseLayers;
	bool FManualStep;
	TDCECollisionEvent FOnCollision;
	void __fastcall SetWorldDirection(Glcoordinates::TGLCoordinates* const Value);
	void __fastcall SetWorldScale(const float Value);
	int __fastcall GetDynamicCount();
	int __fastcall GetStaticCount();
	
protected:
	void __fastcall RegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterAllStatics();
	void __fastcall RegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterAllDynamics();
	
public:
	__fastcall virtual TGLDCEManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDCEManager();
	float __fastcall MoveByDistance(TGLDCEDynamic* &Body, const Glvectorgeometry::TAffineVector &deltaS, const Glvectorgeometry::TAffineVector &deltaAbsS);
	void __fastcall Step(double deltaTime);
	__property int DynamicCount = {read=GetDynamicCount, nodefault};
	__property int StaticCount = {read=GetStaticCount, nodefault};
	
__published:
	__property float Gravity = {read=FGravity, write=FGravity};
	__property Glcoordinates::TGLCoordinates* WorldDirection = {read=FWorldDirection, write=SetWorldDirection};
	__property float WorldScale = {read=FWorldScale, write=SetWorldScale};
	__property float MovimentScale = {read=FMovimentScale, write=FMovimentScale};
	__property TDCECollisionSelection StandardiseLayers = {read=FStandardiseLayers, write=FStandardiseLayers, nodefault};
	__property bool ManualStep = {read=FManualStep, write=FManualStep, nodefault};
	__property TDCECollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


class PASCALIMPLEMENTATION TGLDCEStatic : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	TDCEShape FShape;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Glcoordinates::TGLCoordinates* FSize;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetShape(const TDCEShape Value);
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Glcoordinates::TGLCoordinates* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLDCEStatic(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLDCEStatic();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property TDCEShape Shape = {read=FShape, write=SetShape, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Glcoordinates::TGLCoordinates* Size = {read=FSize, write=SetSize};
};


enum DECLSPEC_DENUM TDCESlideOrBounce : unsigned char { csbSlide, csbBounce };

class PASCALIMPLEMENTATION TGLDCEDynamic : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	bool FUseGravity;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Glcoordinates::TGLCoordinates* FSize;
	System::Byte FMaxRecursionDepth;
	TDCESlideOrBounce FSlideOrBounce;
	Glvectorgeometry::TAffineVector FAccel;
	Glvectorgeometry::TAffineVector FSpeed;
	Glvectorgeometry::TAffineVector FAbsAccel;
	Glvectorgeometry::TAffineVector FAbsSpeed;
	Glvectorgeometry::TAffineVector FGravSpeed;
	float FTotalFriction;
	bool FInGround;
	Glvectorgeometry::TAffineVector FGroundNormal;
	float FJumpHeight;
	float FJumpForce;
	float FJumpSpeed;
	bool FJumping;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Glcoordinates::TGLCoordinates* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLDCEDynamic(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLDCEDynamic();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	void __fastcall ApplyAccel(const Glvectorgeometry::TAffineVector &NewAccel)/* overload */;
	void __fastcall ApplyAccel(float x, float y, float z)/* overload */;
	void __fastcall ApplyAbsAccel(const Glvectorgeometry::TAffineVector &NewAccel)/* overload */;
	void __fastcall ApplyAbsAccel(float x, float y, float z)/* overload */;
	void __fastcall StopAccel();
	void __fastcall StopAbsAccel();
	void __fastcall Jump(float jHeight, float jSpeed);
	void __fastcall Move(const Glvectorgeometry::TAffineVector &deltaS, double deltaTime);
	void __fastcall MoveTo(const Glvectorgeometry::TAffineVector &Position, float Amount);
	void __fastcall DoMove(double deltaTime);
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	__property Glvectorgeometry::TAffineVector Speed = {read=FSpeed, write=FSpeed};
	__property bool InGround = {read=FInGround, nodefault};
	__property System::Byte MaxRecursionDepth = {read=FMaxRecursionDepth, write=FMaxRecursionDepth, nodefault};
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property bool UseGravity = {read=FUseGravity, write=FUseGravity, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Glcoordinates::TGLCoordinates* Size = {read=FSize, write=SetSize};
	__property TDCESlideOrBounce SlideOrBounce = {read=FSlideOrBounce, write=FSlideOrBounce, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Glscene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Gldce */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDCE)
using namespace Gldce;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLDCEHPP
