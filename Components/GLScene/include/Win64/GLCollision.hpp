// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCollision.pas' rev: 35.00 (Windows)

#ifndef GlcollisionHPP
#define GlcollisionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLXCollection.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLGeometryBB.hpp>
#include <GLManager.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcollision
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCollisionManager;
class DELPHICLASS TGLBCollision;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TObjectCollisionEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* object1, Glscene::TGLBaseSceneObject* object2);

enum DECLSPEC_DENUM TCollisionBoundingMode : unsigned char { cbmPoint, cbmSphere, cbmEllipsoid, cbmCube, cbmFaces };

typedef bool __fastcall (*TFastCollisionChecker)(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);

typedef TFastCollisionChecker *PFastCollisionChecker;

class PASCALIMPLEMENTATION TGLCollisionManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FClients;
	TObjectCollisionEvent FOnCollision;
	
protected:
	void __fastcall RegisterClient(TGLBCollision* aClient);
	void __fastcall DeRegisterClient(TGLBCollision* aClient);
	void __fastcall DeRegisterAllClients();
	
public:
	__fastcall virtual TGLCollisionManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCollisionManager();
	void __fastcall CheckCollisions();
	
__published:
	__property TObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


class PASCALIMPLEMENTATION TGLBCollision : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TCollisionBoundingMode FBoundingMode;
	TGLCollisionManager* FManager;
	System::UnicodeString FManagerName;
	int FGroupIndex;
	
protected:
	void __fastcall SetGroupIndex(const int value);
	void __fastcall SetManager(TGLCollisionManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLBCollision(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBCollision();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property TGLCollisionManager* Manager = {read=FManager, write=SetManager};
	__property TCollisionBoundingMode BoundingMode = {read=FBoundingMode, write=FBoundingMode, nodefault};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsPoint(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsSphere(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsEllipsoid(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsCube(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsPoint(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsSphere(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsEllipsoid(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsCube(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsPoint(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsSphere(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsEllipsoid(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsCube(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsPoint(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsSphere(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsEllipsoid(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsCube(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsFace(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckFaceVsCube(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckFaceVsFace(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall IntersectCubes(Glscene::TGLBaseSceneObject* obj1, Glscene::TGLBaseSceneObject* obj2)/* overload */;
extern DELPHI_PACKAGE TGLBCollision* __fastcall GetOrCreateCollision(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBCollision* __fastcall GetOrCreateCollision(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glcollision */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOLLISION)
using namespace Glcollision;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcollisionHPP
