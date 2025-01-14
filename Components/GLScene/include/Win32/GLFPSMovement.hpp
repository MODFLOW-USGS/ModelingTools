// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFPSMovement.pas' rev: 36.00 (Windows)

#ifndef GlfpsmovementHPP
#define GlfpsmovementHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <OpenGLTokens.hpp>
#include <GLCoordinates.hpp>
#include <GLVectorTypes.hpp>
#include <GLContext.hpp>
#include <GLVectorGeometry.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorLists.hpp>
#include <GLXCollection.hpp>
#include <GLGeomObjects.hpp>
#include <GLNavigator.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>
#include <GLManager.hpp>
#include <GLState.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfpsmovement
{
//-- forward type declarations -----------------------------------------------
struct TContactPoint;
class DELPHICLASS TCollisionState;
class DELPHICLASS TCollisionStates;
class DELPHICLASS TGLMapCollectionItem;
class DELPHICLASS TGLMapCollection;
class DELPHICLASS TGLFPSMovementManager;
class DELPHICLASS TGLBFPSMovement;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TContactPoint
{
public:
	Glvectorgeometry::TVector intPoint;
	Glvectorgeometry::TVector intNormal;
};


class PASCALIMPLEMENTATION TCollisionState : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Glvectorgeometry::TVector Position;
	TContactPoint Contact;
	__int64 Time;
public:
	/* TObject.Create */ inline __fastcall TCollisionState() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCollisionState() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCollisionStates : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCollisionStates() { }
	
public:
	/* TObject.Create */ inline __fastcall TCollisionStates() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMapCollectionItem : public Glxcollection::TXCollectionItem
{
	typedef Glxcollection::TXCollectionItem inherited;
	
private:
	Glvectorfileobjects::TGLFreeForm* FMap;
	System::UnicodeString FMapName;
	int FCollisionGroup;
	void __fastcall setMap(Glvectorfileobjects::TGLFreeForm* value);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLMapCollectionItem(Glxcollection::TXCollection* aOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property Glvectorfileobjects::TGLFreeForm* Map = {read=FMap, write=setMap};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLMapCollectionItem() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLMapCollectionItemClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMapCollection : public Glxcollection::TXCollection
{
	typedef Glxcollection::TXCollection inherited;
	
public:
	__classmethod virtual Glxcollection::TXCollectionItemClass __fastcall ItemsClass();
	TGLMapCollectionItem* __fastcall addMap(Glvectorfileobjects::TGLFreeForm* Map, int CollisionGroup = 0x0);
	TGLMapCollectionItem* __fastcall findMap(Glvectorfileobjects::TGLFreeForm* mapFreeForm);
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLMapCollection(System::Classes::TPersistent* aOwner) : Glxcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLMapCollection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLFPSMovementManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glnavigator::TGLNavigator* FNavigator;
	int FDisplayTime;
	float FMovementScale;
	TGLMapCollection* FMaps;
	Glscene::TGLScene* FScene;
	void __fastcall SetNavigator(Glnavigator::TGLNavigator* value);
	void __fastcall setScene(Glscene::TGLScene* value);
	void __fastcall DrawArrows(const Glvectorgeometry::TVector &intPoint, const Glvectorgeometry::TVector &intNormal, const Glvectorgeometry::TVector &Ray, Glgeomobjects::TGLArrowLine* Arrow1, Glgeomobjects::TGLArrowLine* Arrow2);
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteMaps(System::Classes::TStream* stream);
	void __fastcall ReadMaps(System::Classes::TStream* stream);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLFPSMovementManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLFPSMovementManager();
	bool __fastcall SphereSweepAndSlide(Glvectorfileobjects::TGLFreeForm* freeform, TGLBFPSMovement* behaviour, const Glvectorgeometry::TVector &SphereStart, Glvectorgeometry::TVector &Velocity, Glvectorgeometry::TVector &newPosition, float sphereRadius)/* overload */;
	void __fastcall SphereSweepAndSlide(TGLBFPSMovement* behaviour, const Glvectorgeometry::TVector &SphereStart, Glvectorgeometry::TVector &Velocity, Glvectorgeometry::TVector &newPosition, float sphereRadius)/* overload */;
	
__published:
	__property TGLMapCollection* Maps = {read=FMaps, write=FMaps};
	__property Glnavigator::TGLNavigator* Navigator = {read=FNavigator, write=SetNavigator};
	__property Glscene::TGLScene* Scene = {read=FScene, write=setScene};
	__property int DisplayTime = {read=FDisplayTime, write=FDisplayTime, nodefault};
	__property float MovementScale = {read=FMovementScale, write=FMovementScale};
};


class PASCALIMPLEMENTATION TGLBFPSMovement : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLFPSMovementManager* FManager;
	TCollisionStates* CollisionStates;
	Glgeomobjects::TGLArrowLine* ArrowLine1;
	Glgeomobjects::TGLArrowLine* ArrowLine2;
	Glgeomobjects::TGLArrowLine* ArrowLine3;
	Glgeomobjects::TGLArrowLine* ArrowLine4;
	Glgeomobjects::TGLArrowLine* ArrowLine5;
	Glgeomobjects::TGLArrowLine* ArrowLine6;
	Glscene::TGLDirectOpenGL* dirGl;
	__int64 tickCount;
	Glvectorgeometry::TVector oldPosition;
	bool FGravityEnabled;
	float FSphereRadius;
	bool FShowArrows;
	int FCollisionGroup;
	System::UnicodeString FManagerName;
	void __fastcall setShowArrows(bool value);
	void __fastcall RenderArrowLines(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	Glvectorgeometry::TVector Velocity;
	__fastcall virtual TGLBFPSMovement(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFPSMovement();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	void __fastcall TurnHorizontal(float Angle);
	void __fastcall TurnVertical(float Angle);
	void __fastcall MoveForward(float Distance);
	void __fastcall StrafeHorizontal(float Distance);
	void __fastcall StrafeVertical(float Distance);
	void __fastcall Straighten();
	
__published:
	__property TGLFPSMovementManager* Manager = {read=FManager, write=FManager};
	__property float sphereRadius = {read=FSphereRadius, write=FSphereRadius};
	__property bool ShowArrows = {read=FShowArrows, write=setShowArrows, nodefault};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
	__property bool GravityEnabled = {read=FGravityEnabled, write=FGravityEnabled, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Glscene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Glscene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glfpsmovement */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFPSMOVEMENT)
using namespace Glfpsmovement;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfpsmovementHPP
