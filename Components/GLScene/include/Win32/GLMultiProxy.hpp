﻿// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiProxy.pas' rev: 36.00 (Windows)

#ifndef GLMultiProxyHPP
#define GLMultiProxyHPP

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
#include <OpenGLTokens.hpp>
#include <GLPersistentClasses.hpp>
#include <GLContext.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLSilhouette.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmultiproxy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultiProxyMaster;
class DELPHICLASS TGLMultiProxyMasters;
class DELPHICLASS TGLMultiProxy;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMaster : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glscene::TGLBaseSceneObject* FMasterObject;
	float FDistanceMin;
	float FDistanceMin2;
	float FDistanceMax;
	float FDistanceMax2;
	bool FVisible;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetMasterObject(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetDistanceMin(const float val);
	void __fastcall SetDistanceMax(const float val);
	void __fastcall SetVisible(const bool val);
	
public:
	__fastcall virtual TGLMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMultiProxyMaster();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMultiProxy* __fastcall OwnerObject();
	void __fastcall NotifyChange();
	
__published:
	__property Glscene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property float DistanceMin = {read=FDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=FDistanceMax, write=SetDistanceMax};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMultiProxyMaster* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMultiProxyMaster* const val);
	TGLMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMultiProxyMaster* __fastcall Add()/* overload */;
	HIDESBASE TGLMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* master, float distanceMin, float distanceMax)/* overload */;
	__property TGLMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall Notification(System::Classes::TComponent* AComponent);
	void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMultiProxyMasters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMultiProxy : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TGLMultiProxyMasters* FMasterObjects;
	bool FRendering;
	
protected:
	void __fastcall SetMasterObjects(TGLMultiProxyMasters* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Glscene::TGLBaseSceneObject* __fastcall PrimaryMaster();
	
public:
	__fastcall virtual TGLMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMultiProxy();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectorgeometry::TVector &rayStart, const Glvectorgeometry::TVector &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
	__property ObjectsSorting = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMultiProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultiproxy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIPROXY)
using namespace Glmultiproxy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLMultiProxyHPP
