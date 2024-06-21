// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMaterialMultiProxy.pas' rev: 36.00 (Windows)

#ifndef GlmaterialmultiproxyHPP
#define GlmaterialmultiproxyHPP

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
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLSilhouette.hpp>
#include <GLStrings.hpp>
#include <GLCrossPlatform.hpp>
#include <GLPersistentClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>
#include <GLContext.hpp>
#include <GLVectorTypes.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLCoordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmaterialmultiproxy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMaterialMultiProxyMaster;
class DELPHICLASS TGLMaterialMultiProxyMasters;
class DELPHICLASS TGLMaterialMultiProxy;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialMultiProxyMaster : public Glpersistentclasses::TGLInterfacedCollectionItem
{
	typedef Glpersistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Glscene::TGLBaseSceneObject* FMasterObject;
	Glmaterial::TGLLibMaterial* FMasterLibMaterial;
	Glmaterial::TGLLibMaterialName FTempLibMaterialName;
	float FDistanceMin2;
	float FDistanceMax2;
	void __fastcall SetMasterLibMaterialName(const Glmaterial::TGLLibMaterialName Value);
	Glmaterial::TGLLibMaterialName __fastcall GetMasterLibMaterialName();
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetMasterObject(Glscene::TGLBaseSceneObject* const Val);
	void __fastcall SetDistanceMin(const float Val);
	void __fastcall SetDistanceMax(const float Val);
	float __fastcall GetDistanceMin();
	float __fastcall GetDistanceMax();
	
public:
	__fastcall virtual TGLMaterialMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMaterialMultiProxyMaster();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMaterialMultiProxy* __fastcall OwnerObject();
	void __fastcall NotifyChange();
	__property Glmaterial::TGLLibMaterial* MasterLibMaterial = {read=FMasterLibMaterial, write=FMasterLibMaterial, stored=false};
	
__published:
	__property Glscene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property Glmaterial::TGLLibMaterialName MasterLibMaterialName = {read=GetMasterLibMaterialName, write=SetMasterLibMaterialName};
	__property float DistanceMin = {read=GetDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=GetDistanceMax, write=SetDistanceMax};
private:
	void *__IGLMaterialLibrarySupported;	// Glmaterial::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMaterialMultiProxyMaster* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMaterialMultiProxyMaster* const Val);
	TGLMaterialMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent);
	
public:
	__fastcall TGLMaterialMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add()/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* Master, float DistanceMin, float DistanceMax)/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* Master, Glmaterial::TGLLibMaterial* MasterLibMaterial, float DistanceMin, float DistanceMax)/* overload */;
	__property TGLMaterialMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMaterialMultiProxyMasters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMaterialMultiProxy : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	TGLMaterialMultiProxyMasters* FMasterObjects;
	bool FRendering;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	
protected:
	void __fastcall SetMasterObjects(TGLMaterialMultiProxyMasters* const Val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Glscene::TGLBaseSceneObject* __fastcall PrimaryMaster();
	
public:
	__fastcall virtual TGLMaterialMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialMultiProxy();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectorgeometry::TVector &rayStart, const Glvectorgeometry::TVector &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMaterialMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
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
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMaterialMultiProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmaterialmultiproxy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMATERIALMULTIPROXY)
using namespace Glmaterialmultiproxy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmaterialmultiproxyHPP
