// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShadowVolume.pas' rev: 35.00 (Windows)

#ifndef GlshadowvolumeHPP
#define GlshadowvolumeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLSLog.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLSilhouette.hpp>
#include <GLPersistentClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLGeometryBB.hpp>
#include <GLColor.hpp>
#include <GLSelection.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorLists.hpp>
#include <GLState.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glshadowvolume
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShadowVolumeCaster;
class DELPHICLASS TGLShadowVolumeOccluder;
class DELPHICLASS TGLShadowVolumeLight;
class DELPHICLASS TGLShadowVolumeCasters;
class DELPHICLASS TGLShadowVolume;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLShadowVolumeCapping : unsigned char { svcDefault, svcAlways, svcNever };

enum DECLSPEC_DENUM TGLShadowCastingMode : unsigned char { scmAlways, scmVisible, scmRecursivelyVisible, scmParentVisible, scmParentRecursivelyVisible };

class PASCALIMPLEMENTATION TGLShadowVolumeCaster : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glscene::TGLBaseSceneObject* FCaster;
	float FEffectiveRadius;
	TGLShadowVolumeCapping FCapping;
	TGLShadowCastingMode FCastingMode;
	
protected:
	void __fastcall SetCaster(Glscene::TGLBaseSceneObject* const val);
	TGLShadowVolume* __fastcall GetGLShadowVolume();
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLShadowVolumeCaster(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLShadowVolumeCaster();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property Glscene::TGLBaseSceneObject* Caster = {read=FCaster, write=SetCaster};
	__property TGLShadowVolume* GLShadowVolume = {read=GetGLShadowVolume};
	
__published:
	__property float EffectiveRadius = {read=FEffectiveRadius, write=FEffectiveRadius};
	__property TGLShadowVolumeCapping Capping = {read=FCapping, write=FCapping, default=0};
	__property TGLShadowCastingMode CastingMode = {read=FCastingMode, write=FCastingMode, default=2};
};


class PASCALIMPLEMENTATION TGLShadowVolumeOccluder : public TGLShadowVolumeCaster
{
	typedef TGLShadowVolumeCaster inherited;
	
__published:
	__property Caster;
public:
	/* TGLShadowVolumeCaster.Create */ inline __fastcall virtual TGLShadowVolumeOccluder(System::Classes::TCollection* ACollection) : TGLShadowVolumeCaster(ACollection) { }
	/* TGLShadowVolumeCaster.Destroy */ inline __fastcall virtual ~TGLShadowVolumeOccluder() { }
	
};


class PASCALIMPLEMENTATION TGLShadowVolumeLight : public TGLShadowVolumeCaster
{
	typedef TGLShadowVolumeCaster inherited;
	
private:
	Glpersistentclasses::TPersistentObjectList* FSilhouettes;
	
protected:
	Glscene::TGLLightSource* __fastcall GetLightSource();
	void __fastcall SetLightSource(Glscene::TGLLightSource* const ls);
	Glsilhouette::TGLSilhouette* __fastcall GetCachedSilhouette(int AIndex);
	void __fastcall StoreCachedSilhouette(int AIndex, Glsilhouette::TGLSilhouette* ASil);
	bool __fastcall SetupScissorRect(Glgeometrybb::PAABB worldAABB, Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLShadowVolumeLight(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLShadowVolumeLight();
	void __fastcall FlushSilhouetteCache();
	
__published:
	__property Glscene::TGLLightSource* LightSource = {read=GetLightSource, write=SetLightSource};
};


class PASCALIMPLEMENTATION TGLShadowVolumeCasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLShadowVolumeCaster* operator[](int index) { return this->Items[index]; }
	
protected:
	TGLShadowVolumeCaster* __fastcall GetItems(int index);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	
public:
	TGLShadowVolumeCaster* __fastcall AddCaster(Glscene::TGLBaseSceneObject* obj, float effectiveRadius = 0.000000E+00f, TGLShadowCastingMode CastingMode = (TGLShadowCastingMode)(0x2));
	void __fastcall RemoveCaster(Glscene::TGLBaseSceneObject* obj);
	int __fastcall IndexOfCaster(Glscene::TGLBaseSceneObject* obj);
	__property TGLShadowVolumeCaster* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLShadowVolumeCasters(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLShadowVolumeCasters() { }
	
};


enum DECLSPEC_DENUM TGLShadowVolumeOption : unsigned char { svoShowVolumes, svoCacheSilhouettes, svoScissorClips, svoWorldScissorClip, svoDesignVisible };

typedef System::Set<TGLShadowVolumeOption, TGLShadowVolumeOption::svoShowVolumes, TGLShadowVolumeOption::svoDesignVisible> TGLShadowVolumeOptions;

enum DECLSPEC_DENUM TGLShadowVolumeMode : unsigned char { svmAccurate, svmDarkening, svmOff };

class PASCALIMPLEMENTATION TGLShadowVolume : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	bool FActive;
	bool FRendering;
	TGLShadowVolumeCasters* FLights;
	TGLShadowVolumeCasters* FOccluders;
	TGLShadowVolumeCapping FCapping;
	TGLShadowVolumeOptions FOptions;
	TGLShadowVolumeMode FMode;
	Glcolor::TGLColor* FDarkeningColor;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetActive(const bool val);
	void __fastcall SetLights(TGLShadowVolumeCasters* const val);
	void __fastcall SetOccluders(TGLShadowVolumeCasters* const val);
	void __fastcall SetOptions(const TGLShadowVolumeOptions val);
	void __fastcall SetMode(const TGLShadowVolumeMode val);
	void __fastcall SetDarkeningColor(Glcolor::TGLColor* const val);
	
public:
	__fastcall virtual TGLShadowVolume(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowVolume();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall FlushSilhouetteCache();
	
__published:
	__property bool Active = {read=FActive, write=SetActive, default=1};
	__property TGLShadowVolumeCasters* Lights = {read=FLights, write=SetLights};
	__property TGLShadowVolumeCasters* Occluders = {read=FOccluders, write=SetOccluders};
	__property TGLShadowVolumeCapping Capping = {read=FCapping, write=FCapping, default=1};
	__property TGLShadowVolumeOptions Options = {read=FOptions, write=SetOptions, default=6};
	__property TGLShadowVolumeMode Mode = {read=FMode, write=SetMode, default=0};
	__property Glcolor::TGLColor* DarkeningColor = {read=FDarkeningColor, write=SetDarkeningColor};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLShadowVolume(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glshadowvolume */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADOWVOLUME)
using namespace Glshadowvolume;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadowvolumeHPP
