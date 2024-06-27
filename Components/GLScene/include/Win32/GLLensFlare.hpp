// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLLensFlare.pas' rev: 36.00 (Windows)

#ifndef GllensflareHPP
#define GllensflareHPP

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
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLVectorGeometry.hpp>
#include <GLObjects.hpp>
#include <GLContext.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLVectorTypes.hpp>
#include <GLUtils.hpp>
#include <GLTextureFormat.hpp>
#include <GLCoordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gllensflare
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFlareGradient;
class DELPHICLASS TGLLensFlare;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFlareElement : unsigned char { feGlow, feRing, feStreaks, feRays, feSecondaries };

typedef System::Set<TFlareElement, TFlareElement::feGlow, TFlareElement::feSecondaries> TFlareElements;

class PASCALIMPLEMENTATION TGLFlareGradient : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
private:
	Glcolor::TGLColor* FFromColor;
	Glcolor::TGLColor* FToColor;
	
protected:
	void __fastcall SetFromColor(Glcolor::TGLColor* const val);
	void __fastcall SetToColor(Glcolor::TGLColor* const val);
	
public:
	__fastcall virtual TGLFlareGradient(System::Classes::TPersistent* AOwner);
	__fastcall TGLFlareGradient(System::Classes::TPersistent* AOwner, const Glcolor::TColorVector &fromColor, const Glcolor::TColorVector &toColor);
	__fastcall virtual ~TGLFlareGradient();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glcolor::TGLColor* FromColor = {read=FFromColor, write=SetFromColor};
	__property Glcolor::TGLColor* ToColor = {read=FToColor, write=SetToColor};
};


class PASCALIMPLEMENTATION TGLLensFlare : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLLensFlare__1;
	
	typedef System::DynamicArray<float> _TGLLensFlare__2;
	
	
private:
	int FSize;
	float FDeltaTime;
	float FCurrSize;
	int FSeed;
	float FSqueeze;
	int FNumStreaks;
	float FStreakWidth;
	float FStreakAngle;
	int FNumSecs;
	int FResolution;
	bool FAutoZTest;
	TFlareElements FElements;
	_TGLLensFlare__1 FSin20Res;
	_TGLLensFlare__1 FCos20Res;
	_TGLLensFlare__2 FSinRes;
	_TGLLensFlare__2 FCosRes;
	Glcontext::TGLTextureHandle* FTexRays;
	bool FFlareIsNotOccluded;
	Glcontext::TGLOcclusionQueryHandle* FOcclusionQuery;
	TGLFlareGradient* FGlowGradient;
	TGLFlareGradient* FRingGradient;
	TGLFlareGradient* FStreaksGradient;
	TGLFlareGradient* FRaysGradient;
	TGLFlareGradient* FSecondariesGradient;
	bool FDynamic;
	Glscene::TGLRenderPoint* FPreRenderPoint;
	
protected:
	void __fastcall SetGlowGradient(TGLFlareGradient* const val);
	void __fastcall SetRingGradient(TGLFlareGradient* const val);
	void __fastcall SetStreaksGradient(TGLFlareGradient* const val);
	void __fastcall SetRaysGradient(TGLFlareGradient* const val);
	void __fastcall SetSecondariesGradient(TGLFlareGradient* const val);
	void __fastcall SetSize(int aValue);
	void __fastcall SetSeed(int aValue);
	void __fastcall SetSqueeze(float aValue);
	bool __fastcall StoreSqueeze();
	void __fastcall SetNumStreaks(int aValue);
	void __fastcall SetStreakWidth(float aValue);
	bool __fastcall StoreStreakWidth();
	void __fastcall SetStreakAngle(float aValue);
	void __fastcall SetNumSecs(int aValue);
	void __fastcall SetResolution(int aValue);
	void __fastcall SetAutoZTest(bool aValue);
	void __fastcall SetElements(TFlareElements aValue);
	void __fastcall SetDynamic(bool aValue);
	void __fastcall SetPreRenderPoint(Glscene::TGLRenderPoint* const val);
	void __fastcall PreRenderEvent(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall PreRenderPointFreed(System::TObject* Sender);
	void __fastcall SetupRenderingOptions(Glstate::TGLStateCache* StateCache);
	void __fastcall RenderRays(Glstate::TGLStateCache* StateCache, const float size);
	void __fastcall RenderStreaks(Glstate::TGLStateCache* StateCache);
	void __fastcall RenderRing();
	void __fastcall RenderSecondaries(const Glvectorgeometry::TAffineVector &posVector);
	
public:
	__fastcall virtual TGLLensFlare(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLensFlare();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	void __fastcall PreRender(Glscene::TGLSceneBuffer* activeBuffer);
	__property float FlareInstantaneousSize = {read=FCurrSize, write=FCurrSize};
	
__published:
	__property TGLFlareGradient* GlowGradient = {read=FGlowGradient, write=SetGlowGradient};
	__property TGLFlareGradient* RingGradient = {read=FRingGradient};
	__property TGLFlareGradient* StreaksGradient = {read=FStreaksGradient};
	__property TGLFlareGradient* RaysGradient = {read=FRaysGradient};
	__property TGLFlareGradient* SecondariesGradient = {read=FSecondariesGradient};
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property float Squeeze = {read=FSqueeze, write=SetSqueeze, stored=StoreSqueeze};
	__property int NumStreaks = {read=FNumStreaks, write=SetNumStreaks, default=4};
	__property float StreakWidth = {read=FStreakWidth, write=SetStreakWidth, stored=StoreStreakWidth};
	__property float StreakAngle = {read=FStreakAngle, write=SetStreakAngle};
	__property int NumSecs = {read=FNumSecs, write=SetNumSecs, default=8};
	__property int Resolution = {read=FResolution, write=SetResolution, default=64};
	__property bool AutoZTest = {read=FAutoZTest, write=SetAutoZTest, default=1};
	__property bool FlareIsNotOccluded = {read=FFlareIsNotOccluded, write=FFlareIsNotOccluded, nodefault};
	__property TFlareElements Elements = {read=FElements, write=SetElements, default=31};
	__property bool Dynamic = {read=FDynamic, write=FDynamic, default=1};
	__property Glscene::TGLRenderPoint* PreRenderPoint = {read=FPreRenderPoint, write=SetPreRenderPoint};
	__property ObjectsSorting = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLensFlare(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultFlareElements (System::Set<TFlareElement, TFlareElement::feGlow, TFlareElement::feSecondaries>() << TFlareElement::feGlow << TFlareElement::feRing << TFlareElement::feStreaks << TFlareElement::feRays << TFlareElement::feSecondaries )
}	/* namespace Gllensflare */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLLENSFLARE)
using namespace Gllensflare;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GllensflareHPP
