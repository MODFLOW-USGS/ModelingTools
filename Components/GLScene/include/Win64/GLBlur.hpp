// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBlur.pas' rev: 35.00 (Windows)

#ifndef GlblurHPP
#define GlblurHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLObjects.hpp>
#include <GLBitmapFont.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLHUDObjects.hpp>
#include <GLColor.hpp>
#include <GLGraphics.hpp>
#include <GLContext.hpp>
#include <XOpenGL.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glblur
{
//-- forward type declarations -----------------------------------------------
struct TRGBPixel;
class DELPHICLASS EGLMotionBlurException;
class DELPHICLASS TGLBlur;
class DELPHICLASS TGLMotionBlur;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLBlurPreset : unsigned char { pNone, pGlossy, pBeastView, pOceanDepth, pDream, pOverBlur, pAdvancedBlur };

enum DECLSPEC_DENUM TGLBlurkind : unsigned char { bNone, bSimple, bAdvanced };

struct DECLSPEC_DRECORD TRGBPixel
{
public:
	System::Byte R;
	System::Byte G;
	System::Byte B;
};


typedef System::DynamicArray<TRGBPixel> TRGBPixelBuffer;

typedef void __fastcall (__closure *TGLAdvancedBlurImagePrepareEvent)(System::TObject* Sender, Glgraphics::TGLImage* BMP32, bool &DoBlur);

class PASCALIMPLEMENTATION EGLMotionBlurException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLMotionBlurException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLMotionBlurException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLMotionBlurException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLMotionBlurException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLMotionBlurException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLMotionBlurException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLMotionBlurException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLMotionBlurException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLMotionBlurException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLMotionBlurException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLMotionBlurException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLMotionBlurException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLMotionBlurException() { }
	
};


class PASCALIMPLEMENTATION TGLBlur : public Glhudobjects::TGLHUDSprite
{
	typedef Glhudobjects::TGLHUDSprite inherited;
	
private:
	Glscene::TGLMemoryViewer* FViewer;
	double OldTime;
	bool FDoingMemView;
	double FBlurDeltaTime;
	float FBlurTop;
	float FBlurBottom;
	float FBlurLeft;
	float FBlurRight;
	int FRenderHeight;
	int FRenderWidth;
	TGLBlurPreset FPreset;
	Glscene::TGLBaseSceneObject* FTargetObject;
	TGLAdvancedBlurImagePrepareEvent FOnAdvancedBlurImagePrepareEvent;
	TGLBlurkind FBlur;
	TRGBPixelBuffer Pixelbuffer;
	int FAdvancedBlurPasses;
	System::Classes::TNotifyEvent FOnAfterTargetRender;
	System::Classes::TNotifyEvent FOnBeforeTargetRender;
	float FAdvancedBlurAmp;
	bool FBlurSelf;
	System::Byte FAdvancedBlurLowClamp;
	System::Byte FAdvancedBlurHiClamp;
	System::Uitypes::TColor FRenderBackgroundColor;
	void __fastcall DoMemView(Glscene::TGLBaseSceneObject* baseObject);
	void __fastcall SetRenderHeight(const int Value);
	void __fastcall SetRenderWidth(const int Value);
	void __fastcall UpdateImageSettings();
	void __fastcall SetPreset(const TGLBlurPreset Value);
	bool __fastcall StoreBlurBottom();
	bool __fastcall StoreBlurDeltaTime();
	bool __fastcall StoreBlurRight();
	bool __fastcall StoreBlurTop();
	bool __fastcall StoreBlurLeft();
	void __fastcall SetTargetObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetOnAdvancedBlurImagePrepareEvent(const TGLAdvancedBlurImagePrepareEvent Value);
	void __fastcall SetBlur(const TGLBlurkind Value);
	void __fastcall SetAdvancedBlurPasses(const int Value);
	void __fastcall SetOnAfterTargetRender(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnBeforeTargetRender(const System::Classes::TNotifyEvent Value);
	void __fastcall SetAdvancedBlurAmp(const float Value);
	void __fastcall SetBlurSelf(const bool Value);
	void __fastcall SetAdvancedBlurLowClamp(const System::Byte Value);
	void __fastcall SetAdvancedBlurHiClamp(const System::Byte Value);
	void __fastcall SetRenderBackgroundColor(const System::Uitypes::TColor Value);
	
public:
	__fastcall virtual TGLBlur(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBlur();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
__published:
	__property TGLBlurkind Blur = {read=FBlur, write=SetBlur, nodefault};
	__property double BlurDeltaTime = {read=FBlurDeltaTime, write=FBlurDeltaTime, stored=StoreBlurDeltaTime};
	__property float BlurLeft = {read=FBlurLeft, write=FBlurLeft, stored=StoreBlurLeft};
	__property float BlurTop = {read=FBlurTop, write=FBlurTop, stored=StoreBlurTop};
	__property float BlurRight = {read=FBlurRight, write=FBlurRight, stored=StoreBlurRight};
	__property float BlurBottom = {read=FBlurBottom, write=FBlurBottom, stored=StoreBlurBottom};
	__property int RenderWidth = {read=FRenderWidth, write=SetRenderWidth, default=256};
	__property int RenderHeight = {read=FRenderHeight, write=SetRenderHeight, default=256};
	__property TGLBlurPreset Preset = {read=FPreset, write=SetPreset, stored=false, nodefault};
	__property Glscene::TGLBaseSceneObject* TargetObject = {read=FTargetObject, write=SetTargetObject};
	__property int AdvancedBlurPasses = {read=FAdvancedBlurPasses, write=SetAdvancedBlurPasses, nodefault};
	__property float AdvancedBlurAmp = {read=FAdvancedBlurAmp, write=SetAdvancedBlurAmp};
	__property System::Byte AdvancedBlurLowClamp = {read=FAdvancedBlurLowClamp, write=SetAdvancedBlurLowClamp, nodefault};
	__property System::Byte AdvancedBlurHiClamp = {read=FAdvancedBlurHiClamp, write=SetAdvancedBlurHiClamp, nodefault};
	__property bool BlurSelf = {read=FBlurSelf, write=SetBlurSelf, nodefault};
	__property System::Uitypes::TColor RenderBackgroundColor = {read=FRenderBackgroundColor, write=SetRenderBackgroundColor, nodefault};
	__property TGLAdvancedBlurImagePrepareEvent OnAdvancedBlurImagePrepareEvent = {read=FOnAdvancedBlurImagePrepareEvent, write=SetOnAdvancedBlurImagePrepareEvent};
	__property System::Classes::TNotifyEvent OnBeforeTargetRender = {read=FOnBeforeTargetRender, write=SetOnBeforeTargetRender};
	__property System::Classes::TNotifyEvent OnAfterTargetRender = {read=FOnAfterTargetRender, write=SetOnAfterTargetRender};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBlur(Glscene::TGLBaseSceneObject* aParentOwner) : Glhudobjects::TGLHUDSprite(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLMotionBlur : public Glscene::TGLCustomSceneObject
{
	typedef Glscene::TGLCustomSceneObject inherited;
	
private:
	float FIntensity;
	bool __fastcall StoreIntensity();
	
protected:
	virtual void __fastcall DoOnAddedToParent();
	virtual void __fastcall InitializeObject(System::TObject* ASender, const Glrendercontextinfo::TGLRenderContextInfo &ARci);
	
public:
	bool __fastcall SupportsRequiredExtensions();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__fastcall virtual TGLMotionBlur(System::Classes::TComponent* aOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float Intensity = {read=FIntensity, write=FIntensity, stored=StoreIntensity};
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
	__property Hint = {default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLMotionBlur() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMotionBlur(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCustomSceneObject(aParentOwner) { }
	
private:
	void *__IGLInitializable;	// Glscene::IGLInitializable 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {EA40AE8E-79B3-42F5-ADF1-7A901B665E12}
	operator Glscene::_di_IGLInitializable()
	{
		Glscene::_di_IGLInitializable intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glscene::IGLInitializable*(void) { return (Glscene::IGLInitializable*)&__IGLInitializable; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glblur */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBLUR)
using namespace Glblur;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlblurHPP
