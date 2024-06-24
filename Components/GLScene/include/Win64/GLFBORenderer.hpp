// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfborenderer.pas' rev: 36.00 (Windows)

#ifndef GlfborendererHPP
#define GlfborendererHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Opengltokens.hpp>
#include <Glvectorgeometry.hpp>
#include <Glpersistentclasses.hpp>
#include <Glpipelinetransformation.hpp>
#include <Glscene.hpp>
#include <Gltexture.hpp>
#include <Glcontext.hpp>
#include <Glcolor.hpp>
#include <Glmaterial.hpp>
#include <Glrendercontextinfo.hpp>
#include <Glstate.hpp>
#include <Gltextureformat.hpp>
#include <Glvectortypes.hpp>
#include <Glmultisampleimage.hpp>
#include <Glfbo.hpp>
#include <Glslog.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfborenderer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFBORenderer;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLEnabledRenderBuffer : unsigned char { erbDepth, erbStencil };

typedef System::Set<TGLEnabledRenderBuffer, TGLEnabledRenderBuffer::erbDepth, TGLEnabledRenderBuffer::erbStencil> TGLEnabledRenderBuffers;

enum DECLSPEC_DENUM TGLFBOTargetVisibility : unsigned char { tvDefault, tvFBOOnly };

enum DECLSPEC_DENUM TGLFBOClearOption : unsigned char { coColorBufferClear, coDepthBufferClear, coStencilBufferClear, coUseBufferBackground };

typedef System::Set<TGLFBOClearOption, TGLFBOClearOption::coColorBufferClear, TGLFBOClearOption::coUseBufferBackground> TGLFBOClearOptions;

typedef System::DynamicArray<Gltexture::TGLTexture*> TGLTextureArray;

typedef void __fastcall (__closure *TSetTextureTargetsEvent)(System::TObject* Sender, TGLTextureArray &colorTexs);

class PASCALIMPLEMENTATION TGLFBORenderer : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glfbo::TGLFrameBuffer* FFbo;
	Glfbo::TGLDepthRBO* FDepthRBO;
	Glfbo::TGLStencilRBO* FStencilRBO;
	int FColorAttachment;
	bool FRendering;
	bool FHasColor;
	bool FHasDepth;
	bool FHasStencil;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Glmaterial::TGLLibMaterialName FColorTextureName;
	Glmaterial::TGLLibMaterialName FDepthTextureName;
	int FWidth;
	int FHeight;
	bool FForceTextureDimensions;
	Glfbo::TGLStencilPrecision FStencilPrecision;
	Glscene::TGLBaseSceneObject* FRootObject;
	bool FRootVisible;
	Glscene::TGLCamera* FCamera;
	TGLEnabledRenderBuffers FEnabledRenderBuffers;
	TGLFBOTargetVisibility FTargetVisibility;
	Glscene::TGLDirectRenderEvent FBeforeRender;
	System::Classes::TNotifyEvent FPostInitialize;
	Glscene::TGLDirectRenderEvent FAfterRender;
	System::Classes::TNotifyEvent FPreInitialize;
	Glcolor::TGLColor* FBackgroundColor;
	TGLFBOClearOptions FClearOptions;
	float FAspect;
	float FSceneScaleFactor;
	bool FUseLibraryAsMultiTarget;
	bool FPostGenerateMipmap;
	int FMaxSize;
	int FMaxAttachment;
	System::StaticArray<Glvectortypes::TVector4f, 3> FStoreCamera;
	TSetTextureTargetsEvent FOnSetTextureTargets;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	void __fastcall SetDepthTextureName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetColorTextureName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetForceTextureDimentions(const bool Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetWidth(int Value);
	void __fastcall SetLayer(const int Value);
	int __fastcall GetLayer();
	void __fastcall SetLevel(const int Value);
	int __fastcall GetLevel();
	void __fastcall SetStencilPrecision(const Glfbo::TGLStencilPrecision Value);
	void __fastcall SetRootObject(Glscene::TGLBaseSceneObject* const Value);
	Glvectorgeometry::TRectangle __fastcall GetViewport();
	void __fastcall SetCamera(Glscene::TGLCamera* const Value);
	void __fastcall SetEnabledRenderBuffers(const TGLEnabledRenderBuffers Value);
	void __fastcall SetTargetVisibility(const TGLFBOTargetVisibility Value);
	void __fastcall SetBackgroundColor(Glcolor::TGLColor* const Value);
	bool __fastcall StoreSceneScaleFactor();
	bool __fastcall StoreAspect();
	void __fastcall SetUseLibraryAsMultiTarget(bool Value);
	void __fastcall SetPostGenerateMipmap(const bool Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall Initialize();
	void __fastcall ForceDimensions(Gltexture::TGLTexture* Texture);
	void __fastcall RenderToFBO(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall ApplyCamera(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApplyCamera(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoBeforeRender(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoAfterRender(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoPreInitialize();
	void __fastcall DoPostInitialize();
	__property bool HasColor = {read=FHasColor, nodefault};
	__property bool HasDepth = {read=FHasDepth, nodefault};
	__property bool HasStencil = {read=FHasStencil, nodefault};
	__property Glvectorgeometry::TRectangle Viewport = {read=GetViewport};
	
public:
	__fastcall virtual TGLFBORenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFBORenderer();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property int Layer = {read=GetLayer, write=SetLayer, nodefault};
	__property int Level = {read=GetLevel, write=SetLevel, nodefault};
	
__published:
	__property bool Active = {read=GetVisible, write=SetVisible, default=1};
	__property bool PickableTarget = {read=GetPickable, write=SetPickable, default=0};
	__property bool ForceTextureDimensions = {read=FForceTextureDimensions, write=SetForceTextureDimentions, default=1};
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=256};
	__property float Aspect = {read=FAspect, write=FAspect, stored=StoreAspect};
	__property Glmaterial::TGLLibMaterialName ColorTextureName = {read=FColorTextureName, write=SetColorTextureName};
	__property Glmaterial::TGLLibMaterialName DepthTextureName = {read=FDepthTextureName, write=SetDepthTextureName};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Glcolor::TGLColor* BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor};
	__property TGLFBOClearOptions ClearOptions = {read=FClearOptions, write=FClearOptions, nodefault};
	__property Glscene::TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property float SceneScaleFactor = {read=FSceneScaleFactor, write=FSceneScaleFactor, stored=StoreSceneScaleFactor};
	__property Glscene::TGLBaseSceneObject* RootObject = {read=FRootObject, write=SetRootObject};
	__property TGLFBOTargetVisibility TargetVisibility = {read=FTargetVisibility, write=SetTargetVisibility, default=0};
	__property TGLEnabledRenderBuffers EnabledRenderBuffers = {read=FEnabledRenderBuffers, write=SetEnabledRenderBuffers, nodefault};
	__property Glfbo::TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, default=0};
	__property Glscene::TGLDirectRenderEvent BeforeRender = {read=FBeforeRender, write=FBeforeRender};
	__property Glscene::TGLDirectRenderEvent AfterRender = {read=FAfterRender, write=FAfterRender};
	__property System::Classes::TNotifyEvent PreInitialize = {read=FPreInitialize, write=FPreInitialize};
	__property System::Classes::TNotifyEvent PostInitialize = {read=FPostInitialize, write=FPostInitialize};
	__property bool UseLibraryAsMultiTarget = {read=FUseLibraryAsMultiTarget, write=SetUseLibraryAsMultiTarget, default=0};
	__property bool PostGenerateMipmap = {read=FPostGenerateMipmap, write=SetPostGenerateMipmap, default=1};
	__property TSetTextureTargetsEvent OnSetTextureTargets = {read=FOnSetTextureTargets, write=FOnSetTextureTargets};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFBORenderer(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
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


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfborenderer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFBORENDERER)
using namespace Glfborenderer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfborendererHPP
