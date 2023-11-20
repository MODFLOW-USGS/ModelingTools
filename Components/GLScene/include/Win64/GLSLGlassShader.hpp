// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLGlassShader.pas' rev: 34.00 (Windows)

#ifndef GlslglassshaderHPP
#define GlslglassshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLScene.hpp>
#include <GLBaseClasses.hpp>
#include <GLState.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorGeometry.hpp>
#include <GLCoordinates.hpp>
#include <GLTextureFormat.hpp>
#include <GLColor.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLPersistentClasses.hpp>
#include <GLGraphics.hpp>
#include <GLSLShader.hpp>
#include <GLCustomShader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslglassshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLGlassShader;
class DELPHICLASS TGLSLGlassShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLGlassShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glcolor::TGLColor* FDiffuseColor;
	float FDepth;
	float FMix;
	float FAlpha;
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTexName;
	Gltexture::TGLTexture* FRefractionTexture;
	System::UnicodeString FRefractionTexName;
	Glscene::TGLBaseSceneObject* FOwnerObject;
	Glstate::TBlendFunction FBlendSrc;
	Glstate::TBlendFunction FBlendDst;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetRefractionTexTexture(Gltexture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetRefractionTexName();
	void __fastcall SetRefractionTexName(const System::UnicodeString Value);
	void __fastcall SetDiffuseColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLGlassShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLGlassShader();
	__property Glcolor::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property float Depth = {read=FDepth, write=FDepth};
	__property float Mix = {read=FMix, write=FMix};
	__property float Alpha = {read=FAlpha, write=FAlpha};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gltexture::TGLTexture* RefractionTexture = {read=FRefractionTexture, write=SetRefractionTexTexture};
	__property System::UnicodeString RefractionTextureName = {read=GetRefractionTexName, write=SetRefractionTexName};
	__property Glscene::TGLBaseSceneObject* OwnerObject = {read=FOwnerObject, write=FOwnerObject};
	__property Glstate::TBlendFunction BlendSrc = {read=FBlendSrc, write=FBlendSrc, default=6};
	__property Glstate::TBlendFunction BlendDst = {read=FBlendDst, write=FBlendDst, default=8};
};


class PASCALIMPLEMENTATION TGLSLGlassShader : public TGLCustomGLSLGlassShader
{
	typedef TGLCustomGLSLGlassShader inherited;
	
__published:
	__property DiffuseColor;
	__property Depth = {default=0};
	__property Mix = {default=0};
	__property Alpha = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property RefractionTexture;
	__property RefractionTextureName = {default=0};
	__property OwnerObject;
	__property BlendSrc = {default=6};
	__property BlendDst = {default=8};
public:
	/* TGLCustomGLSLGlassShader.Create */ inline __fastcall virtual TGLSLGlassShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLGlassShader(AOwner) { }
	/* TGLCustomGLSLGlassShader.Destroy */ inline __fastcall virtual ~TGLSLGlassShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslglassshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLGLASSSHADER)
using namespace Glslglassshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslglassshaderHPP
