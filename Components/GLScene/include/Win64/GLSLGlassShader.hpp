// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glslglassshader.pas' rev: 36.00 (Windows)

#ifndef GlslglassshaderHPP
#define GlslglassshaderHPP

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
#include <Glscene.hpp>
#include <Glbaseclasses.hpp>
#include <Glstate.hpp>
#include <Opengltokens.hpp>
#include <Glcontext.hpp>
#include <Glrendercontextinfo.hpp>
#include <Glvectorgeometry.hpp>
#include <Glcoordinates.hpp>
#include <Gltextureformat.hpp>
#include <Glcolor.hpp>
#include <Gltexture.hpp>
#include <Glmaterial.hpp>
#include <Glpersistentclasses.hpp>
#include <Glgraphics.hpp>
#include <Glslshader.hpp>
#include <Glcustomshader.hpp>

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
	Glmaterial::TGLLibMaterialName FMainTexName;
	Gltexture::TGLTexture* FRefractionTexture;
	Glmaterial::TGLLibMaterialName FRefractionTexName;
	Glscene::TGLBaseSceneObject* FOwnerObject;
	Glstate::TBlendFunction FBlendSrc;
	Glstate::TBlendFunction FBlendDst;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLLibMaterialName __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetRefractionTexTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLLibMaterialName __fastcall GetRefractionTexName();
	void __fastcall SetRefractionTexName(const Glmaterial::TGLLibMaterialName Value);
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
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gltexture::TGLTexture* RefractionTexture = {read=FRefractionTexture, write=SetRefractionTexTexture};
	__property Glmaterial::TGLLibMaterialName RefractionTextureName = {read=GetRefractionTexName, write=SetRefractionTexName};
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
