// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLErosionShader.pas' rev: 36.00 (Windows)

#ifndef GLSLErosionShaderHPP
#define GLSLErosionShaderHPP

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
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLBaseClasses.hpp>
#include <GLState.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLCoordinates.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLTextureFormat.hpp>
#include <GLColor.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLSLShader.hpp>
#include <GLCustomShader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslerosionshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLSimpleErosionShader;
class DELPHICLASS TGLSLSimpleErosionShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLSimpleErosionShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTex;
	Gltexture::TGLTexture* FNoiseTex;
	Gltexture::TGLTexture* FErosionTex;
	Glmaterial::TGLLibMaterialName FMainTexName;
	Glmaterial::TGLLibMaterialName FNoiseTexName;
	Glmaterial::TGLLibMaterialName FErosionTexName;
	float FErosionScale;
	float FErosionFactor;
	float FIntensityFactor1;
	float FIntensityFactor2;
	Glcolor::TGLColor* FSpecularColor;
	Glcolor::TGLColor* FAmbientColor;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	float FSpecularRoughness;
	float FAnisotropicRoughness;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetNoiseTexTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetErosionTexTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLLibMaterialName __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const Glmaterial::TGLLibMaterialName Value);
	Glmaterial::TGLLibMaterialName __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const Glmaterial::TGLLibMaterialName Value);
	Glmaterial::TGLLibMaterialName __fastcall GetErosionTexName();
	void __fastcall SetErosionTexName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetAmbientColor(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecularColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleErosionShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleErosionShader();
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTex, write=SetMainTexTexture};
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gltexture::TGLTexture* NoiseTexture = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property Glmaterial::TGLLibMaterialName NoiseTextureName = {read=GetNoiseTexName, write=SetNoiseTexName};
	__property Gltexture::TGLTexture* ErosionTexture = {read=FErosionTex, write=SetErosionTexTexture};
	__property Glmaterial::TGLLibMaterialName ErosionTextureName = {read=GetErosionTexName, write=SetErosionTexName};
	__property float ErosionFactor = {read=FErosionFactor, write=FErosionFactor};
	__property float ErosionScale = {read=FErosionFactor, write=FErosionFactor};
	__property float IntensityFactor1 = {read=FIntensityFactor1, write=FIntensityFactor1};
	__property float IntensityFactor2 = {read=FIntensityFactor2, write=FIntensityFactor2};
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property float SpecularRoughness = {read=FSpecularRoughness, write=FSpecularRoughness};
	__property float AnisotropicRoughness = {read=FAnisotropicRoughness, write=FAnisotropicRoughness};
};


class PASCALIMPLEMENTATION TGLSLSimpleErosionShader : public TGLCustomGLSLSimpleErosionShader
{
	typedef TGLCustomGLSLSimpleErosionShader inherited;
	
__published:
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property NoiseTexture;
	__property NoiseTextureName = {default=0};
	__property ErosionTexture;
	__property ErosionTextureName = {default=0};
	__property ErosionScale = {default=0};
	__property ErosionFactor = {default=0};
	__property IntensityFactor1 = {default=0};
	__property IntensityFactor2 = {default=0};
	__property SpecularColor;
	__property AmbientColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property SpecularRoughness = {default=0};
	__property AnisotropicRoughness = {default=0};
public:
	/* TGLCustomGLSLSimpleErosionShader.Create */ inline __fastcall virtual TGLSLSimpleErosionShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleErosionShader(AOwner) { }
	/* TGLCustomGLSLSimpleErosionShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleErosionShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslerosionshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLEROSIONSHADER)
using namespace Glslerosionshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLSLErosionShaderHPP
