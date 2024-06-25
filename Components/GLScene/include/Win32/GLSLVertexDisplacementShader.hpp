// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLVertexDisplacementShader.pas' rev: 36.00 (Windows)

#ifndef GlslvertexdisplacementshaderHPP
#define GlslvertexdisplacementshaderHPP

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
#include <GLVectorGeometry.hpp>
#include <GLCoordinates.hpp>
#include <GLTextureFormat.hpp>
#include <GLColor.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLSLShader.hpp>
#include <GLCustomShader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslvertexdisplacementshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLVertexDisplacementShader;
class DELPHICLASS TGLSLVertexDisplacementShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLVertexDisplacementShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glcolor::TGLColor* FAmbientColor;
	Glcolor::TGLColor* FSpecularColor;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTexture;
	Glmaterial::TGLLibMaterialName FMainTexName;
	float FElapsedTime;
	float FNoise;
	float FDisplacementScale;
	float FNoiseScale;
	float FTurbulenceFactor;
	float FNoisePeriod;
	float FTimeFactor;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLLibMaterialName __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetAmbientColor(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecularColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLVertexDisplacementShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLVertexDisplacementShader();
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property float ElapsedTime = {read=FElapsedTime, write=FElapsedTime};
	__property float NoiseFactor = {read=FNoise, write=FNoise};
	__property float NoiseScale = {read=FNoiseScale, write=FNoiseScale};
	__property float TurbulenceFactor = {read=FTurbulenceFactor, write=FTurbulenceFactor};
	__property float NoisePeriod = {read=FNoisePeriod, write=FNoisePeriod};
	__property float DisplacementScale = {read=FDisplacementScale, write=FDisplacementScale};
	__property float TimeFactor = {read=FTimeFactor, write=FTimeFactor};
};


class PASCALIMPLEMENTATION TGLSLVertexDisplacementShader : public TGLCustomGLSLVertexDisplacementShader
{
	typedef TGLCustomGLSLVertexDisplacementShader inherited;
	
__published:
	__property AmbientColor;
	__property SpecularColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property ElapsedTime = {default=0};
	__property NoiseFactor = {default=0};
	__property NoiseScale = {default=0};
	__property TurbulenceFactor = {default=0};
	__property NoisePeriod = {default=0};
	__property DisplacementScale = {default=0};
	__property TimeFactor = {default=0};
public:
	/* TGLCustomGLSLVertexDisplacementShader.Create */ inline __fastcall virtual TGLSLVertexDisplacementShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLVertexDisplacementShader(AOwner) { }
	/* TGLCustomGLSLVertexDisplacementShader.Destroy */ inline __fastcall virtual ~TGLSLVertexDisplacementShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslvertexdisplacementshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLVERTEXDISPLACEMENTSHADER)
using namespace Glslvertexdisplacementshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslvertexdisplacementshaderHPP
