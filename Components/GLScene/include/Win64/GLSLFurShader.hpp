﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLFurShader.pas' rev: 35.00 (Windows)

#ifndef GlslfurshaderHPP
#define GlslfurshaderHPP

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

namespace Glslfurshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLFurShader;
class DELPHICLASS TGLSLFurShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLFurShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	int FCurrentPass;
	float FPassCount;
	float FFurLength;
	float FMaxFurLength;
	float FFurScale;
	bool FRandomFurLength;
	Glcolor::TGLColor* FColorScale;
	Glcolor::TGLColor* FAmbient;
	Glcoordinates::TGLCoordinates3* FGravity;
	float FLightIntensity;
	Gltexture::TGLTexture* FMainTex;
	Gltexture::TGLTexture* FNoiseTex;
	System::UnicodeString FNoiseTexName;
	System::UnicodeString FMainTexName;
	Glstate::TBlendFunction FBlendSrc;
	Glstate::TBlendFunction FBlendDst;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetNoiseTexTexture(Gltexture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetNoiseTexName();
	void __fastcall SetNoiseTexName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const System::UnicodeString Value);
	void __fastcall SetGravity(Glcoordinates::TGLCoordinates3* APosition);
	void __fastcall SetAmbient(Glcolor::TGLColor* AValue);
	void __fastcall SetColorScale(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLFurShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLFurShader();
	__property float PassCount = {read=FPassCount, write=FPassCount};
	__property float FurLength = {read=FFurLength, write=FFurLength};
	__property float MaxFurLength = {read=FMaxFurLength, write=FMaxFurLength};
	__property float FurDensity = {read=FFurScale, write=FFurScale};
	__property bool RandomFurLength = {read=FRandomFurLength, write=FRandomFurLength, nodefault};
	__property Glcolor::TGLColor* ColorScale = {read=FColorScale, write=SetColorScale};
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTex, write=SetMainTexTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property Gltexture::TGLTexture* NoiseTexture = {read=FNoiseTex, write=SetNoiseTexTexture};
	__property System::UnicodeString NoiseTextureName = {read=GetNoiseTexName, write=SetNoiseTexName};
	__property Glstate::TBlendFunction BlendSrc = {read=FBlendSrc, write=FBlendSrc, default=2};
	__property Glstate::TBlendFunction BlendDst = {read=FBlendDst, write=FBlendDst, default=5};
	__property Glcoordinates::TGLCoordinates3* Gravity = {read=FGravity, write=SetGravity};
	__property float LightIntensity = {read=FLightIntensity, write=FLightIntensity};
};


class PASCALIMPLEMENTATION TGLSLFurShader : public TGLCustomGLSLFurShader
{
	typedef TGLCustomGLSLFurShader inherited;
	
__published:
	__property PassCount = {default=0};
	__property FurLength = {default=0};
	__property MaxFurLength = {default=0};
	__property FurDensity = {default=0};
	__property RandomFurLength;
	__property ColorScale;
	__property Ambient;
	__property LightIntensity = {default=0};
	__property Gravity;
	__property BlendSrc = {default=2};
	__property BlendDst = {default=5};
	__property MainTexture;
	__property MainTextureName = {default=0};
	__property NoiseTexture;
	__property NoiseTextureName = {default=0};
public:
	/* TGLCustomGLSLFurShader.Create */ inline __fastcall virtual TGLSLFurShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLFurShader(AOwner) { }
	/* TGLCustomGLSLFurShader.Destroy */ inline __fastcall virtual ~TGLSLFurShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslfurshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLFURSHADER)
using namespace Glslfurshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslfurshaderHPP