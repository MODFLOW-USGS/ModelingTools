// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLGoochShader.pas' rev: 36.00 (Windows)

#ifndef GlslgoochshaderHPP
#define GlslgoochshaderHPP

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

namespace Glslgoochshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLSimpleGoochShader;
class DELPHICLASS TGLSLSimpleGoochShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLSimpleGoochShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glcolor::TGLColor* FDiffuseColor;
	Glcolor::TGLColor* FWarmColor;
	Glcolor::TGLColor* FCoolColor;
	Glcolor::TGLColor* FSpecularColor;
	Glcolor::TGLColor* FAmbientColor;
	float FDiffuseWarm;
	float FDiffuseCool;
	float FAmbientFactor;
	float FDiffuseFactor;
	float FSpecularFactor;
	Glcustomshader::TGLBlendingModeEx FBlendingMode;
	void __fastcall SetDiffuseColor(Glcolor::TGLColor* AValue);
	void __fastcall SetAmbientColor(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecularColor(Glcolor::TGLColor* AValue);
	void __fastcall SetWarmColor(Glcolor::TGLColor* AValue);
	void __fastcall SetCoolColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleGoochShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleGoochShader();
	__property Glcolor::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property Glcolor::TGLColor* WarmColor = {read=FWarmColor, write=SetWarmColor};
	__property Glcolor::TGLColor* CoolColor = {read=FCoolColor, write=SetCoolColor};
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float WarmFactor = {read=FDiffuseWarm, write=FDiffuseWarm};
	__property float CoolFactor = {read=FDiffuseCool, write=FDiffuseCool};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Glcustomshader::TGLBlendingModeEx BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=0};
};


class PASCALIMPLEMENTATION TGLSLSimpleGoochShader : public TGLCustomGLSLSimpleGoochShader
{
	typedef TGLCustomGLSLSimpleGoochShader inherited;
	
__published:
	__property DiffuseColor;
	__property WarmColor;
	__property CoolColor;
	__property SpecularColor;
	__property AmbientColor;
	__property WarmFactor = {default=0};
	__property CoolFactor = {default=0};
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
public:
	/* TGLCustomGLSLSimpleGoochShader.Create */ inline __fastcall virtual TGLSLSimpleGoochShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleGoochShader(AOwner) { }
	/* TGLCustomGLSLSimpleGoochShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleGoochShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslgoochshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLGOOCHSHADER)
using namespace Glslgoochshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslgoochshaderHPP
