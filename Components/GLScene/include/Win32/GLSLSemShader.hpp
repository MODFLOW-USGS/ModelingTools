// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLSemShader.pas' rev: 36.00 (Windows)

#ifndef GLSLSemShaderHPP
#define GLSLSemShaderHPP

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

namespace Glslsemshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLSemShader;
class DELPHICLASS TGLSLSemShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLSemShader : public Glslshader::TGLCustomGLSLShader
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
	__fastcall virtual TGLCustomGLSLSemShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSemShader();
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property float AmbientFactor = {read=FAmbientFactor, write=FAmbientFactor};
	__property float DiffuseFactor = {read=FDiffuseFactor, write=FDiffuseFactor};
	__property float SpecularFactor = {read=FSpecularFactor, write=FSpecularFactor};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTexName, write=SetMainTexName};
};


class PASCALIMPLEMENTATION TGLSLSemShader : public TGLCustomGLSLSemShader
{
	typedef TGLCustomGLSLSemShader inherited;
	
__published:
	__property AmbientColor;
	__property SpecularColor;
	__property AmbientFactor = {default=0};
	__property DiffuseFactor = {default=0};
	__property SpecularFactor = {default=0};
	__property MaterialLibrary;
	__property MainTexture;
	__property MainTextureName = {default=0};
public:
	/* TGLCustomGLSLSemShader.Create */ inline __fastcall virtual TGLSLSemShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSemShader(AOwner) { }
	/* TGLCustomGLSLSemShader.Destroy */ inline __fastcall virtual ~TGLSLSemShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslsemshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLSEMSHADER)
using namespace Glslsemshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLSLSemShaderHPP
