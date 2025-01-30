// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLLatticeShader.pas' rev: 36.00 (Windows)

#ifndef GLSLLatticeShaderHPP
#define GLSLLatticeShaderHPP

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

namespace Glsllatticeshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLSimpleLatticeShader;
class DELPHICLASS TGLCustomGLSLLatticeShader;
class DELPHICLASS TGLSLSimpleLatticeShader;
class DELPHICLASS TGLSLLatticeShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLSimpleLatticeShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glcoordinates::TGLCoordinates2* FLatticeScale;
	Glcoordinates::TGLCoordinates2* FLatticeThreshold;
	void __fastcall SetLatticeScale(Glcoordinates::TGLCoordinates2* const Value);
	void __fastcall SetLatticeThreshold(Glcoordinates::TGLCoordinates2* const Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLSimpleLatticeShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLSimpleLatticeShader();
	__property Glcoordinates::TGLCoordinates2* LatticeScale = {read=FLatticeScale, write=SetLatticeScale};
	__property Glcoordinates::TGLCoordinates2* LatticeThreshold = {read=FLatticeThreshold, write=SetLatticeThreshold};
};


class PASCALIMPLEMENTATION TGLCustomGLSLLatticeShader : public TGLCustomGLSLSimpleLatticeShader
{
	typedef TGLCustomGLSLSimpleLatticeShader inherited;
	
private:
	Glcolor::TGLColor* FAmbientColor;
	Glcolor::TGLColor* FDiffuseColor;
	Glcolor::TGLColor* FSpecularColor;
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTexture;
	Glmaterial::TGLLibMaterialName FMainTexName;
	float FSpecularPower;
	float FLightPower;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMainTexTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLLibMaterialName __fastcall GetMainTexName();
	void __fastcall SetMainTexName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetDiffuseColor(Glcolor::TGLColor* AValue);
	void __fastcall SetAmbientColor(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecularColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomGLSLLatticeShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLLatticeShader();
	__property Glcolor::TGLColor* DiffuseColor = {read=FDiffuseColor, write=SetDiffuseColor};
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor, write=SetSpecularColor};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexTexture};
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTexName, write=SetMainTexName};
	__property float SpecularPower = {read=FSpecularPower, write=FSpecularPower};
	__property float LightPower = {read=FLightPower, write=FLightPower};
};


class PASCALIMPLEMENTATION TGLSLSimpleLatticeShader : public TGLCustomGLSLSimpleLatticeShader
{
	typedef TGLCustomGLSLSimpleLatticeShader inherited;
	
__published:
	__property LatticeScale;
	__property LatticeThreshold;
public:
	/* TGLCustomGLSLSimpleLatticeShader.Create */ inline __fastcall virtual TGLSLSimpleLatticeShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLSimpleLatticeShader(AOwner) { }
	/* TGLCustomGLSLSimpleLatticeShader.Destroy */ inline __fastcall virtual ~TGLSLSimpleLatticeShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLLatticeShader : public TGLCustomGLSLLatticeShader
{
	typedef TGLCustomGLSLLatticeShader inherited;
	
__published:
	__property LatticeScale;
	__property LatticeThreshold;
	__property AmbientColor;
	__property DiffuseColor;
	__property SpecularColor;
	__property MainTexture;
	__property SpecularPower = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLCustomGLSLLatticeShader.Create */ inline __fastcall virtual TGLSLLatticeShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLLatticeShader(AOwner) { }
	/* TGLCustomGLSLLatticeShader.Destroy */ inline __fastcall virtual ~TGLSLLatticeShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsllatticeshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLLATTICESHADER)
using namespace Glsllatticeshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLSLLatticeShaderHPP
