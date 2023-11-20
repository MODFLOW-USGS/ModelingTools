// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLBumpShader.pas' rev: 35.00 (Windows)

#ifndef GlslbumpshaderHPP
#define GlslbumpshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLTexture.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLCadencer.hpp>
#include <GLStrings.hpp>
#include <OpenGLTokens.hpp>
#include <GLSLShader.hpp>
#include <GLCustomShader.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslbumpshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLSLBumpShaderException;
class DELPHICLASS TGLBaseCustomGLSLBumpShader;
class DELPHICLASS TGLBaseCustomGLSLBumpShaderMT;
class DELPHICLASS TGLCustomGLSLBumpShaderAM;
class DELPHICLASS TGLCustomGLSLBumpShaderMT;
class DELPHICLASS TGLCustomGLSLBumpShader;
class DELPHICLASS TGLCustomGLSLMLBumpShader;
class DELPHICLASS TGLCustomGLSLMLBumpShaderMT;
class DELPHICLASS TGLSLBumpShaderMT;
class DELPHICLASS TGLSLBumpShader;
class DELPHICLASS TGLSLBumpShaderAM;
class DELPHICLASS TGLSLMLBumpShader;
class DELPHICLASS TGLSLMLBumpShaderMT;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLBumpShaderException : public Glslshader::EGLSLShaderException
{
	typedef Glslshader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg) : Glslshader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Glslshader::EGLSLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident)/* overload */ : Glslshader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLBumpShaderException() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	float FBumpHeight;
	int FBumpSmoothness;
	float FSpecularPower;
	float FSpecularSpread;
	float FLightPower;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FNormalTexture;
	Gltexture::TGLTexture* FSpecularTexture;
	System::UnicodeString FNormalTextureName;
	System::UnicodeString FSpecularTextureName;
	System::UnicodeString __fastcall GetNormalTextureName();
	System::UnicodeString __fastcall GetSpecularTextureName();
	void __fastcall SetNormalTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetNormalTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLBaseCustomGLSLBumpShader(System::Classes::TComponent* AOwner);
	__property float BumpHeight = {read=FBumpHeight, write=FBumpHeight};
	__property int BumpSmoothness = {read=FBumpSmoothness, write=FBumpSmoothness, nodefault};
	__property float SpecularPower = {read=FSpecularPower, write=FSpecularPower};
	__property float SpecularSpread = {read=FSpecularSpread, write=FSpecularSpread};
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property Gltexture::TGLTexture* NormalTexture = {read=FNormalTexture, write=SetNormalTexture};
	__property Gltexture::TGLTexture* SpecularTexture = {read=FSpecularTexture, write=SetSpecularTexture};
	__property System::UnicodeString NormalTextureName = {read=GetNormalTextureName, write=SetNormalTextureName};
	__property System::UnicodeString SpecularTextureName = {read=GetSpecularTextureName, write=SetSpecularTextureName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShader() { }
	
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


class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Gltexture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString __fastcall GetMainTextureName();
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLBaseCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderAM : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Glcolor::TGLColor* FAmbientColor;
	Glcolor::TGLColor* FDiffuseColor;
	Glcolor::TGLColor* FSpecularColor;
	float __fastcall GetAlpha();
	void __fastcall SetAlpha(const float Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLBumpShaderAM(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLBumpShaderAM();
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor};
	__property Glcolor::TGLColor* DiffuseColor = {read=FDiffuseColor};
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor};
	__property float Alpha = {read=GetAlpha, write=SetAlpha};
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	void __fastcall SetShaderTextures(Gltexture::TGLTexture* const *Textures, const int Textures_High);
	void __fastcall GetShaderTextures(Gltexture::TGLTexture* *Textures, const int Textures_High);
	void __fastcall SetShaderColorParams(const Glvectortypes::TVector4f &AAmbientColor, const Glvectortypes::TVector4f &ADiffuseColor, const Glvectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Glvectortypes::TVector4f &AAmbientColor, Glvectortypes::TVector4f &ADiffuseColor, Glvectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Glcadencer::TGLCadencer* const ACadencer, Glmaterial::TGLMaterialLibrary* const AMatLib, const Glcustomshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Glcadencer::TGLCadencer* &ACadencer, Glmaterial::TGLMaterialLibrary* &AMatLib, Glcustomshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha();
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription();
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShader() { }
	
private:
	void *__IGLShaderDescription;	// Glcustomshader::IGLShaderDescription 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glcustomshader::_di_IGLShaderDescription()
	{
		Glcustomshader::_di_IGLShaderDescription intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glcustomshader::IGLShaderDescription*(void) { return (Glcustomshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Glcustomshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glcustomshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	void __fastcall SetShaderTextures(Gltexture::TGLTexture* const *Textures, const int Textures_High);
	void __fastcall GetShaderTextures(Gltexture::TGLTexture* *Textures, const int Textures_High);
	void __fastcall SetShaderColorParams(const Glvectortypes::TVector4f &AAmbientColor, const Glvectortypes::TVector4f &ADiffuseColor, const Glvectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Glvectortypes::TVector4f &AAmbientColor, Glvectortypes::TVector4f &ADiffuseColor, Glvectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Glcadencer::TGLCadencer* const ACadencer, Glmaterial::TGLMaterialLibrary* const AMatLib, const Glcustomshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Glcadencer::TGLCadencer* &ACadencer, Glmaterial::TGLMaterialLibrary* &AMatLib, Glcustomshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha();
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription();
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShader(System::Classes::TComponent* AOwner);
	__property Glcustomshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShader() { }
	
private:
	void *__IGLShaderDescription;	// Glcustomshader::IGLShaderDescription 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glcustomshader::_di_IGLShaderDescription()
	{
		Glcustomshader::_di_IGLShaderDescription intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glcustomshader::IGLShaderDescription*(void) { return (Glcustomshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Glcustomshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glcustomshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner);
	__property Glcustomshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShaderMT : public TGLCustomGLSLBumpShaderMT
{
	typedef TGLCustomGLSLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShader : public TGLCustomGLSLBumpShader
{
	typedef TGLCustomGLSLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLBumpShaderAM : public TGLCustomGLSLBumpShaderAM
{
	typedef TGLCustomGLSLBumpShaderAM inherited;
	
__published:
	__property AmbientColor;
	__property DiffuseColor;
	__property SpecularColor;
	__property Alpha = {stored=false, default=0};
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLCustomGLSLBumpShaderAM.Create */ inline __fastcall virtual TGLSLBumpShaderAM(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderAM(AOwner) { }
	/* TGLCustomGLSLBumpShaderAM.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderAM() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLBumpShader : public TGLCustomGLSLMLBumpShader
{
	typedef TGLCustomGLSLMLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShader.Create */ inline __fastcall virtual TGLSLMLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLBumpShaderMT : public TGLCustomGLSLMLBumpShaderMT
{
	typedef TGLCustomGLSLMLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShaderMT.Create */ inline __fastcall virtual TGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShaderMT() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslbumpshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLBUMPSHADER)
using namespace Glslbumpshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslbumpshaderHPP
