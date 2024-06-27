// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLDiffuseSpecularShader.pas' rev: 36.00 (Windows)

#ifndef GlsldiffusespecularshaderHPP
#define GlsldiffusespecularshaderHPP

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
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLTexture.hpp>
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLStrings.hpp>
#include <GLCustomShader.hpp>
#include <GLSLShader.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsldiffusespecularshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLSLDiffuseSpecularShaderException;
class DELPHICLASS TGLBaseCustomGLSLDiffuseSpecular;
class DELPHICLASS TGLBaseGLSLDiffuseSpecularShaderMT;
class DELPHICLASS TGLCustomGLSLDiffuseSpecularShader;
class DELPHICLASS TGLCustomGLSLDiffuseSpecularShaderMT;
struct TLightRecord;
class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShader;
class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLDiffuseSpecularShader;
class DELPHICLASS TGLSLMLDiffuseSpecularShaderMT;
class DELPHICLASS TGLSLMLDiffuseSpecularShader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLDiffuseSpecularShaderException : public Glslshader::EGLSLShaderException
{
	typedef Glslshader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg) : Glslshader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : Glslshader::EGLSLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::NativeUInt Ident)/* overload */ : Glslshader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::NativeUInt Ident, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLDiffuseSpecularShaderException() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLBaseCustomGLSLDiffuseSpecular : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	float FLightPower;
	bool FRealisticSpecular;
	Glcustomshader::TGLShaderFogSupport FFogSupport;
	void __fastcall SetRealisticSpecular(const bool Value);
	void __fastcall SetFogSupport(const Glcustomshader::TGLShaderFogSupport Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLBaseCustomGLSLDiffuseSpecular(System::Classes::TComponent* AOwner);
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property bool RealisticSpecular = {read=FRealisticSpecular, write=SetRealisticSpecular, nodefault};
	__property Glcustomshader::TGLShaderFogSupport FogSupport = {read=FFogSupport, write=SetFogSupport, default=2};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLDiffuseSpecular() { }
	
};


class PASCALIMPLEMENTATION TGLBaseGLSLDiffuseSpecularShaderMT : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTexture;
	Glmaterial::TGLLibMaterialName FMainTextureName;
	Glmaterial::TGLLibMaterialName __fastcall GetMainTextureName();
	void __fastcall SetMainTextureName(const Glmaterial::TGLLibMaterialName Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property Glmaterial::TGLLibMaterialName MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLBaseGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseGLSLDiffuseSpecularShaderMT() { }
	
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


class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
protected:
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
protected:
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShaderMT() { }
	
};


struct DECLSPEC_DRECORD TLightRecord
{
public:
	bool Enabled;
	Glscene::TGLLightStyle Style;
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShaderMT : public TGLCustomGLSLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShader : public TGLCustomGLSLDiffuseSpecularShader
{
	typedef TGLCustomGLSLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShader() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShaderMT : public TGLCustomGLSLMLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLMLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShaderMT.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShaderMT() { }
	
};


class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShader : public TGLCustomGLSLMLDiffuseSpecularShader
{
	typedef TGLCustomGLSLMLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShader.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsldiffusespecularshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLDIFFUSESPECULARSHADER)
using namespace Glsldiffusespecularshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsldiffusespecularshaderHPP
