// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLShader.pas' rev: 36.00 (Windows)

#ifndef GlslshaderHPP
#define GlslshaderHPP

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
#include <OpenGLAdapter.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLTexture.hpp>
#include <GLContext.hpp>
#include <GLCustomShader.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLTextureFormat.hpp>
#include <GLSLParameter.hpp>
#include <GLMaterial.hpp>
#include <GLState.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLSLShaderException;
struct TGLActiveAttrib;
class DELPHICLASS TGLCustomGLSLShader;
class DELPHICLASS TGLSLShaderParameter;
class DELPHICLASS TGLSLShader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLShaderException : public Glcustomshader::EGLCustomShaderException
{
	typedef Glcustomshader::EGLCustomShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg) : Glcustomshader::EGLCustomShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : Glcustomshader::EGLCustomShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLShaderException(System::NativeUInt Ident)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLShaderException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, int AHelpContext) : Glcustomshader::EGLCustomShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : Glcustomshader::EGLCustomShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLShaderException(System::NativeUInt Ident, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLShaderException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLShaderException() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TGLSLShaderEvent)(TGLCustomGLSLShader* Shader);

typedef void __fastcall (__closure *TGLSLShaderUnApplyEvent)(TGLCustomGLSLShader* Shader, bool &ThereAreMorePasses);

typedef void __fastcall (__closure *TGLSLShaderEventEx)(TGLCustomGLSLShader* Shader, System::TObject* Sender);

struct DECLSPEC_DRECORD TGLActiveAttrib
{
public:
	System::UnicodeString Name;
	int Size;
	Glslparameter::TGLSLDataType AType;
	int Location;
};


typedef System::DynamicArray<TGLActiveAttrib> TGLActiveAttribArray;

class PASCALIMPLEMENTATION TGLCustomGLSLShader : public Glcustomshader::TGLCustomShader
{
	typedef Glcustomshader::TGLCustomShader inherited;
	
private:
	Glcontext::TGLProgramHandle* FGLSLProg;
	TGLSLShaderParameter* FParam;
	System::Classes::TStrings* FActiveVarying;
	Glcustomshader::TGLTransformFeedBackMode FTransformFeedBackMode;
	TGLSLShaderEvent FOnInitialize;
	TGLSLShaderEvent FOnApply;
	TGLSLShaderUnApplyEvent FOnUnApply;
	TGLSLShaderEventEx FOnInitializeEx;
	TGLSLShaderEventEx FOnApplyEx;
	int FNextTexIndex;
	TGLSLShaderParameter* __fastcall GetParam(const System::UnicodeString Index);
	TGLSLShaderParameter* __fastcall GetDirectParam(const unsigned Index);
	void __fastcall OnChangeActiveVarying(System::TObject* Sender);
	
protected:
	__property TGLSLShaderEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TGLSLShaderUnApplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
	__property TGLSLShaderEvent OnInitialize = {read=FOnInitialize, write=FOnInitialize};
	__property TGLSLShaderEventEx OnInitializeEx = {read=FOnInitializeEx, write=FOnInitializeEx};
	__property TGLSLShaderEventEx OnApplyEx = {read=FOnApplyEx, write=FOnApplyEx};
	virtual Glcontext::TGLProgramHandle* __fastcall GetGLSLProg();
	virtual TGLSLShaderParameter* __fastcall GetCurrentParam();
	void __fastcall SetActiveVarying(System::Classes::TStrings* const Value);
	void __fastcall SetTransformFeedBackMode(const Glcustomshader::TGLTransformFeedBackMode Value);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoFinalize();
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLShader();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported();
	TGLActiveAttribArray __fastcall GetActiveAttribs();
	void __fastcall SetTex(const System::UnicodeString TexParamName, Gltexture::TGLTexture* Tex)/* overload */;
	void __fastcall SetTex(const System::UnicodeString TexParamName, Glmaterial::TGLLibMaterial* Mat)/* overload */;
	void __fastcall SetTex(TGLSLShaderParameter* TexParam, Gltexture::TGLTexture* Tex)/* overload */;
	void __fastcall SetTex(TGLSLShaderParameter* TexParam, Glmaterial::TGLLibMaterial* Mat)/* overload */;
	__property TGLSLShaderParameter* Param[const System::UnicodeString Index] = {read=GetParam};
	__property TGLSLShaderParameter* DirectParam[const unsigned Index] = {read=GetDirectParam};
	__property System::Classes::TStrings* ActiveVarying = {read=FActiveVarying, write=SetActiveVarying};
	__property Glcustomshader::TGLTransformFeedBackMode TransformFeedBackMode = {read=FTransformFeedBackMode, write=SetTransformFeedBackMode, default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLShaderParameter : public Glcustomshader::TGLCustomShaderParameter
{
	typedef Glcustomshader::TGLCustomShaderParameter inherited;
	
private:
	Glcontext::TGLProgramHandle* FGLSLProg;
	int FParameterID;
	
protected:
	virtual float __fastcall GetAsVector1f();
	virtual Glvectortypes::TVector2f __fastcall GetAsVector2f();
	virtual Glvectortypes::TVector3f __fastcall GetAsVector3f();
	virtual Glvectorgeometry::TVector __fastcall GetAsVector4f();
	virtual int __fastcall GetAsVector1i();
	virtual Glvectortypes::TVector2i __fastcall GetAsVector2i();
	virtual Glvectortypes::TVector3i __fastcall GetAsVector3i();
	virtual Glvectortypes::TVector4i __fastcall GetAsVector4i();
	virtual unsigned __fastcall GetAsVector1ui();
	virtual Glvectortypes::TVector2ui __fastcall GetAsVector2ui();
	virtual Glvectortypes::TVector3ui __fastcall GetAsVector3ui();
	virtual Glvectortypes::TVector4ui __fastcall GetAsVector4ui();
	virtual void __fastcall SetAsVector1f(const float Value);
	virtual void __fastcall SetAsVector2f(const Glvectortypes::TVector2f &Value);
	virtual void __fastcall SetAsVector3f(const Glvectortypes::TVector3f &Value);
	virtual void __fastcall SetAsVector4f(const Glvectortypes::TVector4f &Value);
	virtual void __fastcall SetAsVector1i(const int Value);
	virtual void __fastcall SetAsVector2i(const Glvectortypes::TVector2i &Value);
	virtual void __fastcall SetAsVector3i(const Glvectortypes::TVector3i &Value);
	virtual void __fastcall SetAsVector4i(const Glvectortypes::TVector4i &Value);
	virtual void __fastcall SetAsVector1ui(const unsigned Value);
	virtual void __fastcall SetAsVector2ui(const Glvectortypes::TVector2ui &Value);
	virtual void __fastcall SetAsVector3ui(const Glvectortypes::TVector3ui &Value);
	virtual void __fastcall SetAsVector4ui(const Glvectortypes::TVector4ui &Value);
	virtual Glvectortypes::TMatrix2f __fastcall GetAsMatrix2f();
	virtual Glvectortypes::TMatrix3f __fastcall GetAsMatrix3f();
	virtual Glvectortypes::TMatrix4f __fastcall GetAsMatrix4f();
	virtual void __fastcall SetAsMatrix2f(const Glvectortypes::TMatrix2f &Value);
	virtual void __fastcall SetAsMatrix3f(const Glvectortypes::TMatrix3f &Value);
	virtual void __fastcall SetAsMatrix4f(const Glvectortypes::TMatrix4f &Value);
	virtual unsigned __fastcall GetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget);
	virtual void __fastcall SetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget, const unsigned Value);
	virtual unsigned __fastcall GetAsUniformBuffer();
	virtual void __fastcall SetAsUniformBuffer(unsigned UBO);
public:
	/* TObject.Create */ inline __fastcall TGLSLShaderParameter() : Glcustomshader::TGLCustomShaderParameter() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSLShaderParameter() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSLShader : public TGLCustomGLSLShader
{
	typedef TGLCustomGLSLShader inherited;
	
__published:
	__property FragmentProgram;
	__property VertexProgram;
	__property GeometryProgram;
	__property OnApply;
	__property OnApplyEx;
	__property OnUnApply;
	__property OnInitialize;
	__property OnInitializeEx;
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
	__property ActiveVarying;
	__property TransformFeedBackMode = {default=0};
public:
	/* TGLCustomGLSLShader.Create */ inline __fastcall virtual TGLSLShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLShader(AOwner) { }
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLSHADER)
using namespace Glslshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslshaderHPP
