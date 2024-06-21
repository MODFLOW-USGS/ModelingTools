// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLParameter.pas' rev: 36.00 (Windows)

#ifndef GlslparameterHPP
#define GlslparameterHPP

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
#include <GLStrings.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorTypes.hpp>
#include <GLTextureFormat.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslparameter
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IShaderParameter;
typedef System::DelphiInterface<IShaderParameter> _di_IShaderParameter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSLDataType : unsigned char { GLSLTypeUndefined, GLSLType1F, GLSLType2F, GLSLType3F, GLSLType4F, GLSLType1I, GLSLType2I, GLSLType3I, GLSLType4I, GLSLType1UI, GLSLType2UI, GLSLType3UI, GLSLType4UI, GLSLTypeMat2F, GLSLTypeMat3F, GLSLTypeMat4F, GLSLTypeVoid };

enum DECLSPEC_DENUM TGLSLSamplerType : unsigned char { GLSLSamplerUndefined, GLSLSampler1D, GLSLSampler2D, GLSLSampler3D, GLSLSamplerCube, GLSLSampler1DShadow, GLSLSampler2DShadow, GLSLSampler1DArray, GLSLSampler2DArray, GLSLSampler1DArrayShadow, GLSLSampler2DArrayShadow, GLSLSamplerCubeShadow, GLSLIntSampler1D, GLSLIntSampler2D, GLSLIntSampler3D, GLSLIntSamplerCube, GLSLIntSampler1DArray, GLSLIntSampler2DArray, GLSLUIntSampler1D, GLSLUIntSampler2D, GLSLUIntSampler3D, GLSLUIntSamplerCube, GLSLUIntSampler1DArray, GLSLUIntSampler2DArray, GLSLSamplerRect, GLSLSamplerRectShadow, GLSLSamplerBuffer, GLSLIntSamplerRect, GLSLIntSamplerBuffer, GLSLUIntSamplerRect, GLSLUIntSamplerBuffer, GLSLSamplerMS, GLSLIntSamplerMS, GLSLUIntSamplerMS, GLSLSamplerMSArray, 
	GLSLIntSamplerMSArray, GLSLUIntSamplerMSArray };

enum DECLSPEC_DENUM TGLgsInTypes : unsigned char { gsInPoints, gsInLines, gsInAdjLines, gsInTriangles, gsInAdjTriangles };

enum DECLSPEC_DENUM TGLgsOutTypes : unsigned char { gsOutPoints, gsOutLineStrip, sOutTriangleStrip };

__interface IShaderParameter  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual TGLSLDataType __fastcall GetGLSLType() = 0 ;
	virtual TGLSLSamplerType __fastcall GetGLSLSamplerType() = 0 ;
	virtual System::UnicodeString __fastcall GetAutoSetMethod() = 0 ;
	virtual System::UnicodeString __fastcall GetTextureName() = 0 ;
	virtual System::UnicodeString __fastcall GetSamplerName() = 0 ;
	virtual Gltextureformat::TSwizzleVector __fastcall GetTextureSwizzle() = 0 ;
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetTextureSwizzle(const Gltextureformat::TSwizzleVector AValue) = 0 ;
	virtual float __fastcall GetFloat() = 0 ;
	virtual Glvectortypes::TVector2f __fastcall GetVec2() = 0 ;
	virtual Glvectortypes::TVector3f __fastcall GetVec3() = 0 ;
	virtual Glvectortypes::TVector4f __fastcall GetVec4() = 0 ;
	virtual int __fastcall GetInt() = 0 ;
	virtual Glvectortypes::TVector2i __fastcall GetIVec2() = 0 ;
	virtual Glvectortypes::TVector3i __fastcall GetIVec3() = 0 ;
	virtual Glvectortypes::TVector4i __fastcall GetIVec4() = 0 ;
	virtual unsigned __fastcall GetUInt() = 0 ;
	virtual Glvectortypes::TVector2ui __fastcall GetUVec2() = 0 ;
	virtual Glvectortypes::TVector3ui __fastcall GetUVec3() = 0 ;
	virtual Glvectortypes::TVector4ui __fastcall GetUVec4() = 0 ;
	virtual void __fastcall SetFloat(const Opengltokens::TGLfloat Value) = 0 ;
	virtual void __fastcall SetVec2(const Glvectortypes::TVector2f &Value) = 0 ;
	virtual void __fastcall SetVec3(const Glvectortypes::TVector3f &Value) = 0 ;
	virtual void __fastcall SetVec4(const Glvectortypes::TVector4f &Value) = 0 ;
	virtual void __fastcall SetInt(const int Value) = 0 ;
	virtual void __fastcall SetIVec2(const Glvectortypes::TVector2i &Value) = 0 ;
	virtual void __fastcall SetIVec3(const Glvectortypes::TVector3i &Value) = 0 ;
	virtual void __fastcall SetIVec4(const Glvectortypes::TVector4i &Value) = 0 ;
	virtual void __fastcall SetUInt(const unsigned Value) = 0 ;
	virtual void __fastcall SetUVec2(const Glvectortypes::TVector2ui &Value) = 0 ;
	virtual void __fastcall SetUVec3(const Glvectortypes::TVector3ui &Value) = 0 ;
	virtual void __fastcall SetUVec4(const Glvectortypes::TVector4ui &Value) = 0 ;
	virtual Glvectortypes::TMatrix2f __fastcall GetMat2() = 0 ;
	virtual Glvectortypes::TMatrix3f __fastcall GetMat3() = 0 ;
	virtual Glvectortypes::TMatrix4f __fastcall GetMat4() = 0 ;
	virtual void __fastcall SetMat2(const Glvectortypes::TMatrix2f &Value) = 0 ;
	virtual void __fastcall SetMat3(const Glvectortypes::TMatrix3f &Value) = 0 ;
	virtual void __fastcall SetMat4(const Glvectortypes::TMatrix4f &Value) = 0 ;
	virtual void __fastcall SetFloatArray(const Opengltokens::PGLfloat Values, int Count) = 0 ;
	virtual void __fastcall SetIntArray(const Opengltokens::PGLint Values, int Count) = 0 ;
	virtual void __fastcall SetUIntArray(const Opengltokens::PGLuint Values, int Count) = 0 ;
	__property System::UnicodeString Name = {read=GetName};
	__property TGLSLDataType GLSLType = {read=GetGLSLType};
	__property TGLSLSamplerType GLSLSamplerType = {read=GetGLSLSamplerType};
	__property Opengltokens::TGLfloat Float = {read=GetFloat, write=SetFloat};
	__property int Int = {read=GetInt, write=SetInt};
	__property unsigned uint = {read=GetUInt, write=SetUInt};
	__property Glvectortypes::TVector2f vec2 = {read=GetVec2, write=SetVec2};
	__property Glvectortypes::TVector3f vec3 = {read=GetVec3, write=SetVec3};
	__property Glvectortypes::TVector4f vec4 = {read=GetVec4, write=SetVec4};
	__property Glvectortypes::TVector2i ivec2 = {read=GetIVec2, write=SetIVec2};
	__property Glvectortypes::TVector3i ivec3 = {read=GetIVec3, write=SetIVec3};
	__property Glvectortypes::TVector4i ivec4 = {read=GetIVec4, write=SetIVec4};
	__property Glvectortypes::TVector2ui uvec2 = {read=GetUVec2, write=SetUVec2};
	__property Glvectortypes::TVector3ui uvec3 = {read=GetUVec3, write=SetUVec3};
	__property Glvectortypes::TVector4ui uvec4 = {read=GetUVec4, write=SetUVec4};
	__property Glvectortypes::TMatrix2f mat2 = {read=GetMat2, write=SetMat2};
	__property Glvectortypes::TMatrix3f mat3 = {read=GetMat3, write=SetMat3};
	__property Glvectortypes::TMatrix4f mat4 = {read=GetMat4, write=SetMat4};
	__property System::UnicodeString AutoSetMethod = {read=GetAutoSetMethod, write=SetAutoSetMethod};
	__property System::UnicodeString TextureName = {read=GetTextureName, write=SetTextureName};
	__property System::UnicodeString SamplerName = {read=GetSamplerName, write=SetSamplerName};
	__property Gltextureformat::TSwizzleVector TextureSwizzle = {read=GetTextureSwizzle, write=SetTextureSwizzle};
};

typedef System::StaticArray<System::AnsiString, 17> Glslparameter__1;

typedef System::StaticArray<System::AnsiString, 37> Glslparameter__2;

typedef void __fastcall (__closure *TUniformAutoSetMethod)(_di_IShaderParameter Sender, Glrendercontextinfo::TGLRenderContextInfo &ARci);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Glslparameter__1 cGLSLTypeString;
extern DELPHI_PACKAGE Glslparameter__2 cGLSLSamplerString;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 5> cGLgsInTypes;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 3> cGLgsOutTypes;
extern DELPHI_PACKAGE unsigned __fastcall GLSLTypeEnum(TGLSLDataType AType);
extern DELPHI_PACKAGE int __fastcall GLSLTypeComponentCount(TGLSLDataType AType);
extern DELPHI_PACKAGE void __fastcall RegisterUniformAutoSetMethod(System::UnicodeString AMethodName, TGLSLDataType AType, TUniformAutoSetMethod AMethod);
extern DELPHI_PACKAGE void __fastcall FillUniformAutoSetMethodList(System::Classes::TStrings* AList, TGLSLDataType TypeFilter)/* overload */;
extern DELPHI_PACKAGE void __fastcall FillUniformAutoSetMethodList(System::Classes::TStrings* AList, TGLSLSamplerType TypeFilter)/* overload */;
extern DELPHI_PACKAGE TUniformAutoSetMethod __fastcall GetUniformAutoSetMethod(System::UnicodeString AMethodName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetUniformAutoSetMethodName(TUniformAutoSetMethod AMethod);
}	/* namespace Glslparameter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLPARAMETER)
using namespace Glslparameter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslparameterHPP
