// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLState.pas' rev: 36.00 (Windows)

#ifndef GLStateHPP
#define GLStateHPP

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
#include <GLCrossPlatform.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glstate
{
//-- forward type declarations -----------------------------------------------
struct TLightSourceState;
struct TShaderLightSourceState;
struct TUBOStates;
class DELPHICLASS TGLStateCache;
struct TStateRecord;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLStateType : unsigned char { sttCurrent, sttPoint, sttLine, sttPolygon, sttPolygonStipple, sttPixelMode, sttLighting, sttFog, sttDepthBuffer, sttAccumBuffer, sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer, sttHint, sttEval, sttList, sttTexture, sttScissor, sttMultisample };

typedef System::Set<TGLStateType, TGLStateType::sttCurrent, TGLStateType::sttMultisample> TGLStateTypes;

enum DECLSPEC_DENUM TGLMeshPrimitive : unsigned char { mpNOPRIMITIVE, mpTRIANGLES, mpTRIANGLE_STRIP, mpTRIANGLE_FAN, mpPOINTS, mpLINES, mpLINE_LOOP, mpLINE_STRIP, mpLINES_ADJACENCY, mpLINE_STRIP_ADJACENCY, mpTRIANGLES_ADJACENCY, mpTRIANGLE_STRIP_ADJACENCY, mpPATCHES };

typedef System::Set<TGLMeshPrimitive, TGLMeshPrimitive::mpNOPRIMITIVE, TGLMeshPrimitive::mpPATCHES> TGLMeshPrimitives;

enum DECLSPEC_DENUM TGLState : unsigned char { stAlphaTest, stAutoNormal, stBlend, stColorMaterial, stCullFace, stDepthTest, stDither, stFog, stLighting, stLineSmooth, stLineStipple, stIndexLogicOp, stColorLogicOp, stNormalize, stPointSmooth, stPointSprite, stPolygonSmooth, stPolygonStipple, stScissorTest, stStencilTest, stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill, stDepthClamp };

typedef System::Set<TGLState, TGLState::stAlphaTest, TGLState::stDepthClamp> TGLStates;

enum DECLSPEC_DENUM TComparisonFunction : unsigned char { cfNever, cfAlways, cfLess, cfLEqual, cfEqual, cfGreater, cfNotEqual, cfGEqual };

typedef TComparisonFunction TStencilFunction;

typedef TComparisonFunction TDepthFunction;

enum DECLSPEC_DENUM TBlendFunction : unsigned char { bfZero, bfOne, bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor, bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha, bfConstantColor, bfOneMinusConstantColor, bfConstantAlpha, bfOneMinusConstantAlpha, bfSrcAlphaSat };

typedef TBlendFunction TDstBlendFunction;

enum DECLSPEC_DENUM TBlendEquation : unsigned char { beAdd, beSubtract, beReverseSubtract, beMin, beMax };

enum DECLSPEC_DENUM TStencilOp : unsigned char { soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap, soDecrWrap };

enum DECLSPEC_DENUM TLogicOp : unsigned char { loClear, loAnd, loAndReverse, loCopy, loAndInverted, loNoOp, loXOr, loOr, loNor, loEquiv, loInvert, loOrReverse, loCopyInverted, loOrInverted, loNAnd, loSet };

enum DECLSPEC_DENUM TQueryType : unsigned char { qrySamplesPassed, qryPrimitivesGenerated, qryTransformFeedbackPrimitivesWritten, qryTimeElapsed, qryAnySamplesPassed };

enum DECLSPEC_DENUM TFaceWinding : unsigned char { fwCounterClockWise, fwClockWise };

enum DECLSPEC_DENUM TPolygonMode : unsigned char { pmFill, pmLines, pmPoints };

enum DECLSPEC_DENUM TCullFaceMode : unsigned char { cmFront, cmBack, cmFrontAndBack };

enum DECLSPEC_DENUM TColorComponent : unsigned char { ccRed, ccGreen, ccBlue, ccAlpha };

typedef System::Set<TColorComponent, TColorComponent::ccRed, TColorComponent::ccAlpha> TColorMask;

enum DECLSPEC_DENUM THintType : unsigned char { hintDontCare, hintFastest, hintNicest };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TLightSourceState
{
public:
	System::StaticArray<Glvectortypes::TVector4f, 16> Position;
	System::StaticArray<Glvectortypes::TVector4f, 16> Ambient;
	System::StaticArray<Glvectortypes::TVector4f, 16> Diffuse;
	System::StaticArray<Glvectortypes::TVector4f, 16> Specular;
	System::StaticArray<Glvectortypes::TVector4f, 16> SpotDirection;
	System::StaticArray<Glvectortypes::TVector4f, 16> SpotCosCutoffExponent;
	System::StaticArray<Glvectortypes::TVector4f, 16> Attenuation;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TShaderLightSourceState
{
public:
	System::StaticArray<Glvectortypes::TVector4f, 8> Position;
	System::StaticArray<Glvectortypes::TVector4f, 8> Ambient;
	System::StaticArray<Glvectortypes::TVector4f, 8> Diffuse;
	System::StaticArray<Glvectortypes::TVector4f, 8> Specular;
	System::StaticArray<Glvectortypes::TVector4f, 8> SpotDirection;
	System::StaticArray<Glvectortypes::TVector4f, 8> SpotCosCutoffExponent;
	System::StaticArray<Glvectortypes::TVector4f, 8> Attenuation;
};
#pragma pack(pop)


typedef void __fastcall (*TOnLightsChanged)(System::TObject* Sender);

enum DECLSPEC_DENUM TGLBufferBindingTarget : unsigned char { bbtUniform, bbtTransformFeedBack };

struct DECLSPEC_DRECORD TUBOStates
{
public:
	unsigned FUniformBufferBinding;
	Opengltokens::TGLintptr FOffset;
	Opengltokens::TGLsizeiptr FSize;
};


enum DECLSPEC_DENUM TGLMaterialLevel : unsigned char { mlAuto, mlFixedFunction, mlMultitexturing, mlSM3, mlSM4, mlSM5 };

class PASCALIMPLEMENTATION TGLStateCache : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLStateTypes> _TGLStateCache__1;
	
	
private:
	System::StaticArray<System::StaticArray<Glvectortypes::TVector4f, 4>, 2> FFrontBackColors;
	System::StaticArray<int, 2> FFrontBackShininess;
	TComparisonFunction FAlphaFunc;
	float FAlphaRef;
	TPolygonMode FPolygonBackMode;
	unsigned FMaxLights;
	System::StaticArray<bool, 16> FLightEnabling;
	System::StaticArray<int, 16> FLightIndices;
	int FLightNumber;
	TLightSourceState FLightStates;
	System::StaticArray<float, 16> FSpotCutoff;
	TShaderLightSourceState FShaderLightStates;
	bool FShaderLightStatesChanged;
	bool FColorWriting;
	TGLStates FStates;
	_TGLStateCache__1 FListStates;
	unsigned FCurrentList;
	System::StaticArray<bool, 4> FTextureMatrixIsIdentity;
	bool FFFPLight;
	unsigned FVertexArrayBinding;
	unsigned FArrayBufferBinding;
	unsigned FElementBufferBinding;
	unsigned FTextureBufferBinding;
	Opengltokens::TGLboolean FEnablePrimitiveRestart;
	unsigned FPrimitiveRestartIndex;
	Glvectortypes::TVector4i FViewPort;
	System::StaticArray<double, 2> FDepthRange;
	System::StaticArray<System::ByteBool, 8> FEnableClipDistance;
	Opengltokens::TGLboolean FEnableDepthClamp;
	unsigned FClampReadColor;
	unsigned FProvokingVertex;
	Opengltokens::TGLfloat FPointSize;
	Opengltokens::TGLfloat FPointFadeThresholdSize;
	unsigned FPointSpriteCoordOrigin;
	float FLineWidth;
	Opengltokens::TGLint FLineStippleFactor;
	Opengltokens::TGLushort FLineStipplePattern;
	Opengltokens::TGLboolean FEnableLineSmooth;
	Opengltokens::TGLboolean FEnableCullFace;
	TCullFaceMode FCullFaceMode;
	TFaceWinding FFrontFace;
	Opengltokens::TGLboolean FEnablePolygonSmooth;
	TPolygonMode FPolygonMode;
	Opengltokens::TGLfloat FPolygonOffsetFactor;
	Opengltokens::TGLfloat FPolygonOffsetUnits;
	Opengltokens::TGLboolean FEnablePolygonOffsetPoint;
	Opengltokens::TGLboolean FEnablePolygonOffsetLine;
	Opengltokens::TGLboolean FEnablePolygonOffsetFill;
	Opengltokens::TGLboolean FEnableMultisample;
	Opengltokens::TGLboolean FEnableSampleAlphaToCoverage;
	Opengltokens::TGLboolean FEnableSampleAlphaToOne;
	Opengltokens::TGLboolean FEnableSampleCoverage;
	Opengltokens::TGLfloat FSampleCoverageValue;
	Opengltokens::TGLboolean FSampleCoverageInvert;
	Opengltokens::TGLboolean FEnableSampleMask;
	System::StaticArray<unsigned, 16> FSampleMaskValue;
	unsigned FMaxTextureSize;
	unsigned FMax3DTextureSize;
	unsigned FMaxCubeTextureSize;
	unsigned FMaxArrayTextureSize;
	unsigned FMaxTextureImageUnits;
	unsigned FMaxTextureAnisotropy;
	unsigned FMaxSamples;
	System::StaticArray<System::StaticArray<unsigned, 12>, 48> FTextureBinding;
	System::StaticArray<System::StaticArray<double, 12>, 48> FTextureBindingTime;
	System::StaticArray<unsigned, 48> FSamplerBinding;
	Opengltokens::TGLint FActiveTexture;
	System::StaticArray<System::StaticArray<bool, 12>, 48> FActiveTextureEnabling;
	Opengltokens::TGLboolean FEnableScissorTest;
	Glvectortypes::TVector4i FScissorBox;
	Opengltokens::TGLboolean FEnableStencilTest;
	TStencilFunction FStencilFunc;
	unsigned FStencilValueMask;
	Opengltokens::TGLint FStencilRef;
	TStencilOp FStencilFail;
	TStencilOp FStencilPassDepthFail;
	TStencilOp FStencilPassDepthPass;
	TStencilFunction FStencilBackFunc;
	unsigned FStencilBackValueMask;
	unsigned FStencilBackRef;
	TStencilOp FStencilBackFail;
	TStencilOp FStencilBackPassDepthPass;
	TStencilOp FStencilBackPassDepthFail;
	Opengltokens::TGLboolean FEnableDepthTest;
	TDepthFunction FDepthFunc;
	System::StaticArray<System::ByteBool, 16> FEnableBlend;
	TBlendFunction FBlendSrcRGB;
	TBlendFunction FBlendSrcAlpha;
	TDstBlendFunction FBlendDstRGB;
	TDstBlendFunction FBlendDstAlpha;
	TBlendEquation FBlendEquationRGB;
	TBlendEquation FBlendEquationAlpha;
	Glvectorgeometry::TVector FBlendColor;
	Opengltokens::TGLboolean FEnableFramebufferSRGB;
	Opengltokens::TGLboolean FEnableDither;
	Opengltokens::TGLboolean FEnableColorLogicOp;
	TLogicOp FLogicOpMode;
	System::StaticArray<TColorMask, 16> FColorWriteMask;
	Opengltokens::TGLboolean FDepthWriteMask;
	unsigned FStencilWriteMask;
	unsigned FStencilBackWriteMask;
	Glvectorgeometry::TVector FColorClearValue;
	Opengltokens::TGLfloat FDepthClearValue;
	unsigned FStencilClearValue;
	unsigned FDrawFrameBuffer;
	unsigned FReadFrameBuffer;
	unsigned FRenderBuffer;
	Opengltokens::TGLboolean FUnpackSwapBytes;
	Opengltokens::TGLboolean FUnpackLSBFirst;
	unsigned FUnpackImageHeight;
	unsigned FUnpackSkipImages;
	unsigned FUnpackRowLength;
	unsigned FUnpackSkipRows;
	unsigned FUnpackSkipPixels;
	unsigned FUnpackAlignment;
	Opengltokens::TGLboolean FPackSwapBytes;
	Opengltokens::TGLboolean FPackLSBFirst;
	unsigned FPackImageHeight;
	unsigned FPackSkipImages;
	unsigned FPackRowLength;
	unsigned FPackSkipRows;
	unsigned FPackSkipPixels;
	unsigned FPackAlignment;
	unsigned FPixelPackBufferBinding;
	unsigned FPixelUnpackBufferBinding;
	unsigned FCurrentProgram;
	unsigned FMaxTextureUnits;
	unsigned FUniformBufferBinding;
	System::StaticArray<System::StaticArray<TUBOStates, 75>, 2> FUBOStates;
	System::StaticArray<Glvectortypes::TVector4f, 16> FCurrentVertexAttrib;
	Opengltokens::TGLboolean FEnableProgramPointSize;
	unsigned FTransformFeedbackBufferBinding;
	THintType FTextureCompressionHint;
	THintType FPolygonSmoothHint;
	THintType FFragmentShaderDerivitiveHint;
	THintType FLineSmoothHint;
	THintType FMultisampleFilterHint;
	System::StaticArray<unsigned, 5> FCurrentQuery;
	unsigned FCopyReadBufferBinding;
	unsigned FCopyWriteBufferBinding;
	Opengltokens::TGLboolean FEnableTextureCubeMapSeamless;
	bool FInsideList;
	TOnLightsChanged FOnLightsChanged;
	
protected:
	void __fastcall SetVertexArrayBinding(const unsigned Value);
	unsigned __fastcall GetArrayBufferBinding();
	void __fastcall SetArrayBufferBinding(const unsigned Value);
	unsigned __fastcall GetElementBufferBinding();
	void __fastcall SetElementBufferBinding(const unsigned Value);
	Opengltokens::TGLboolean __fastcall GetEnablePrimitiveRestart();
	unsigned __fastcall GetPrimitiveRestartIndex();
	void __fastcall SetEnablePrimitiveRestart(const Opengltokens::TGLboolean enabled);
	void __fastcall SetPrimitiveRestartIndex(const unsigned index);
	void __fastcall SetTextureBufferBinding(const unsigned Value);
	void __fastcall SetViewPort(const Glvectortypes::TVector4i &Value);
	Opengltokens::TGLboolean __fastcall GetEnableClipDistance(unsigned ClipDistance);
	void __fastcall SetEnableClipDistance(unsigned index, const Opengltokens::TGLboolean Value);
	Opengltokens::TGLclampd __fastcall GetDepthRangeFar();
	void __fastcall SetDepthRangeFar(const Opengltokens::TGLclampd Value);
	Opengltokens::TGLclampd __fastcall GetDepthRangeNear();
	void __fastcall SetDepthRangeNear(const Opengltokens::TGLclampd Value);
	void __fastcall SetEnableDepthClamp(const Opengltokens::TGLboolean enabled);
	void __fastcall SetClampReadColor(const unsigned Value);
	void __fastcall SetProvokingVertex(const unsigned Value);
	void __fastcall SetPointSize(const Opengltokens::TGLfloat Value);
	void __fastcall SetPointFadeThresholdSize(const Opengltokens::TGLfloat Value);
	void __fastcall SetPointSpriteCoordOrigin(const unsigned Value);
	void __fastcall SetLineWidth(const Opengltokens::TGLfloat Value);
	void __fastcall SetLineStippleFactor(const Opengltokens::TGLint Value);
	void __fastcall SetLineStipplePattern(const Opengltokens::TGLushort Value);
	void __fastcall SetEnableLineSmooth(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableCullFace(const Opengltokens::TGLboolean Value);
	void __fastcall SetCullFaceMode(const TCullFaceMode Value);
	void __fastcall SetFrontFace(const TFaceWinding Value);
	void __fastcall SetEnablePolygonSmooth(const Opengltokens::TGLboolean Value);
	void __fastcall SetPolygonMode(const TPolygonMode Value);
	void __fastcall SetPolygonOffsetFactor(const Opengltokens::TGLfloat Value);
	void __fastcall SetPolygonOffsetUnits(const Opengltokens::TGLfloat Value);
	void __fastcall SetEnablePolygonOffsetPoint(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnablePolygonOffsetLine(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnablePolygonOffsetFill(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableMultisample(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableSampleAlphaToCoverage(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableSampleAlphaToOne(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableSampleCoverage(const Opengltokens::TGLboolean Value);
	void __fastcall SetSampleCoverageValue(const Opengltokens::TGLfloat Value);
	void __fastcall SetSampleCoverageInvert(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableSampleMask(const Opengltokens::TGLboolean Value);
	Opengltokens::TGLbitfield __fastcall GetSampleMaskValue(int index);
	void __fastcall SetSampleMaskValue(int index, const Opengltokens::TGLbitfield Value);
	unsigned __fastcall GetMaxTextureSize();
	unsigned __fastcall GetMax3DTextureSize();
	unsigned __fastcall GetMaxCubeTextureSize();
	unsigned __fastcall GetMaxArrayTextureSize();
	unsigned __fastcall GetMaxTextureImageUnits();
	unsigned __fastcall GetMaxTextureAnisotropy();
	unsigned __fastcall GetMaxSamples();
	unsigned __fastcall GetTextureBinding(int index, Gltextureformat::TGLTextureTarget target);
	double __fastcall GetTextureBindingTime(int index, Gltextureformat::TGLTextureTarget target);
	void __fastcall SetTextureBinding(int index, Gltextureformat::TGLTextureTarget target, const unsigned Value);
	bool __fastcall GetActiveTextureEnabled(Gltextureformat::TGLTextureTarget target);
	void __fastcall SetActiveTextureEnabled(Gltextureformat::TGLTextureTarget target, const bool Value);
	unsigned __fastcall GetSamplerBinding(unsigned index);
	void __fastcall SetSamplerBinding(unsigned index, const unsigned Value);
	void __fastcall SetActiveTexture(const Opengltokens::TGLint Value);
	void __fastcall SetEnableScissorTest(const Opengltokens::TGLboolean Value);
	void __fastcall SetScissorBox(const Glvectortypes::TVector4i &Value);
	void __fastcall SetEnableStencilTest(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableDepthTest(const Opengltokens::TGLboolean Value);
	void __fastcall SetDepthFunc(const TDepthFunction Value);
	Opengltokens::TGLboolean __fastcall GetEnableBlend(int index);
	void __fastcall SetEnableBlend(int index, const Opengltokens::TGLboolean Value);
	void __fastcall SetBlendColor(const Glvectorgeometry::TVector &Value);
	void __fastcall SetEnableFramebufferSRGB(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableDither(const Opengltokens::TGLboolean Value);
	void __fastcall SetEnableColorLogicOp(const Opengltokens::TGLboolean Value);
	void __fastcall SetLogicOpMode(const TLogicOp Value);
	TColorMask __fastcall GetColorWriteMask(int index);
	void __fastcall SetColorWriteMask(int index, const TColorMask Value);
	void __fastcall SetDepthWriteMask(const Opengltokens::TGLboolean Value);
	void __fastcall SetStencilWriteMask(const unsigned Value);
	void __fastcall SetStencilBackWriteMask(const unsigned Value);
	void __fastcall SetColorClearValue(const Glvectorgeometry::TVector &Value);
	void __fastcall SetDepthClearValue(const Opengltokens::TGLfloat Value);
	void __fastcall SetStencilClearValue(const unsigned Value);
	void __fastcall SetDrawFrameBuffer(const unsigned Value);
	void __fastcall SetReadFrameBuffer(const unsigned Value);
	void __fastcall SetRenderBuffer(const unsigned Value);
	void __fastcall SetUnpackSwapBytes(const Opengltokens::TGLboolean Value);
	void __fastcall SetUnpackLSBFirst(const Opengltokens::TGLboolean Value);
	void __fastcall SetUnpackImageHeight(const unsigned Value);
	void __fastcall SetUnpackSkipImages(const unsigned Value);
	void __fastcall SetUnpackRowLength(const unsigned Value);
	void __fastcall SetUnpackSkipRows(const unsigned Value);
	void __fastcall SetUnpackSkipPixels(const unsigned Value);
	void __fastcall SetUnpackAlignment(const unsigned Value);
	void __fastcall SetPackSwapBytes(const Opengltokens::TGLboolean Value);
	void __fastcall SetPackLSBFirst(const Opengltokens::TGLboolean Value);
	void __fastcall SetPackImageHeight(const unsigned Value);
	void __fastcall SetPackSkipImages(const unsigned Value);
	void __fastcall SetPackRowLength(const unsigned Value);
	void __fastcall SetPackSkipRows(const unsigned Value);
	void __fastcall SetPackSkipPixels(const unsigned Value);
	void __fastcall SetPackAlignment(const unsigned Value);
	void __fastcall SetPixelPackBufferBinding(const unsigned Value);
	void __fastcall SetPixelUnpackBufferBinding(const unsigned Value);
	void __fastcall SetCurrentProgram(const unsigned Value);
	void __fastcall SetUniformBufferBinding(const unsigned Value);
	unsigned __fastcall GetMaxTextureUnits();
	Glvectorgeometry::TVector __fastcall GetCurrentVertexAttrib(int index);
	void __fastcall SetCurrentVertexAttrib(int index, const Glvectorgeometry::TVector &Value);
	void __fastcall SetEnableProgramPointSize(const Opengltokens::TGLboolean Value);
	void __fastcall SetTransformFeedbackBufferBinding(const unsigned Value);
	void __fastcall SetLineSmoothHint(const THintType Value);
	void __fastcall SetPolygonSmoothHint(const THintType Value);
	void __fastcall SetTextureCompressionHint(const THintType Value);
	void __fastcall SetFragmentShaderDerivitiveHint(const THintType Value);
	void __fastcall SetMultisampleFilterHint(const THintType Value);
	unsigned __fastcall GetCurrentQuery(TQueryType index);
	void __fastcall SetCopyReadBufferBinding(const unsigned Value);
	void __fastcall SetCopyWriteBufferBinding(const unsigned Value);
	void __fastcall SetEnableTextureCubeMapSeamless(const Opengltokens::TGLboolean Value);
	void __fastcall SetFFPLight(bool Value);
	int __fastcall GetMaxLights();
	bool __fastcall GetLightEnabling(int I);
	void __fastcall SetLightEnabling(int I, bool Value);
	Glvectorgeometry::TVector __fastcall GetLightPosition(int I);
	void __fastcall SetLightPosition(int I, const Glvectorgeometry::TVector &Value);
	Glvectorgeometry::TAffineVector __fastcall GetLightSpotDirection(int I);
	void __fastcall SetLightSpotDirection(int I, const Glvectorgeometry::TAffineVector &Value);
	Glvectorgeometry::TVector __fastcall GetLightAmbient(int I);
	void __fastcall SetLightAmbient(int I, const Glvectorgeometry::TVector &Value);
	Glvectorgeometry::TVector __fastcall GetLightDiffuse(int I);
	void __fastcall SetLightDiffuse(int I, const Glvectorgeometry::TVector &Value);
	Glvectorgeometry::TVector __fastcall GetLightSpecular(int I);
	void __fastcall SetLightSpecular(int I, const Glvectorgeometry::TVector &Value);
	float __fastcall GetSpotCutoff(int I);
	void __fastcall SetSpotCutoff(int I, const float Value);
	float __fastcall GetSpotExponent(int I);
	void __fastcall SetSpotExponent(int I, const float Value);
	float __fastcall GetConstantAtten(int I);
	void __fastcall SetConstantAtten(int I, const float Value);
	float __fastcall GetLinearAtten(int I);
	void __fastcall SetLinearAtten(int I, const float Value);
	float __fastcall GetQuadAtten(int I);
	void __fastcall SetQuadAtten(int I, const float Value);
	void __fastcall SetForwardContext(bool Value);
	Glvectorgeometry::TVector __fastcall GetMaterialAmbient(const TCullFaceMode aFace);
	Glvectorgeometry::TVector __fastcall GetMaterialDiffuse(const TCullFaceMode aFace);
	Glvectorgeometry::TVector __fastcall GetMaterialSpecular(const TCullFaceMode aFace);
	Glvectorgeometry::TVector __fastcall GetMaterialEmission(const TCullFaceMode aFace);
	int __fastcall GetMaterialShininess(const TCullFaceMode aFace);
	
public:
	__fastcall virtual TGLStateCache();
	__fastcall virtual ~TGLStateCache();
	void __fastcall PushAttrib(const TGLStateTypes stateTypes);
	void __fastcall PopAttrib();
	void __fastcall Enable(const TGLState aState);
	void __fastcall Disable(const TGLState aState);
	void __fastcall PerformEnable(const TGLState aState);
	void __fastcall PerformDisable(const TGLState aState);
	void __fastcall SetGLState _DEPRECATED_ATTRIBUTE0 (const TGLState aState);
	void __fastcall UnSetGLState _DEPRECATED_ATTRIBUTE0 (const TGLState aState);
	void __fastcall ResetGLPolygonMode _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall ResetGLMaterialColors _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall ResetGLTexture _DEPRECATED_ATTRIBUTE0 (const int TextureUnit);
	void __fastcall ResetGLCurrentTexture _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall ResetGLFrontFace _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall SetGLFrontFaceCW _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall ResetAll _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall SetGLMaterialColors(const TCullFaceMode aFace, const Glvectorgeometry::TVector &emission, const Glvectorgeometry::TVector &Ambient, const Glvectorgeometry::TVector &Diffuse, const Glvectorgeometry::TVector &Specular, const int shininess);
	__property Glvectorgeometry::TVector MaterialAmbient[const TCullFaceMode aFace] = {read=GetMaterialAmbient};
	__property Glvectorgeometry::TVector MaterialDiffuse[const TCullFaceMode aFace] = {read=GetMaterialDiffuse};
	__property Glvectorgeometry::TVector MaterialSpecular[const TCullFaceMode aFace] = {read=GetMaterialSpecular};
	__property Glvectorgeometry::TVector MaterialEmission[const TCullFaceMode aFace] = {read=GetMaterialEmission};
	__property int MaterialShininess[const TCullFaceMode aFace] = {read=GetMaterialShininess};
	void __fastcall SetGLMaterialAlphaChannel(const unsigned aFace, const Opengltokens::TGLfloat alpha);
	void __fastcall SetGLMaterialDiffuseColor(const unsigned aFace, const Glvectorgeometry::TVector &Diffuse);
	__property bool FixedFunctionPipeLight = {read=FFFPLight, write=SetFFPLight, nodefault};
	__property int MaxLights = {read=GetMaxLights, nodefault};
	__property bool LightEnabling[int Index] = {read=GetLightEnabling, write=SetLightEnabling};
	__property Glvectorgeometry::TVector LightPosition[int Index] = {read=GetLightPosition, write=SetLightPosition};
	__property Glvectorgeometry::TAffineVector LightSpotDirection[int Index] = {read=GetLightSpotDirection, write=SetLightSpotDirection};
	__property Glvectorgeometry::TVector LightAmbient[int Index] = {read=GetLightAmbient, write=SetLightAmbient};
	__property Glvectorgeometry::TVector LightDiffuse[int Index] = {read=GetLightDiffuse, write=SetLightDiffuse};
	__property Glvectorgeometry::TVector LightSpecular[int Index] = {read=GetLightSpecular, write=SetLightSpecular};
	__property float LightSpotCutoff[int Index] = {read=GetSpotCutoff, write=SetSpotCutoff};
	__property float LightSpotExponent[int Index] = {read=GetSpotExponent, write=SetSpotExponent};
	__property float LightConstantAtten[int Index] = {read=GetConstantAtten, write=SetConstantAtten};
	__property float LightLinearAtten[int Index] = {read=GetLinearAtten, write=SetLinearAtten};
	__property float LightQuadraticAtten[int Index] = {read=GetQuadAtten, write=SetQuadAtten};
	Opengltokens::PGLint __fastcall GetLightIndicesAsAddress();
	void * __fastcall GetLightStateAsAddress();
	__property int LightNumber = {read=FLightNumber, nodefault};
	__property TOnLightsChanged OnLightsChanged = {read=FOnLightsChanged, write=FOnLightsChanged};
	void __fastcall SetGLAlphaFunction(TComparisonFunction func, float ref);
	__property unsigned VertexArrayBinding = {read=FVertexArrayBinding, write=SetVertexArrayBinding, nodefault};
	__property unsigned ArrayBufferBinding = {read=GetArrayBufferBinding, write=SetArrayBufferBinding, nodefault};
	__property unsigned ElementBufferBinding = {read=GetElementBufferBinding, write=SetElementBufferBinding, nodefault};
	__property Opengltokens::TGLboolean EnablePrimitiveRestart = {read=GetEnablePrimitiveRestart, write=SetEnablePrimitiveRestart, nodefault};
	__property unsigned PrimitiveRestartIndex = {read=GetPrimitiveRestartIndex, write=SetPrimitiveRestartIndex, nodefault};
	__property unsigned TextureBufferBinding = {read=FTextureBufferBinding, write=SetTextureBufferBinding, nodefault};
	__property Glvectortypes::TVector4i ViewPort = {read=FViewPort, write=SetViewPort};
	void __fastcall SetDepthRange(const Opengltokens::TGLclampd ZNear, const Opengltokens::TGLclampd ZFar);
	__property Opengltokens::TGLclampd DepthRangeNear = {read=GetDepthRangeNear, write=SetDepthRangeNear};
	__property Opengltokens::TGLclampd DepthRangeFar = {read=GetDepthRangeFar, write=SetDepthRangeFar};
	__property Opengltokens::TGLboolean EnableClipDistance[unsigned Index] = {read=GetEnableClipDistance, write=SetEnableClipDistance};
	__property Opengltokens::TGLboolean EnableDepthClamp = {read=FEnableDepthClamp, write=SetEnableDepthClamp, nodefault};
	__property unsigned ClampReadColor = {read=FClampReadColor, write=SetClampReadColor, nodefault};
	__property unsigned ProvokingVertex = {read=FProvokingVertex, write=SetProvokingVertex, nodefault};
	__property Opengltokens::TGLfloat PointSize = {read=FPointSize, write=SetPointSize};
	__property Opengltokens::TGLfloat PointFadeThresholdSize = {read=FPointFadeThresholdSize, write=SetPointFadeThresholdSize};
	__property unsigned PointSpriteCoordOrigin = {read=FPointSpriteCoordOrigin, write=SetPointSpriteCoordOrigin, nodefault};
	__property Opengltokens::TGLfloat LineWidth = {read=FLineWidth, write=SetLineWidth};
	__property Opengltokens::TGLint LineStippleFactor = {read=FLineStippleFactor, write=SetLineStippleFactor, nodefault};
	__property Opengltokens::TGLushort LineStipplePattern = {read=FLineStipplePattern, write=SetLineStipplePattern, nodefault};
	__property Opengltokens::TGLboolean EnableLineSmooth = {read=FEnableLineSmooth, write=SetEnableLineSmooth, nodefault};
	__property Opengltokens::TGLboolean EnableCullFace = {read=FEnableCullFace, write=SetEnableCullFace, nodefault};
	__property TCullFaceMode CullFaceMode = {read=FCullFaceMode, write=SetCullFaceMode, nodefault};
	__property TFaceWinding FrontFace = {read=FFrontFace, write=SetFrontFace, nodefault};
	__property Opengltokens::TGLboolean EnablePolygonSmooth = {read=FEnablePolygonSmooth, write=SetEnablePolygonSmooth, nodefault};
	__property TPolygonMode PolygonMode = {read=FPolygonMode, write=SetPolygonMode, nodefault};
	__property Opengltokens::TGLfloat PolygonOffsetFactor = {read=FPolygonOffsetFactor, write=SetPolygonOffsetFactor};
	__property Opengltokens::TGLfloat PolygonOffsetUnits = {read=FPolygonOffsetUnits, write=SetPolygonOffsetUnits};
	void __fastcall SetPolygonOffset(const Opengltokens::TGLfloat factor, const Opengltokens::TGLfloat units);
	__property Opengltokens::TGLboolean EnablePolygonOffsetPoint = {read=FEnablePolygonOffsetPoint, write=SetEnablePolygonOffsetPoint, nodefault};
	__property Opengltokens::TGLboolean EnablePolygonOffsetLine = {read=FEnablePolygonOffsetLine, write=SetEnablePolygonOffsetLine, nodefault};
	__property Opengltokens::TGLboolean EnablePolygonOffsetFill = {read=FEnablePolygonOffsetFill, write=SetEnablePolygonOffsetFill, nodefault};
	__property Opengltokens::TGLboolean EnableMultisample = {read=FEnableMultisample, write=SetEnableMultisample, nodefault};
	__property Opengltokens::TGLboolean EnableSampleAlphaToCoverage = {read=FEnableSampleAlphaToCoverage, write=SetEnableSampleAlphaToCoverage, nodefault};
	__property Opengltokens::TGLboolean EnableSampleAlphaToOne = {read=FEnableSampleAlphaToOne, write=SetEnableSampleAlphaToOne, nodefault};
	__property Opengltokens::TGLboolean EnableSampleCoverage = {read=FEnableSampleCoverage, write=SetEnableSampleCoverage, nodefault};
	__property Opengltokens::TGLfloat SampleCoverageValue = {read=FSampleCoverageValue, write=SetSampleCoverageValue};
	__property Opengltokens::TGLboolean SampleCoverageInvert = {read=FSampleCoverageInvert, write=SetSampleCoverageInvert, nodefault};
	void __fastcall SetSampleCoverage(const Opengltokens::TGLfloat Value, Opengltokens::TGLboolean invert);
	__property Opengltokens::TGLboolean EnableSampleMask = {read=FEnableSampleMask, write=SetEnableSampleMask, nodefault};
	__property Opengltokens::TGLbitfield SampleMaskValue[int Index] = {read=GetSampleMaskValue, write=SetSampleMaskValue};
	__property unsigned TextureBinding[int Index][Gltextureformat::TGLTextureTarget target] = {read=GetTextureBinding, write=SetTextureBinding};
	__property double TextureBindingTime[int Index][Gltextureformat::TGLTextureTarget target] = {read=GetTextureBindingTime};
	__property bool ActiveTextureEnabled[Gltextureformat::TGLTextureTarget target] = {read=GetActiveTextureEnabled, write=SetActiveTextureEnabled};
	__property unsigned SamplerBinding[unsigned Index] = {read=GetSamplerBinding, write=SetSamplerBinding};
	__property unsigned MaxTextureSize = {read=GetMaxTextureSize, nodefault};
	__property unsigned Max3DTextureSize = {read=GetMax3DTextureSize, nodefault};
	__property unsigned MaxCubeTextureSize = {read=GetMaxCubeTextureSize, nodefault};
	__property unsigned MaxArrayTextureSize = {read=GetMaxArrayTextureSize, nodefault};
	__property unsigned MaxTextureImageUnits = {read=GetMaxTextureImageUnits, nodefault};
	__property unsigned MaxTextureAnisotropy = {read=GetMaxTextureAnisotropy, nodefault};
	__property unsigned MaxSamples = {read=GetMaxSamples, nodefault};
	__property Opengltokens::TGLint ActiveTexture = {read=FActiveTexture, write=SetActiveTexture, nodefault};
	__property Opengltokens::TGLboolean EnableScissorTest = {read=FEnableScissorTest, write=SetEnableScissorTest, nodefault};
	__property Glvectortypes::TVector4i ScissorBox = {read=FScissorBox, write=SetScissorBox};
	__property Opengltokens::TGLboolean EnableStencilTest = {read=FEnableStencilTest, write=SetEnableStencilTest, nodefault};
	__property TStencilFunction StencilFunc = {read=FStencilFunc, nodefault};
	__property unsigned StencilValueMask = {read=FStencilValueMask, nodefault};
	__property Opengltokens::TGLint StencilRef = {read=FStencilRef, nodefault};
	__property TStencilOp StencilFail = {read=FStencilFail, nodefault};
	__property TStencilOp StencilPassDepthFail = {read=FStencilPassDepthFail, nodefault};
	__property TStencilOp StencilPassDepthPass = {read=FStencilPassDepthPass, nodefault};
	__property TStencilFunction StencilBackFunc = {read=FStencilBackFunc, nodefault};
	__property unsigned StencilBackValueMask = {read=FStencilBackValueMask, nodefault};
	__property unsigned StencilBackRef = {read=FStencilBackRef, nodefault};
	__property TStencilOp StencilBackFail = {read=FStencilBackFail, nodefault};
	__property TStencilOp StencilBackPassDepthFail = {read=FStencilBackPassDepthFail, nodefault};
	__property TStencilOp StencilBackPassDepthPass = {read=FStencilBackPassDepthPass, nodefault};
	void __fastcall SetStencilFunc(const TStencilFunction func, const Opengltokens::TGLint ref, const unsigned mask);
	void __fastcall SetStencilFuncSeparate(const TCullFaceMode face, const TStencilFunction func, const Opengltokens::TGLint ref, const unsigned mask);
	void __fastcall SetStencilOp(const TStencilOp fail, const TStencilOp zfail, const TStencilOp zpass);
	void __fastcall SetStencilOpSeparate(const TCullFaceMode face, const TStencilOp sfail, const TStencilOp dpfail, const TStencilOp dppass);
	__property Opengltokens::TGLboolean EnableDepthTest = {read=FEnableDepthTest, write=SetEnableDepthTest, nodefault};
	__property TDepthFunction DepthFunc = {read=FDepthFunc, write=SetDepthFunc, nodefault};
	__property Opengltokens::TGLboolean EnableBlend[int Index] = {read=GetEnableBlend, write=SetEnableBlend};
	__property TBlendFunction BlendSrcRGB = {read=FBlendSrcRGB, nodefault};
	__property TBlendFunction BlendSrcAlpha = {read=FBlendSrcAlpha, nodefault};
	__property TDstBlendFunction BlendDstRGB = {read=FBlendDstRGB, nodefault};
	__property TDstBlendFunction BlendDstAlpha = {read=FBlendDstAlpha, nodefault};
	void __fastcall SetBlendFunc(const TBlendFunction Src, const TDstBlendFunction Dst);
	void __fastcall SetBlendFuncSeparate(const TBlendFunction SrcRGB, const TDstBlendFunction DstRGB, const TBlendFunction SrcAlpha, const TDstBlendFunction DstAlpha);
	__property TBlendEquation BlendEquationRGB = {read=FBlendEquationRGB, nodefault};
	__property TBlendEquation BlendEquationAlpha = {read=FBlendEquationAlpha, nodefault};
	void __fastcall SetBlendEquation(const TBlendEquation mode);
	void __fastcall SetBlendEquationSeparate(const TBlendEquation modeRGB, const TBlendEquation modeAlpha);
	__property Glvectorgeometry::TVector BlendColor = {read=FBlendColor, write=SetBlendColor};
	__property Opengltokens::TGLboolean EnableFramebufferSRGB = {read=FEnableFramebufferSRGB, write=SetEnableFramebufferSRGB, nodefault};
	__property Opengltokens::TGLboolean EnableDither = {read=FEnableDither, write=SetEnableDither, nodefault};
	__property Opengltokens::TGLboolean EnableColorLogicOp = {read=FEnableColorLogicOp, write=SetEnableColorLogicOp, nodefault};
	__property TLogicOp LogicOpMode = {read=FLogicOpMode, write=SetLogicOpMode, nodefault};
	__property TColorMask ColorWriteMask[int Index] = {read=GetColorWriteMask, write=SetColorWriteMask};
	void __fastcall SetColorMask(TColorMask mask);
	__property Opengltokens::TGLboolean DepthWriteMask = {read=FDepthWriteMask, write=SetDepthWriteMask, nodefault};
	__property unsigned StencilWriteMask = {read=FStencilWriteMask, write=SetStencilWriteMask, nodefault};
	__property unsigned StencilBackWriteMask = {read=FStencilBackWriteMask, write=SetStencilBackWriteMask, nodefault};
	__property Glvectorgeometry::TVector ColorClearValue = {read=FColorClearValue, write=SetColorClearValue};
	__property Opengltokens::TGLfloat DepthClearValue = {read=FDepthClearValue, write=SetDepthClearValue};
	__property unsigned StencilClearValue = {read=FStencilClearValue, write=SetStencilClearValue, nodefault};
	__property unsigned DrawFrameBuffer = {read=FDrawFrameBuffer, write=SetDrawFrameBuffer, nodefault};
	__property unsigned ReadFrameBuffer = {read=FReadFrameBuffer, write=SetReadFrameBuffer, nodefault};
	void __fastcall SetFrameBuffer(const unsigned Value);
	__property unsigned RenderBuffer = {read=FRenderBuffer, write=SetRenderBuffer, nodefault};
	__property Opengltokens::TGLboolean UnpackSwapBytes = {read=FUnpackSwapBytes, write=SetUnpackSwapBytes, nodefault};
	__property Opengltokens::TGLboolean UnpackLSBFirst = {read=FUnpackLSBFirst, write=SetUnpackLSBFirst, nodefault};
	__property unsigned UnpackImageHeight = {read=FUnpackImageHeight, write=SetUnpackImageHeight, nodefault};
	__property unsigned UnpackSkipImages = {read=FUnpackSkipImages, write=SetUnpackSkipImages, nodefault};
	__property unsigned UnpackRowLength = {read=FUnpackRowLength, write=SetUnpackRowLength, nodefault};
	__property unsigned UnpackSkipRows = {read=FUnpackSkipRows, write=SetUnpackSkipRows, nodefault};
	__property unsigned UnpackSkipPixels = {read=FUnpackSkipPixels, write=SetUnpackSkipPixels, nodefault};
	__property unsigned UnpackAlignment = {read=FUnpackAlignment, write=SetUnpackAlignment, nodefault};
	__property Opengltokens::TGLboolean PackSwapBytes = {read=FPackSwapBytes, write=SetPackSwapBytes, nodefault};
	__property Opengltokens::TGLboolean PackLSBFirst = {read=FPackLSBFirst, write=SetPackLSBFirst, nodefault};
	__property unsigned PackImageHeight = {read=FPackImageHeight, write=SetPackImageHeight, nodefault};
	__property unsigned PackSkipImages = {read=FPackSkipImages, write=SetPackSkipImages, nodefault};
	__property unsigned PackRowLength = {read=FPackRowLength, write=SetPackRowLength, nodefault};
	__property unsigned PackSkipRows = {read=FPackSkipRows, write=SetPackSkipRows, nodefault};
	__property unsigned PackSkipPixels = {read=FPackSkipPixels, write=SetPackSkipPixels, nodefault};
	__property unsigned PackAlignment = {read=FPackAlignment, write=SetPackAlignment, nodefault};
	__property unsigned PixelPackBufferBinding = {read=FPixelPackBufferBinding, write=SetPixelPackBufferBinding, nodefault};
	__property unsigned PixelUnpackBufferBinding = {read=FPixelUnpackBufferBinding, write=SetPixelUnpackBufferBinding, nodefault};
	__property unsigned CurrentProgram = {read=FCurrentProgram, write=SetCurrentProgram, nodefault};
	__property unsigned MaxTextureUnits = {read=GetMaxTextureUnits, nodefault};
	__property unsigned UniformBufferBinding = {read=FUniformBufferBinding, write=SetUniformBufferBinding, nodefault};
	void __fastcall SetBufferIndexedBinding(const unsigned Value, TGLBufferBindingTarget ATarget, unsigned AIndex, Opengltokens::TGLsizeiptr ABufferSize)/* overload */;
	void __fastcall SetBufferIndexedBinding(const unsigned Value, TGLBufferBindingTarget ATarget, unsigned AIndex, Opengltokens::TGLintptr AOffset, Opengltokens::TGLsizeiptr ARangeSize)/* overload */;
	__property Glvectorgeometry::TVector CurrentVertexAttrib[int Index] = {read=GetCurrentVertexAttrib, write=SetCurrentVertexAttrib};
	__property Opengltokens::TGLboolean EnableProgramPointSize = {read=FEnableProgramPointSize, write=SetEnableProgramPointSize, nodefault};
	__property unsigned TransformFeedbackBufferBinding = {read=FTransformFeedbackBufferBinding, write=SetTransformFeedbackBufferBinding, nodefault};
	__property THintType LineSmoothHint = {read=FLineSmoothHint, write=SetLineSmoothHint, nodefault};
	__property THintType PolygonSmoothHint = {read=FPolygonSmoothHint, write=SetPolygonSmoothHint, nodefault};
	__property THintType TextureCompressionHint = {read=FTextureCompressionHint, write=SetTextureCompressionHint, nodefault};
	__property THintType FragmentShaderDerivitiveHint = {read=FFragmentShaderDerivitiveHint, write=SetFragmentShaderDerivitiveHint, nodefault};
	__property THintType MultisampleFilterHint = {read=FMultisampleFilterHint, write=SetMultisampleFilterHint, nodefault};
	__property unsigned CurrentQuery[TQueryType Index] = {read=GetCurrentQuery};
	void __fastcall BeginQuery(const TQueryType target, const unsigned Value);
	void __fastcall EndQuery(const TQueryType target);
	__property unsigned CopyReadBufferBinding = {read=FCopyReadBufferBinding, write=SetCopyReadBufferBinding, nodefault};
	__property unsigned CopyWriteBufferBinding = {read=FCopyWriteBufferBinding, write=SetCopyWriteBufferBinding, nodefault};
	__property Opengltokens::TGLboolean EnableTextureCubeMapSeamless = {read=FEnableTextureCubeMapSeamless, write=SetEnableTextureCubeMapSeamless, nodefault};
	__property bool InsideList = {read=FInsideList, nodefault};
	void __fastcall NewList(unsigned list, unsigned mode);
	void __fastcall EndList();
	void __fastcall CallList(unsigned list);
	void __fastcall SetGLTextureMatrix(const Glvectorgeometry::TMatrix &matrix);
	void __fastcall ResetGLTextureMatrix();
	void __fastcall ResetAllGLTextureMatrix();
	void __fastcall SetGLColorWriting(bool flag);
	void __fastcall InvertGLFrontFace();
	__property TGLStates States = {read=FStates, nodefault};
};


struct DECLSPEC_DRECORD TStateRecord
{
public:
	unsigned GLConst;
	bool GLDeprecated;
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Int8 GLS_VERTEX_ATTR_NUM = System::Int8(0x10);
#define cAllAttribBits (System::Set<TGLStateType, TGLStateType::sttCurrent, TGLStateType::sttMultisample>() << TGLStateType::sttCurrent << TGLStateType::sttPoint << TGLStateType::sttLine << TGLStateType::sttPolygon << TGLStateType::sttPolygonStipple << TGLStateType::sttPixelMode << TGLStateType::sttLighting << TGLStateType::sttFog << TGLStateType::sttDepthBuffer << TGLStateType::sttAccumBuffer << TGLStateType::sttStencilBuffer << TGLStateType::sttViewport << TGLStateType::sttTransform << TGLStateType::sttEnable << TGLStateType::sttColorBuffer << TGLStateType::sttHint << TGLStateType::sttEval << TGLStateType::sttList << TGLStateType::sttTexture << TGLStateType::sttScissor << TGLStateType::sttMultisample )
#define cAllMeshPrimitive (System::Set<TGLMeshPrimitive, TGLMeshPrimitive::mpNOPRIMITIVE, TGLMeshPrimitive::mpPATCHES>() << TGLMeshPrimitive::mpTRIANGLES << TGLMeshPrimitive::mpTRIANGLE_STRIP << TGLMeshPrimitive::mpTRIANGLE_FAN << TGLMeshPrimitive::mpPOINTS << TGLMeshPrimitive::mpLINES << TGLMeshPrimitive::mpLINE_LOOP << TGLMeshPrimitive::mpLINE_STRIP << TGLMeshPrimitive::mpLINES_ADJACENCY << TGLMeshPrimitive::mpLINE_STRIP_ADJACENCY << TGLMeshPrimitive::mpTRIANGLES_ADJACENCY << TGLMeshPrimitive::mpTRIANGLE_STRIP_ADJACENCY << TGLMeshPrimitive::mpPATCHES )
#define cAllColorComponents (System::Set<TColorComponent, TColorComponent::ccRed, TColorComponent::ccAlpha>() << TColorComponent::ccRed << TColorComponent::ccGreen << TColorComponent::ccBlue << TColorComponent::ccAlpha )
static _DELPHI_CONST System::Int8 MAX_HARDWARE_LIGHT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 MAX_SHADER_LIGHT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 MAX_HARDWARE_TEXTURE_UNIT = System::Int8(0x30);
static _DELPHI_CONST System::Int8 MAX_HARDWARE_UNIFORM_BUFFER_BINDING = System::Int8(0x4b);
extern DELPHI_PACKAGE System::StaticArray<unsigned, 21> cGLStateTypeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<TStateRecord, 24> cGLStateToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 12> cGLTexTypeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 5> cGLQueryTypeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 8> cGLStencilOpToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 16> cGLLogicOpToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 8> cGLComparisonFunctionToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 15> cGLBlendFunctionToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 5> cGLBlendEquationToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 2> cGLFaceWindingToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 3> cGLPolygonModeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 3> cGLCullFaceModeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 3> cGLHintToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 2> cGLBufferBindingTarget;
}	/* namespace Glstate */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSTATE)
using namespace Glstate;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLStateHPP
