﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPipelineTransformation.pas' rev: 35.00 (Windows)

#ifndef GlpipelinetransformationHPP
#define GlpipelinetransformationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <OpenGLTokens.hpp>
#include <OpenGLAdapter.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLSLog.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glpipelinetransformation
{
//-- forward type declarations -----------------------------------------------
struct TTransformationRec;
class DELPHICLASS TGLTransformation;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLPipelineTransformationState : unsigned char { trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged, trsViewProjChanged, trsFrustum };

typedef System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum> TGLPipelineTransformationStates;

typedef TTransformationRec *PTransformationRec;

struct DECLSPEC_DRECORD TTransformationRec
{
public:
	TGLPipelineTransformationStates FStates;
	Glvectortypes::TMatrix4f FModelMatrix;
	Glvectortypes::TMatrix4f FViewMatrix;
	Glvectortypes::TMatrix4f FProjectionMatrix;
	Glvectortypes::TMatrix4f FInvModelMatrix;
	Glvectortypes::TMatrix3f FNormalModelMatrix;
	Glvectortypes::TMatrix4f FModelViewMatrix;
	Glvectortypes::TMatrix4f FInvModelViewMatrix;
	Glvectortypes::TMatrix4f FViewProjectionMatrix;
	Glvectorgeometry::TFrustum FFrustum;
};


typedef void __fastcall (__closure *TOnMatricesPush)(void);

class PASCALIMPLEMENTATION TGLTransformation : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TTransformationRec> _TGLTransformation__1;
	
	
private:
	int FStackPos;
	_TGLTransformation__1 FStack;
	bool FLoadMatricesEnabled;
	TOnMatricesPush FOnPush;
	Glvectorgeometry::PMatrix __fastcall GetModelMatrix();
	Glvectorgeometry::PMatrix __fastcall GetViewMatrix();
	Glvectorgeometry::PMatrix __fastcall GetProjectionMatrix();
	Glvectorgeometry::PMatrix __fastcall GetModelViewMatrix();
	Glvectorgeometry::PMatrix __fastcall GetInvModelViewMatrix();
	Glvectorgeometry::PMatrix __fastcall GetInvModelMatrix();
	Glvectorgeometry::PAffineMatrix __fastcall GetNormalModelMatrix();
	Glvectorgeometry::PMatrix __fastcall GetViewProjectionMatrix();
	Glvectorgeometry::TFrustum __fastcall GetFrustum();
	
protected:
	void __fastcall LoadModelViewMatrix();
	void __fastcall LoadProjectionMatrix();
	void __fastcall DoMatricesLoaded();
	__property TOnMatricesPush OnPush = {read=FOnPush, write=FOnPush};
	
public:
	__fastcall TGLTransformation();
	void __fastcall SetModelMatrix(const Glvectortypes::TMatrix4f &AMatrix);
	void __fastcall SetViewMatrix(const Glvectortypes::TMatrix4f &AMatrix);
	void __fastcall SetProjectionMatrix(const Glvectortypes::TMatrix4f &AMatrix);
	void __fastcall IdentityAll();
	void __fastcall Push(PTransformationRec AValue)/* overload */;
	void __fastcall Push()/* overload */;
	void __fastcall Pop();
	void __fastcall ReplaceFromStack();
	TTransformationRec __fastcall StackTop();
	__property Glvectorgeometry::PMatrix ModelMatrix = {read=GetModelMatrix};
	__property Glvectorgeometry::PMatrix ViewMatrix = {read=GetViewMatrix};
	__property Glvectorgeometry::PMatrix ProjectionMatrix = {read=GetProjectionMatrix};
	__property Glvectorgeometry::PMatrix InvModelMatrix = {read=GetInvModelMatrix};
	__property Glvectorgeometry::PMatrix ModelViewMatrix = {read=GetModelViewMatrix};
	__property Glvectorgeometry::PAffineMatrix NormalModelMatrix = {read=GetNormalModelMatrix};
	__property Glvectorgeometry::PMatrix InvModelViewMatrix = {read=GetInvModelViewMatrix};
	__property Glvectorgeometry::PMatrix ViewProjectionMatrix = {read=GetViewProjectionMatrix};
	__property Glvectorgeometry::TFrustum Frustum = {read=GetFrustum};
	__property bool LoadMatricesEnabled = {read=FLoadMatricesEnabled, write=FLoadMatricesEnabled, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLTransformation() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte MAX_MATRIX_STACK_DEPTH = System::Byte(0x80);
#define cAllStatesChanged (System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum>() << TGLPipelineTransformationState::trsModelViewChanged << TGLPipelineTransformationState::trsInvModelViewChanged << TGLPipelineTransformationState::trsInvModelChanged << TGLPipelineTransformationState::trsNormalModelChanged << TGLPipelineTransformationState::trsViewProjChanged << TGLPipelineTransformationState::trsFrustum )
}	/* namespace Glpipelinetransformation */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPIPELINETRANSFORMATION)
using namespace Glpipelinetransformation;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlpipelinetransformationHPP