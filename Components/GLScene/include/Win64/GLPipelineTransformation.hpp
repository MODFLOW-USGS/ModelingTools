// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glpipelinetransformation.pas' rev: 36.00 (Windows)

#ifndef GlpipelinetransformationHPP
#define GlpipelinetransformationHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Opengltokens.hpp>
#include <Opengladapter.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectortypes.hpp>
#include <Glslog.hpp>

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
	Glvectorgeometry::TMatrix FModelMatrix;
	Glvectorgeometry::TMatrix FViewMatrix;
	Glvectorgeometry::TMatrix FProjectionMatrix;
	Glvectorgeometry::TMatrix FInvModelMatrix;
	Glvectorgeometry::TAffineMatrix FNormalModelMatrix;
	Glvectorgeometry::TMatrix FModelViewMatrix;
	Glvectorgeometry::TMatrix FInvModelViewMatrix;
	Glvectorgeometry::TMatrix FViewProjectionMatrix;
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
	void __fastcall SetModelMatrix(const Glvectorgeometry::TMatrix &AMatrix);
	void __fastcall SetViewMatrix(const Glvectorgeometry::TMatrix &AMatrix);
	void __fastcall SetProjectionMatrix(const Glvectorgeometry::TMatrix &AMatrix);
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
static _DELPHI_CONST System::Byte MAX_MATRIX_STACK_DEPTH = System::Byte(0x80);
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
