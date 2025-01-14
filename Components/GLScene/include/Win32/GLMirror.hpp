// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMirror.pas' rev: 36.00 (Windows)

#ifndef GlmirrorHPP
#define GlmirrorHPP

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
#include <OpenGLAdapter.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLMaterial.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLVectorTypes.hpp>
#include <GLPersistentClasses.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLXCollection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmirror
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMirror;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLMirrorOption : unsigned char { moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer };

typedef System::Set<TGLMirrorOption, TGLMirrorOption::moUseStencil, TGLMirrorOption::moClearZBuffer> TGLMirrorOptions;

enum DECLSPEC_DENUM TMirrorShapes : unsigned char { msRect, msDisk };

class PASCALIMPLEMENTATION TGLMirror : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	bool FRendering;
	Glscene::TGLBaseSceneObject* FMirrorObject;
	float FWidth;
	float FHeight;
	TGLMirrorOptions FMirrorOptions;
	System::Classes::TNotifyEvent FOnBeginRenderingMirrors;
	System::Classes::TNotifyEvent FOnEndRenderingMirrors;
	TMirrorShapes FShape;
	float FRadius;
	int FSlices;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetMirrorObject(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetMirrorOptions(const TGLMirrorOptions val);
	void __fastcall ClearZBufferArea(Glscene::TGLSceneBuffer* aBuffer);
	void __fastcall SetHeight(float AValue);
	void __fastcall SetWidth(float AValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(const int aValue);
	void __fastcall SetShape(TMirrorShapes aValue);
	float __fastcall GetRadius();
	int __fastcall GetSlices();
	
public:
	__fastcall virtual TGLMirror(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	
__published:
	__property Glscene::TGLBaseSceneObject* MirrorObject = {read=FMirrorObject, write=SetMirrorObject};
	__property TGLMirrorOptions MirrorOptions = {read=FMirrorOptions, write=SetMirrorOptions, default=1};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Width = {read=FWidth, write=SetWidth};
	__property System::Classes::TNotifyEvent OnBeginRenderingMirrors = {read=FOnBeginRenderingMirrors, write=FOnBeginRenderingMirrors};
	__property System::Classes::TNotifyEvent OnEndRenderingMirrors = {read=FOnEndRenderingMirrors, write=FOnEndRenderingMirrors};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property TMirrorShapes Shape = {read=FShape, write=SetShape, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLMirror() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMirror(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultMirrorOptions (System::Set<TGLMirrorOption, TGLMirrorOption::moUseStencil, TGLMirrorOption::moClearZBuffer>() << TGLMirrorOption::moUseStencil )
}	/* namespace Glmirror */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMIRROR)
using namespace Glmirror;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmirrorHPP
