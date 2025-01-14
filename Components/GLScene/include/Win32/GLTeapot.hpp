// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTeapot.pas' rev: 36.00 (Windows)

#ifndef GlteapotHPP
#define GlteapotHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
#include <GLState.hpp>
#include <GLVectorGeometry.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glteapot
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTeapot;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTeapot : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	unsigned FGrid;
	
public:
	__fastcall virtual TGLTeapot(System::Classes::TComponent* AOwner);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTeapot() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTeapot(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glteapot */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEAPOT)
using namespace Glteapot;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlteapotHPP
