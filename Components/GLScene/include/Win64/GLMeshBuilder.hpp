// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshBuilder.pas' rev: 34.00 (Windows)

#ifndef GlmeshbuilderHPP
#define GlmeshbuilderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmeshbuilder
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall BuildCube(Glvectorfileobjects::TMeshObject* Mesh, const Glvectortypes::TVector3f &Position, const Glvectortypes::TVector3f &Scale);
extern DELPHI_PACKAGE void __fastcall BuildCylinder(Glvectorfileobjects::TMeshObject* Mesh, const Glvectortypes::TVector3f &Position, const Glvectortypes::TVector3f &Scale, int Slices);
extern DELPHI_PACKAGE void __fastcall BuildCylinder2(Glvectorfileobjects::TMeshObject* Mesh, const Glvectortypes::TVector3f &Position, const Glvectortypes::TVector3f &Scale, float TopRadius, float BottomRadius, float Height, int Slices);
}	/* namespace Glmeshbuilder */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHBUILDER)
using namespace Glmeshbuilder;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshbuilderHPP
