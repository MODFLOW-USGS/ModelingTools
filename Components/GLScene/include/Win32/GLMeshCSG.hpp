// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshCSG.pas' rev: 36.00 (Windows)

#ifndef GlmeshcsgHPP
#define GlmeshcsgHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <GLScene.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorGeometry.hpp>
#include <GLBSP.hpp>
#include <GLVectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmeshcsg
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCSGOperation : unsigned char { CSG_Union, CSG_Subtraction, CSG_Intersection };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall CSG_Operation(Glvectorfileobjects::TMeshObject* obj1, Glvectorfileobjects::TMeshObject* obj2, TCSGOperation Operation, Glvectorfileobjects::TMeshObject* Res, const System::UnicodeString MaterialName1, const System::UnicodeString MaterialName2);
}	/* namespace Glmeshcsg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHCSG)
using namespace Glmeshcsg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshcsgHPP
