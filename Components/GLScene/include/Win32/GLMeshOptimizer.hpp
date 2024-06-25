// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshOptimizer.pas' rev: 36.00 (Windows)

#ifndef GlmeshoptimizerHPP
#define GlmeshoptimizerHPP

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
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorFileObjects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmeshoptimizer
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLMeshOptimizerOption : unsigned char { mooStandardize, mooVertexCache, mooSortByMaterials, mooMergeObjects };

typedef System::Set<TGLMeshOptimizerOption, TGLMeshOptimizerOption::mooStandardize, TGLMeshOptimizerOption::mooMergeObjects> TGLMeshOptimizerOptions;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLMeshOptimizerOptions vDefaultMeshOptimizerOptions;
extern DELPHI_PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TGLMeshObjectList* aList)/* overload */;
extern DELPHI_PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TGLMeshObjectList* aList, TGLMeshOptimizerOptions options)/* overload */;
extern DELPHI_PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObject* aMeshObject)/* overload */;
extern DELPHI_PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObject* aMeshObject, TGLMeshOptimizerOptions options)/* overload */;
extern DELPHI_PACKAGE void __fastcall FacesSmooth(Glvectorfileobjects::TMeshObject* aMeshObj, float aWeldDistance = 1.000000E-07f, float aThreshold = 3.500000E+01f, bool InvertNormals = false);
}	/* namespace Glmeshoptimizer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHOPTIMIZER)
using namespace Glmeshoptimizer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshoptimizerHPP
