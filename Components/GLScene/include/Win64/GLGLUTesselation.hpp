// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glglutesselation.pas' rev: 36.00 (Windows)

#ifndef GlglutesselationHPP
#define GlglutesselationHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Sysutils.hpp>
#include <Opengladapter.hpp>
#include <Opengltokens.hpp>
#include <Glpersistentclasses.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glvectorlists.hpp>
#include <Glvectorgeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glglutesselation
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall DoTesselate(Glvectorlists::TAffineVectorList* Vertexes, Glvectorfileobjects::TGLBaseMesh* Mesh, Glvectorgeometry::PAffineVector normal = (Glvectorgeometry::PAffineVector)(0x0), bool invertNormals = false);
}	/* namespace Glglutesselation */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGLUTESSELATION)
using namespace Glglutesselation;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlglutesselationHPP
