// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBumpMapping.pas' rev: 35.00 (Windows)

#ifndef GlbumpmappingHPP
#define GlbumpmappingHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <GLColor.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbumpmapping
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNormalMapSpace : unsigned char { nmsObject, nmsTangent };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall CalcObjectSpaceLightVectors(const Glvectortypes::TVector3f &Light, Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TVectorList* Colors);
extern DELPHI_PACKAGE void __fastcall SetupTangentSpace(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TAffineVectorList* Normals, Glvectorlists::TAffineVectorList* TexCoords, Glvectorlists::TAffineVectorList* Tangents, Glvectorlists::TAffineVectorList* BiNormals);
extern DELPHI_PACKAGE void __fastcall CalcTangentSpaceLightVectors(const Glvectortypes::TVector3f &Light, Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TAffineVectorList* Normals, Glvectorlists::TAffineVectorList* Tangents, Glvectorlists::TAffineVectorList* BiNormals, Glvectorlists::TVectorList* Colors);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateObjectSpaceNormalMap(int Width, int Height, Glvectorlists::TAffineVectorList* HiNormals, Glvectorlists::TAffineVectorList* HiTexCoords);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateTangentSpaceNormalMap(int Width, int Height, Glvectorlists::TAffineVectorList* HiNormals, Glvectorlists::TAffineVectorList* HiTexCoords, Glvectorlists::TAffineVectorList* LoNormals, Glvectorlists::TAffineVectorList* LoTexCoords, Glvectorlists::TAffineVectorList* Tangents, Glvectorlists::TAffineVectorList* BiNormals);
}	/* namespace Glbumpmapping */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBUMPMAPPING)
using namespace Glbumpmapping;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbumpmappingHPP
