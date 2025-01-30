// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDCEMisc.pas' rev: 36.00 (Windows)

#ifndef GLDCEMiscHPP
#define GLDCEMiscHPP

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
#include <GLScene.hpp>
#include <GLCoordinates.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLEllipseCollision.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLTerrainRenderer.hpp>
#include <GLProxyObjects.hpp>
#include <GLMultiProxy.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gldcemisc
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<Glvectortypes::TVector3f, 36> DCEBox;
extern DELPHI_PACKAGE void __fastcall ECSetCollisionRange(Glellipsecollision::TECMovePack &MovePack);
extern DELPHI_PACKAGE void __fastcall ECResetColliders(Glellipsecollision::TECMovePack &MovePack);
extern DELPHI_PACKAGE void __fastcall ECAddFreeForm(Glellipsecollision::TECMovePack &MovePack, Glscene::TGLBaseSceneObject* FreeForm, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddBox(Glellipsecollision::TECMovePack &MovePack, Glscene::TGLBaseSceneObject* BoxObj, const Glvectorgeometry::TAffineVector &BoxSize, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddTerrain(Glellipsecollision::TECMovePack &MovePack, Glterrainrenderer::TGLTerrainRenderer* TerrainRenderer, float Resolution, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddEllipsoid(Glellipsecollision::TECMovePack &MovePack, const Glvectorgeometry::TAffineVector &ePos, const Glvectorgeometry::TAffineVector &eRadius, bool Solid, int ObjectID);
}	/* namespace Gldcemisc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDCEMISC)
using namespace Gldcemisc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLDCEMiscHPP
