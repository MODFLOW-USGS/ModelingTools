// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSpatialPartitioning.pas' rev: 36.00 (Windows)

#ifndef GlspatialpartitioningHPP
#define GlspatialpartitioningHPP

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
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>
#include <GLContext.hpp>
#include <GLWin32Viewer.hpp>
#include <GLSpacePartition.hpp>
#include <GLGeometryBB.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLPersistentClasses.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glspatialpartitioning
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSceneObj;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSceneObj : public Glspacepartition::TSpacePartitionLeaf
{
	typedef Glspacepartition::TSpacePartitionLeaf inherited;
	
public:
	Glscene::TGLBaseSceneObject* Obj;
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__fastcall TSceneObj(Glspacepartition::TSectoredSpacePartition* Owner, Glscene::TGLBaseSceneObject* aObj);
	__fastcall virtual ~TSceneObj();
public:
	/* TSpacePartitionLeaf.CreateOwned */ inline __fastcall TSceneObj(Glspacepartition::TBaseSpacePartition* SpacePartition) : Glspacepartition::TSpacePartitionLeaf(SpacePartition) { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TSceneObj() : Glspacepartition::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSceneObj(Glpersistentclasses::TVirtualReader* reader) : Glspacepartition::TSpacePartitionLeaf(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall RenderAABB(Glrendercontextinfo::TGLRenderContextInfo &rci, const Glgeometrybb::TAABB &AABB)/* overload */;
extern DELPHI_PACKAGE void __fastcall RenderAABB(Glrendercontextinfo::TGLRenderContextInfo &rci, const Glgeometrybb::TAABB &AABB, float w, float r, float g, float b)/* overload */;
extern DELPHI_PACKAGE void __fastcall RenderSpatialPartitioning(Glrendercontextinfo::TGLRenderContextInfo &rci, Glspacepartition::TSectoredSpacePartition* const Space);
extern DELPHI_PACKAGE Glspacepartition::TExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Glvectorgeometry::TFrustum &AFrustum, Glwin32viewer::TGLSceneViewer* const AGLSceneViewer)/* overload */;
extern DELPHI_PACKAGE Glspacepartition::TExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Glvectorgeometry::TFrustum &AFrustum, const int vWidth, const int vHeight, Glscene::TGLCamera* AGLCamera)/* overload */;
}	/* namespace Glspatialpartitioning */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSPATIALPARTITIONING)
using namespace Glspatialpartitioning;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlspatialpartitioningHPP
