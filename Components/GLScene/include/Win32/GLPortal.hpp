// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPortal.pas' rev: 36.00 (Windows)

#ifndef GlportalHPP
#define GlportalHPP

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
#include <GLPersistentClasses.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLScene.hpp>
#include <GLMaterial.hpp>
#include <GLVectorGeometry.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glportal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPortalMeshObjectList;
class DELPHICLASS TSectorMeshObject;
class DELPHICLASS TFGPolygon;
class DELPHICLASS TFGPortalPolygon;
class DELPHICLASS TGLPortal;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPortalMeshObjectList : public Glvectorfileobjects::TGLMeshObjectList
{
	typedef Glvectorfileobjects::TGLMeshObjectList inherited;
	
public:
	__fastcall TPortalMeshObjectList(Glvectorfileobjects::TGLBaseMesh* AOwner);
	__fastcall virtual ~TPortalMeshObjectList();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TPortalMeshObjectList() : Glvectorfileobjects::TGLMeshObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TPortalMeshObjectList(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TGLMeshObjectList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectorMeshObject : public Glvectorfileobjects::TGLMorphableMeshObject
{
	typedef Glvectorfileobjects::TGLMorphableMeshObject inherited;
	
private:
	bool FRenderDone;
	
public:
	__fastcall TSectorMeshObject(Glvectorfileobjects::TGLMeshObjectList* AOwner);
	__fastcall virtual ~TSectorMeshObject();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	__property bool RenderDone = {read=FRenderDone, write=FRenderDone, nodefault};
public:
	/* TGLMorphableMeshObject.Create */ inline __fastcall virtual TSectorMeshObject() : Glvectorfileobjects::TGLMorphableMeshObject() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSectorMeshObject(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TGLMorphableMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGPolygon : public Glvectorfileobjects::TFGVertexNormalTexIndexList
{
	typedef Glvectorfileobjects::TFGVertexNormalTexIndexList inherited;
	
public:
	__fastcall virtual TFGPolygon(Glvectorfileobjects::TGLFaceGroups* AOwner);
	__fastcall virtual ~TFGPolygon();
	virtual void __fastcall Prepare();
public:
	/* TFGVertexNormalTexIndexList.Create */ inline __fastcall virtual TFGPolygon() : Glvectorfileobjects::TFGVertexNormalTexIndexList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGPolygon(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TFGVertexNormalTexIndexList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGPortalPolygon : public TFGPolygon
{
	typedef TFGPolygon inherited;
	
private:
	int FDestinationSectorIndex;
	Glvectorgeometry::TAffineVector FCenter;
	Glvectorgeometry::TAffineVector FNormal;
	float FRadius;
	
public:
	__fastcall virtual TFGPortalPolygon(Glvectorfileobjects::TGLFaceGroups* AOwner);
	__fastcall virtual ~TFGPortalPolygon();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	__property int DestinationSectorIndex = {read=FDestinationSectorIndex, write=FDestinationSectorIndex, nodefault};
public:
	/* TFGVertexNormalTexIndexList.Create */ inline __fastcall virtual TFGPortalPolygon() : TFGPolygon() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGPortalPolygon(Glpersistentclasses::TVirtualReader* reader) : TFGPolygon(reader) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLPortal : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	__fastcall virtual TGLPortal(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPortal();
	
__published:
	__property MaterialLibrary;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPortal(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glportal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPORTAL)
using namespace Glportal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlportalHPP
