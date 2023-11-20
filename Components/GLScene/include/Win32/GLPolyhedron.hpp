// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPolyhedron.pas' rev: 35.00 (Windows)

#ifndef GlpolyhedronHPP
#define GlpolyhedronHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLObjects.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLMesh.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glpolyhedron
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTetrahedron;
class DELPHICLASS TGLOctahedron;
class DELPHICLASS TGLHexahedron;
class DELPHICLASS TGLDodecahedron;
class DELPHICLASS TGLIcosahedron;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTetrahedron : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLTetrahedron(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLTetrahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTetrahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLOctahedron : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLOctahedron(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLOctahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLOctahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLHexahedron : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLHexahedron(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLHexahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHexahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLDodecahedron : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLDodecahedron(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLDodecahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDodecahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLIcosahedron : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TGLBaseMesh.Create */ inline __fastcall virtual TGLIcosahedron(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLBaseMesh(AOwner) { }
	/* TGLBaseMesh.Destroy */ inline __fastcall virtual ~TGLIcosahedron() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLIcosahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glpolyhedron */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPOLYHEDRON)
using namespace Glpolyhedron;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlpolyhedronHPP
