// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBaseMeshSilhouette.pas' rev: 36.00 (Windows)

#ifndef GlbasemeshsilhouetteHPP
#define GlbasemeshsilhouetteHPP

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
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLSilhouette.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbasemeshsilhouette
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFaceGroupConnectivity;
class DELPHICLASS TGLBaseMeshConnectivity;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLFaceGroupConnectivity : public Glsilhouette::TConnectivity
{
	typedef Glsilhouette::TConnectivity inherited;
	
private:
	Glvectorfileobjects::TMeshObject* FMeshObject;
	bool FOwnsVertices;
	void __fastcall SetMeshObject(Glvectorfileobjects::TMeshObject* const Value);
	
public:
	virtual void __fastcall Clear();
	void __fastcall RebuildEdgeList();
	__property Glvectorfileobjects::TMeshObject* MeshObject = {read=FMeshObject, write=SetMeshObject};
	__fastcall virtual TGLFaceGroupConnectivity(bool APrecomputeFaceNormal);
	__fastcall TGLFaceGroupConnectivity(Glvectorfileobjects::TMeshObject* aMeshObject, bool APrecomputeFaceNormal);
	__fastcall virtual ~TGLFaceGroupConnectivity();
};


class PASCALIMPLEMENTATION TGLBaseMeshConnectivity : public Glsilhouette::TBaseConnectivity
{
	typedef Glsilhouette::TBaseConnectivity inherited;
	
private:
	Glvectorfileobjects::TGLBaseMesh* FBaseMesh;
	System::Classes::TList* FFaceGroupConnectivityList;
	TGLFaceGroupConnectivity* __fastcall GetFaceGroupConnectivity(int i);
	int __fastcall GetConnectivityCount();
	void __fastcall SetBaseMesh(Glvectorfileobjects::TGLBaseMesh* const Value);
	
protected:
	virtual int __fastcall GetEdgeCount();
	virtual int __fastcall GetFaceCount();
	
public:
	__property int ConnectivityCount = {read=GetConnectivityCount, nodefault};
	__property TGLFaceGroupConnectivity* FaceGroupConnectivity[int i] = {read=GetFaceGroupConnectivity};
	__property Glvectorfileobjects::TGLBaseMesh* BaseMesh = {read=FBaseMesh, write=SetBaseMesh};
	void __fastcall Clear(bool SaveFaceGroupConnectivity);
	void __fastcall RebuildEdgeList();
	HIDESBASE void __fastcall CreateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters, Glsilhouette::TGLSilhouette* &aSilhouette, bool AddToSilhouette);
	__fastcall virtual TGLBaseMeshConnectivity(bool APrecomputeFaceNormal);
	__fastcall TGLBaseMeshConnectivity(Glvectorfileobjects::TGLBaseMesh* aBaseMesh);
	__fastcall virtual ~TGLBaseMeshConnectivity();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbasemeshsilhouette */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBASEMESHSILHOUETTE)
using namespace Glbasemeshsilhouette;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbasemeshsilhouetteHPP
