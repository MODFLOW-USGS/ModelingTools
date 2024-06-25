// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileDXF.pas' rev: 36.00 (Windows)

#ifndef GlfiledxfHPP
#define GlfiledxfHPP

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
#include <GLPersistentClasses.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLScene.hpp>
#include <GLTexture.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfiledxf
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDXFVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDXFVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	System::UnicodeString FBuffer;
	int FLineNo;
	bool FEof;
	int FBufPos;
	bool HasPushedCode;
	int PushedCode;
	System::Classes::TStringList* FLayers;
	System::Classes::TStringList* FBlocks;
	System::Byte FLastpercentdone;
	
protected:
	void __fastcall PushCode(int code);
	int __fastcall GetCode();
	void __fastcall SkipTable();
	void __fastcall SkipSection();
	Glvectorfileobjects::TMeshObject* __fastcall NeedMesh(Glvectorfileobjects::TGLBaseMesh* basemesh, System::UnicodeString layer);
	Glvectorfileobjects::TFGVertexIndexList* __fastcall NeedFaceGroup(Glvectorfileobjects::TMeshObject* m, Glvectorfileobjects::TGLFaceGroupMeshMode fgmode, System::UnicodeString fgmat);
	void __fastcall NeedMeshAndFaceGroup(Glvectorfileobjects::TGLBaseMesh* basemesh, System::UnicodeString layer, Glvectorfileobjects::TGLFaceGroupMeshMode fgmode, System::UnicodeString fgmat, Glvectorfileobjects::TMeshObject* &m, Glvectorfileobjects::TFGVertexIndexList* &fg);
	System::UnicodeString __fastcall ReadLine();
	int __fastcall ReadInt();
	double __fastcall ReadDouble();
	void __fastcall ReadTables();
	void __fastcall ReadLayer();
	void __fastcall ReadLayerTable();
	void __fastcall ReadBlocks();
	void __fastcall ReadInsert(Glvectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntity3Dface(Glvectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntityPolyLine(Glvectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntities(Glvectorfileobjects::TGLBaseMesh* basemesh);
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLDXFVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDXFVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfiledxf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEDXF)
using namespace Glfiledxf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfiledxfHPP
