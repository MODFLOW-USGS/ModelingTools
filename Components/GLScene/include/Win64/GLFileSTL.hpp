// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilestl.pas' rev: 36.00 (Windows)

#ifndef GlfilestlHPP
#define GlfilestlHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glapplicationfileio.hpp>
#include <Glutils.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilestl
{
//-- forward type declarations -----------------------------------------------
struct TSTLHeader;
struct TSTLFace;
class DELPHICLASS TGLSTLVectorFile;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TSTLHeader
{
public:
	System::StaticArray<System::Byte, 80> dummy;
	System::LongInt nbFaces;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TSTLFace
{
public:
	Glvectorgeometry::TAffineVector normal;
	Glvectorgeometry::TAffineVector v1;
	Glvectorgeometry::TAffineVector v2;
	Glvectorgeometry::TAffineVector v3;
	System::StaticArray<System::Byte, 2> padding;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TGLSTLVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLSTLVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLSTLVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilestl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILESTL)
using namespace Glfilestl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilestlHPP
