// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileLWO.pas' rev: 36.00 (Windows)

#ifndef GlfilelwoHPP
#define GlfilelwoHPP

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
#include <System.Math.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorLists.hpp>
#include <LWObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilelwo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLWOVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLWOVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	Lwobjects::TLWObjectFile* FLWO;
	Lwobjects::TLWPnts* FPnts;
	void __fastcall AddLayr(Lwobjects::TLWLayr* Layr, Lwobjects::TLWObjectFile* LWO);
	void __fastcall AddSurf(Lwobjects::TLWSurf* Surf, Lwobjects::TLWObjectFile* LWO);
	void __fastcall AddPnts(Lwobjects::TLWPnts* Pnts, Glvectorfileobjects::TMeshObject* Mesh);
	void __fastcall AddPols(Lwobjects::TLWPols* Pols, Glvectorfileobjects::TMeshObject* Mesh);
	void __fastcall AddVMap(Lwobjects::TLWVMap* VMap, Glvectorfileobjects::TMeshObject* Mesh);
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLLWOVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLWOVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilelwo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILELWO)
using namespace Glfilelwo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilelwoHPP
