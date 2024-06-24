// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilelwo.pas' rev: 36.00 (Windows)

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
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.Math.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glvectorlists.hpp>
#include <Lwobjects.hpp>
#include <Glapplicationfileio.hpp>
#include <Glbaseclasses.hpp>

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
