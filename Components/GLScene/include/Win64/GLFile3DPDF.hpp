// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfile3dpdf.pas' rev: 36.00 (Windows)

#ifndef Glfile3dpdfHPP
#define Glfile3dpdfHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Shellapi.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.Strutils.hpp>
#include <Glvectortypes.hpp>
#include <Glpersistentclasses.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glapplicationfileio.hpp>
#include <Glutils.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfile3dpdf
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLIDTFVectorFile;
class DELPHICLASS TGLU3DVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLIDTFVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	void __fastcall BuildNormals(Glvectorfileobjects::TMeshObject* m);
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLIDTFVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLIDTFVectorFile() { }
	
};


class PASCALIMPLEMENTATION TGLU3DVectorFile : public TGLIDTFVectorFile
{
	typedef TGLIDTFVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLU3DVectorFile(System::Classes::TPersistent* AOwner) : TGLIDTFVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLU3DVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString IDTFConverterFileName;
}	/* namespace Glfile3dpdf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILE3DPDF)
using namespace Glfile3dpdf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfile3dpdfHPP
