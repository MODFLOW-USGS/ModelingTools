// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfiletin.pas' rev: 36.00 (Windows)

#ifndef GlfiletinHPP
#define GlfiletinHPP

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
#include <Glvectorfileobjects.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectorgeometry.hpp>
#include <Glutils.hpp>
#include <Gltypes.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfiletin
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTINVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTINVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLTINVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTINVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfiletin */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILETIN)
using namespace Glfiletin;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfiletinHPP
