// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilevrml.pas' rev: 36.00 (Windows)

#ifndef GlfilevrmlHPP
#define GlfilevrmlHPP

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
#include <Glmaterial.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Vrmlparser.hpp>
#include <Glmeshutils.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilevrml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLVRMLVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLVRMLVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLVRMLVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLVRMLVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilevrml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEVRML)
using namespace Glfilevrml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilevrmlHPP
