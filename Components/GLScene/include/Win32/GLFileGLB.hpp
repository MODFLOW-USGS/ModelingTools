// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileGLB.pas' rev: 35.00 (Windows)

#ifndef GlfileglbHPP
#define GlfileglbHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLTexture.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorGeometry.hpp>
#include <GLMaterial.hpp>
#include <GLUtils.hpp>
#include <GLPersistentClasses.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfileglb
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLBVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLBVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfileglb */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEGLB)
using namespace Glfileglb;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileglbHPP
