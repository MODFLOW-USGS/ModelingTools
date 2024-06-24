// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilemd3.pas' rev: 36.00 (Windows)

#ifndef Glfilemd3HPP
#define Glfilemd3HPP

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
#include <Glvectorfileobjects.hpp>
#include <Glmaterial.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectorgeometry.hpp>
#include <Filemd3.hpp>
#include <Gltexture.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilemd3
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMD3VectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMD3VectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLMD3VectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMD3VectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilemd3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEMD3)
using namespace Glfilemd3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilemd3HPP
