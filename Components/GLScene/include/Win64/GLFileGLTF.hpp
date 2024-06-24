// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilegltf.pas' rev: 36.00 (Windows)

#ifndef GlfilegltfHPP
#define GlfilegltfHPP

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
#include <Glpersistentclasses.hpp>
#include <Glvectorfileobjects.hpp>
#include <Gltexture.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorlists.hpp>
#include <Glvectorgeometry.hpp>
#include <Glmaterial.hpp>
#include <Glutils.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilegltf
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTFVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTFVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLTFVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTFVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilegltf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEGLTF)
using namespace Glfilegltf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilegltfHPP
