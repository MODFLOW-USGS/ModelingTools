﻿// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfileoct.pas' rev: 36.00 (Windows)

#ifndef GlfileoctHPP
#define GlfileoctHPP

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
#include <Vcl.Graphics.hpp>
#include <Gltexture.hpp>
#include <Glmaterial.hpp>
#include <Glgraphics.hpp>
#include <Glcrossplatform.hpp>
#include <Glstate.hpp>
#include <Glutils.hpp>
#include <Gltextureformat.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glvectorgeometry.hpp>
#include <Glapplicationfileio.hpp>
#include <Fileoct.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfileoct
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLOCTGLVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLOCTGLVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLOCTGLVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLOCTGLVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float vGLFileOCTLightmapBrightness;
extern DELPHI_PACKAGE float vGLFileOCTLightmapGammaCorrection;
extern DELPHI_PACKAGE bool vGLFileOCTAllocateMaterials;
}	/* namespace Glfileoct */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEOCT)
using namespace Glfileoct;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileoctHPP
