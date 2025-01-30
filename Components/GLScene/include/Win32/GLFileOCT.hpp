// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileOCT.pas' rev: 36.00 (Windows)

#ifndef GLFileOCTHPP
#define GLFileOCTHPP

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
#include <Vcl.Graphics.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLGraphics.hpp>
#include <GLCrossPlatform.hpp>
#include <GLState.hpp>
#include <GLUtils.hpp>
#include <GLTextureFormat.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorGeometry.hpp>
#include <GLApplicationFileIO.hpp>
#include <FileOCT.hpp>
#include <GLBaseClasses.hpp>

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
#endif	// GLFileOCTHPP
