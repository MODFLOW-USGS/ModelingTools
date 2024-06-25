// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileQ3BSP.pas' rev: 36.00 (Windows)

#ifndef Glfileq3bspHPP
#define Glfileq3bspHPP

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
#include <GLVectorFileObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <Q3BSP.hpp>
#include <GLBSP.hpp>
#include <GLTexture.hpp>
#include <GLGraphics.hpp>
#include <GLState.hpp>
#include <GLUtils.hpp>
#include <GLMaterial.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfileq3bsp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLQ3BSPVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLQ3BSPVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLQ3BSPVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLQ3BSPVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float vQ3BSPLightmapGammaCorrection;
extern DELPHI_PACKAGE float vQ3BSPLightmapBrightness;
extern DELPHI_PACKAGE bool vGLFileQ3BSPLoadMaterials;
}	/* namespace Glfileq3bsp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEQ3BSP)
using namespace Glfileq3bsp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfileq3bspHPP
