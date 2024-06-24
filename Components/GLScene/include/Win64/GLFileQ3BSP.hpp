// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfileq3bsp.pas' rev: 36.00 (Windows)

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
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Vcl.Graphics.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorlists.hpp>
#include <Q3bsp.hpp>
#include <Glbsp.hpp>
#include <Gltexture.hpp>
#include <Glgraphics.hpp>
#include <Glstate.hpp>
#include <Glutils.hpp>
#include <Glmaterial.hpp>
#include <Gltextureformat.hpp>
#include <Glbaseclasses.hpp>

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
