// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Hdrimage.pas' rev: 36.00 (Windows)

#ifndef HdrimageHPP
#define HdrimageHPP

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
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Vcl.Graphics.hpp>
#include <Opengltokens.hpp>
#include <Glvectorgeometry.hpp>
#include <Glgraphics.hpp>

//-- user supplied -----------------------------------------------------------

namespace Hdrimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THDRImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION THDRImage : public Vcl::Graphics::TBitmap
{
	typedef Vcl::Graphics::TBitmap inherited;
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* stream);
public:
	/* TBitmap.Create */ inline __fastcall virtual THDRImage()/* overload */ : Vcl::Graphics::TBitmap() { }
	/* TBitmap.Create */ inline __fastcall THDRImage(int AWidth, int AHeight)/* overload */ : Vcl::Graphics::TBitmap(AWidth, AHeight) { }
	/* TBitmap.Destroy */ inline __fastcall virtual ~THDRImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Hdrimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HDRIMAGE)
using namespace Hdrimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HdrimageHPP
