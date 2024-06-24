// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'O3tcimage.pas' rev: 36.00 (Windows)

#ifndef O3tcimageHPP
#define O3tcimageHPP

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

namespace O3tcimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TO3TCImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TO3TCImage : public Vcl::Graphics::TBitmap
{
	typedef Vcl::Graphics::TBitmap inherited;
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* stream);
public:
	/* TBitmap.Create */ inline __fastcall virtual TO3TCImage()/* overload */ : Vcl::Graphics::TBitmap() { }
	/* TBitmap.Create */ inline __fastcall TO3TCImage(int AWidth, int AHeight)/* overload */ : Vcl::Graphics::TBitmap(AWidth, AHeight) { }
	/* TBitmap.Destroy */ inline __fastcall virtual ~TO3TCImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace O3tcimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_O3TCIMAGE)
using namespace O3tcimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// O3tcimageHPP
