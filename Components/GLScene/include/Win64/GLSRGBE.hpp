// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSRGBE.pas' rev: 34.00 (Windows)

#ifndef GlsrgbeHPP
#define GlsrgbeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsrgbe
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Float2rgbe(Glvectortypes::TVector4b &RGBE, const float Red, const float Green, const float Blue);
extern DELPHI_PACKAGE void __fastcall Rgbe2float(float &Red, float &Green, float &Blue, const Glvectortypes::TVector4b RGBE);
extern DELPHI_PACKAGE void __fastcall LoadRLEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Scanline_width, int Num_scanlines);
extern DELPHI_PACKAGE void __fastcall LoadRGBEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Numpixels);
}	/* namespace Glsrgbe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSRGBE)
using namespace Glsrgbe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsrgbeHPP
