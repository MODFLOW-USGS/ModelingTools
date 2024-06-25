// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPerlinBase.pas' rev: 36.00 (Windows)

#ifndef GlperlinbaseHPP
#define GlperlinbaseHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glperlinbase
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<double> T1DPerlinArray;

typedef System::DynamicArray<T1DPerlinArray> T2DPerlinArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int __fastcall ExponateCrap(int root, int exponant);
extern DELPHI_PACKAGE double __fastcall Perlin_Random1(int x);
extern DELPHI_PACKAGE double __fastcall Perlin_Random2(const int x, const int Y);
extern DELPHI_PACKAGE void __fastcall Perlin_Random1DStrip(int x, int Width, int Step, double Amp, T1DPerlinArray Res);
extern DELPHI_PACKAGE void __fastcall Smooth_Interpolate_Strip(T1DPerlinArray B1, T1DPerlinArray B2, T1DPerlinArray B3, T1DPerlinArray Res, int Width);
extern DELPHI_PACKAGE void __fastcall Cubic_Interpolate_Strip(T1DPerlinArray B1, T1DPerlinArray B2, T1DPerlinArray B3, T1DPerlinArray B4, T1DPerlinArray Res, int Width);
extern DELPHI_PACKAGE double __fastcall Linear_Interpolate(const double a, const double b, const double x);
extern DELPHI_PACKAGE double __fastcall Cosine_Interpolate(const double a, const double b, const double x);
extern DELPHI_PACKAGE double __fastcall Cubic_Interpolate(double v0, double v1, double v2, double v3, double x);
}	/* namespace Glperlinbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPERLINBASE)
using namespace Glperlinbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlperlinbaseHPP
