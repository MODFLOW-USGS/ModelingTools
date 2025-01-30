// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Filters.pas' rev: 36.00 (Windows)

#ifndef GR32_FiltersHPP
#define GR32_FiltersHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>
#include <GR32_Blend.hpp>
#include <GR32_System.hpp>
#include <GR32_Bindings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_filters
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::Byte, 256> TLUT8;

enum DECLSPEC_DENUM TLogicalOperator : unsigned char { loXOR, loAND, loOR };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall CheckParams(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src, bool ResizeDst = true);
extern DELPHI_PACKAGE void __fastcall CopyComponents(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src, Gr32::TColor32Components Components)/* overload */;
extern DELPHI_PACKAGE void __fastcall CopyComponents(Gr32::TCustomBitmap32* Dst, int DstX, int DstY, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TColor32Components Components)/* overload */;
extern DELPHI_PACKAGE void __fastcall AlphaToGrayscale(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src);
extern DELPHI_PACKAGE void __fastcall IntensityToAlpha(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src);
extern DELPHI_PACKAGE void __fastcall Invert(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src, Gr32::TColor32Components Components = (Gr32::TColor32Components() << Gr32::TColor32Component::ccBlue << Gr32::TColor32Component::ccGreen << Gr32::TColor32Component::ccRed << Gr32::TColor32Component::ccAlpha ));
extern DELPHI_PACKAGE void __fastcall InvertRGB(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src);
extern DELPHI_PACKAGE void __fastcall ColorToGrayscale(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src, bool PreserveAlpha = false);
extern DELPHI_PACKAGE void __fastcall ApplyLUT(Gr32::TCustomBitmap32* Dst, Gr32::TCustomBitmap32* Src, const TLUT8 &LUT, bool PreserveAlpha = false);
extern DELPHI_PACKAGE void __fastcall ChromaKey(Gr32::TCustomBitmap32* ABitmap, Gr32::TColor32 TrColor);
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall CreateBitmask(Gr32::TColor32Components Components);
extern DELPHI_PACKAGE void __fastcall ApplyBitmask(Gr32::TCustomBitmap32* Dst, int DstX, int DstY, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TColor32 Bitmask, TLogicalOperator LogicalOperator)/* overload */;
extern DELPHI_PACKAGE void __fastcall ApplyBitmask(Gr32::TCustomBitmap32* ABitmap, const Gr32::TRect &ARect, Gr32::TColor32 Bitmask, TLogicalOperator LogicalOperator)/* overload */;
}	/* namespace Gr32_filters */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_FILTERS)
using namespace Gr32_filters;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GR32_FiltersHPP
