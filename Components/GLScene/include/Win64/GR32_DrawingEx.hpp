// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_DrawingEx.pas' rev: 35.00 (Windows)

#ifndef Gr32_drawingexHPP
#define Gr32_drawingexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <GR32.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_drawingex
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TBlendLineProc)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall ClipLine(int &X1, int &Y1, int &X2, int &Y2, int MinX, int MinY, int MaxX, int MaxY);
}	/* namespace Gr32_drawingex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_DRAWINGEX)
using namespace Gr32_drawingex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_drawingexHPP
