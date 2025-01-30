// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_LowLevel.pas' rev: 36.00 (Windows)

#ifndef GR32_LowLevelHPP
#define GR32_LowLevelHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <GR32.hpp>
#include <GR32_Math.hpp>
#include <GR32_System.hpp>
#include <GR32_Bindings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_lowlevel
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall (*FillLongword)(void *X, unsigned Count, System::LongWord Value);
extern DELPHI_PACKAGE System::StaticArray<Gr32::TWrapProc, 3> WRAP_PROCS;
extern DELPHI_PACKAGE System::StaticArray<Gr32::TWrapProcEx, 3> WRAP_PROCS_EX;
extern DELPHI_PACKAGE int __fastcall Clamp(const int Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall FillWord(void *X, unsigned Count, System::LongWord Value);
extern DELPHI_PACKAGE void __fastcall MoveLongword(const void *Source, void *Dest, int Count);
extern DELPHI_PACKAGE void __fastcall MoveWord(const void *Source, void *Dest, int Count);
extern DELPHI_PACKAGE void __fastcall Swap(void * &A, void * &B)/* overload */;
extern DELPHI_PACKAGE void __fastcall Swap(int &A, int &B)/* overload */;
extern DELPHI_PACKAGE void __fastcall Swap(Gr32::TFixed &A, Gr32::TFixed &B)/* overload */;
extern DELPHI_PACKAGE void __fastcall Swap(Gr32::TColor32 &A, Gr32::TColor32 &B)/* overload */;
extern DELPHI_PACKAGE void __fastcall TestSwap(int &A, int &B)/* overload */;
extern DELPHI_PACKAGE void __fastcall TestSwap(Gr32::TFixed &A, Gr32::TFixed &B)/* overload */;
extern DELPHI_PACKAGE bool __fastcall TestClip(int &A, int &B, const int Size)/* overload */;
extern DELPHI_PACKAGE bool __fastcall TestClip(int &A, int &B, const int Start, const int Stop)/* overload */;
extern DELPHI_PACKAGE int __fastcall Constrain(const int Value, const int Lo, const int Hi)/* overload */;
extern DELPHI_PACKAGE float __fastcall Constrain(const float Value, const float Lo, const float Hi)/* overload */;
extern DELPHI_PACKAGE int __fastcall SwapConstrain(const int Value, int Constrain1, int Constrain2);
extern DELPHI_PACKAGE int __fastcall Max(const int A, const int B, const int C)/* overload */;
extern DELPHI_PACKAGE int __fastcall Min(const int A, const int B, const int C)/* overload */;
extern DELPHI_PACKAGE int __fastcall Clamp(int Value, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall Clamp(int Value, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall Wrap(int Value, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall Wrap(int Value, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE float __fastcall Wrap(float Value, float Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall Mirror(int Value, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall Mirror(int Value, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall WrapPow2(int Value, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall WrapPow2(int Value, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall MirrorPow2(int Value, int Max)/* overload */;
extern DELPHI_PACKAGE int __fastcall MirrorPow2(int Value, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProc __fastcall GetOptimalWrap(int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProcEx __fastcall GetOptimalWrap(int Min, int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProc __fastcall GetOptimalMirror(int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProcEx __fastcall GetOptimalMirror(int Min, int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProc __fastcall GetWrapProc(Gr32::TWrapMode WrapMode)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProc __fastcall GetWrapProc(Gr32::TWrapMode WrapMode, int Max)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProcEx __fastcall GetWrapProcEx(Gr32::TWrapMode WrapMode)/* overload */;
extern DELPHI_PACKAGE Gr32::TWrapProcEx __fastcall GetWrapProcEx(Gr32::TWrapMode WrapMode, int Min, int Max)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall Div255(unsigned Value);
extern DELPHI_PACKAGE int __fastcall SAR_4(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_8(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_9(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_11(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_12(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_13(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_14(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_15(int Value);
extern DELPHI_PACKAGE int __fastcall SAR_16(int Value);
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall ColorSwap(System::Uitypes::TColor WinColor);
extern DELPHI_PACKAGE void * __fastcall StackAlloc(int Size);
extern DELPHI_PACKAGE void __fastcall StackFree(void * P);
}	/* namespace Gr32_lowlevel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_LOWLEVEL)
using namespace Gr32_lowlevel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GR32_LowLevelHPP
