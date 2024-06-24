// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Blend.pas' rev: 35.00 (Windows)

#ifndef Gr32_blendHPP
#define Gr32_blendHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GR32.hpp>
#include <GR32_System.hpp>
#include <GR32_Bindings.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_blend
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef Gr32::TColor32 __fastcall (*TBlendReg)(Gr32::TColor32 F, Gr32::TColor32 B);

typedef void __fastcall (*TBlendMem)(Gr32::TColor32 F, Gr32::TColor32 &B);

typedef Gr32::TColor32 __fastcall (*TBlendRegEx)(Gr32::TColor32 F, Gr32::TColor32 B, Gr32::TColor32 M);

typedef void __fastcall (*TBlendMemEx)(Gr32::TColor32 F, Gr32::TColor32 &B, Gr32::TColor32 M);

typedef void __fastcall (*TBlendLine)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count);

typedef void __fastcall (*TBlendLineEx)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count, Gr32::TColor32 M);

typedef Gr32::TColor32 __fastcall (*TCombineReg)(Gr32::TColor32 X, Gr32::TColor32 Y, Gr32::TColor32 W);

typedef void __fastcall (*TCombineMem)(Gr32::TColor32 X, Gr32::TColor32 &Y, Gr32::TColor32 W);

typedef void __fastcall (*TCombineLine)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count, Gr32::TColor32 W);

typedef Gr32::TColor32 __fastcall (*TLightenReg)(Gr32::TColor32 C, int Amount);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool MMX_ACTIVE;
extern DELPHI_PACKAGE void __fastcall (*EMMS)(void);
extern DELPHI_PACKAGE TBlendReg BlendReg;
extern DELPHI_PACKAGE TBlendMem BlendMem;
extern DELPHI_PACKAGE TBlendRegEx BlendRegEx;
extern DELPHI_PACKAGE TBlendMemEx BlendMemEx;
extern DELPHI_PACKAGE TBlendLine BlendLine;
extern DELPHI_PACKAGE TBlendLineEx BlendLineEx;
extern DELPHI_PACKAGE TCombineReg CombineReg;
extern DELPHI_PACKAGE TCombineMem CombineMem;
extern DELPHI_PACKAGE TCombineLine CombineLine;
extern DELPHI_PACKAGE TBlendReg MergeReg;
extern DELPHI_PACKAGE TBlendMem MergeMem;
extern DELPHI_PACKAGE TBlendRegEx MergeRegEx;
extern DELPHI_PACKAGE TBlendMemEx MergeMemEx;
extern DELPHI_PACKAGE TBlendLine MergeLine;
extern DELPHI_PACKAGE TBlendLineEx MergeLineEx;
extern DELPHI_PACKAGE TBlendReg ColorAdd;
extern DELPHI_PACKAGE TBlendReg ColorSub;
extern DELPHI_PACKAGE TBlendReg ColorDiv;
extern DELPHI_PACKAGE TBlendReg ColorModulate;
extern DELPHI_PACKAGE TBlendReg ColorMax;
extern DELPHI_PACKAGE TBlendReg ColorMin;
extern DELPHI_PACKAGE TBlendReg ColorDifference;
extern DELPHI_PACKAGE TBlendReg ColorAverage;
extern DELPHI_PACKAGE TBlendReg ColorExclusion;
extern DELPHI_PACKAGE TBlendReg ColorScale;
extern DELPHI_PACKAGE void *AlphaTable;
extern DELPHI_PACKAGE void *bias_ptr;
extern DELPHI_PACKAGE void *alpha_ptr;
extern DELPHI_PACKAGE TLightenReg LightenReg;
typedef Gr32::TColor32 __fastcall (*_dt_Gr32_blend_1)(Gr32::TColor32 F, Gr32::TColor32 B);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_1*, 2> BLEND_REG;
typedef void __fastcall (*_dt_Gr32_blend_2)(Gr32::TColor32 F, Gr32::TColor32 &B);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_2*, 2> BLEND_MEM;
typedef Gr32::TColor32 __fastcall (*_dt_Gr32_blend_3)(Gr32::TColor32 F, Gr32::TColor32 B, Gr32::TColor32 M);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_3*, 2> BLEND_REG_EX;
typedef void __fastcall (*_dt_Gr32_blend_4)(Gr32::TColor32 F, Gr32::TColor32 &B, Gr32::TColor32 M);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_4*, 2> BLEND_MEM_EX;
typedef void __fastcall (*_dt_Gr32_blend_5)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_5*, 2> BLEND_LINE;
typedef void __fastcall (*_dt_Gr32_blend_6)(Gr32::PColor32 Src, Gr32::PColor32 Dst, int Count, Gr32::TColor32 M);
extern DELPHI_PACKAGE System::StaticArray<_dt_Gr32_blend_6*, 2> BLEND_LINE_EX;
extern DELPHI_PACKAGE Gr32_bindings::TFunctionRegistry* BlendRegistry;
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall Lighten(Gr32::TColor32 C, int Amount);
}	/* namespace Gr32_blend */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_BLEND)
using namespace Gr32_blend;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_blendHPP
