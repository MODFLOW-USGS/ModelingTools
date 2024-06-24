// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAnimationUtils.pas' rev: 35.00 (Windows)

#ifndef GlanimationutilsHPP
#define GlanimationutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glanimationutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TEaseType : unsigned char { etLinear, etQuadIn, etQuadOut, etQuadInOut, etQuadOutIn, etCubicIn, etCubicOut, etCubicInOut, etCubicOutIn, etQuintIn, etQuintOut, etQuintInOut, etQuintOutIn, etSineIn, etSineOut, etSineInOut, etSineOutIn, etCircIn, etCircOut, etCircInOut, etCircOutIn, etExpoIn, etExpoOut, etExpoInOut, etExpoOutIn, etElasticIn, etElasticOut, etElasticInOut, etElasticOutIn, etBackIn, etBackOut, etBackInOut, etBackOutIn, etBounceIn, etBounceOut, etBounceInOut, etBounceOutIn };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Tweener(const Glvectortypes::TVector3f &Current, const Glvectortypes::TVector3f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Tweener(const Glvectortypes::TVector4f &Current, const Glvectortypes::TVector4f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall Tweener(const Glvectortypes::TVector2f &Current, const Glvectortypes::TVector2f &Target, float Time, float Duration, TEaseType EaseType)/* overload */;
extern DELPHI_PACKAGE float __fastcall Tweener(float Current, float Target, float Time, float Duration, TEaseType EaseType)/* overload */;
}	/* namespace Glanimationutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLANIMATIONUTILS)
using namespace Glanimationutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlanimationutilsHPP
