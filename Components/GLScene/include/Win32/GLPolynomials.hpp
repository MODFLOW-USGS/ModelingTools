// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPolynomials.pas' rev: 36.00 (Windows)

#ifndef GLPolynomialsHPP
#define GLPolynomialsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glpolynomials
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<double> TDoubleArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE double __fastcall EvalPolynom(const TDoubleArray poly, const double x);
extern DELPHI_PACKAGE TDoubleArray __fastcall DerivatedPolynom(const TDoubleArray poly);
extern DELPHI_PACKAGE double __fastcall FindRoot(const TDoubleArray poly, double min, double max, double epsilon);
extern DELPHI_PACKAGE bool __fastcall MinPositiveCoef(const TDoubleArray coefs, double &aMin);
extern DELPHI_PACKAGE double __fastcall cbrt(const double x);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveQuadric(const Glvectorgeometry::PDoubleArray c);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveCubic(const Glvectorgeometry::PDoubleArray c);
extern DELPHI_PACKAGE TDoubleArray __fastcall SolveQuartic(const Glvectorgeometry::PDoubleArray c);
}	/* namespace Glpolynomials */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPOLYNOMIALS)
using namespace Glpolynomials;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLPolynomialsHPP
