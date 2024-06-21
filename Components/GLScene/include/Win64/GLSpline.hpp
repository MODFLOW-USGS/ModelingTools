// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSpline.pas' rev: 36.00 (Windows)

#ifndef GlsplineHPP
#define GlsplineHPP

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
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glspline
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCubicSpline;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::StaticArray<float, 4> > TCubicSplineMatrix;

class PASCALIMPLEMENTATION TCubicSpline : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCubicSplineMatrix matX;
	TCubicSplineMatrix matY;
	TCubicSplineMatrix matZ;
	TCubicSplineMatrix matW;
	int FNb;
	
public:
	__fastcall TCubicSpline(const Glvectorgeometry::PFloatArray X, const Glvectorgeometry::PFloatArray Y, const Glvectorgeometry::PFloatArray Z, const Glvectorgeometry::PFloatArray W, const int nb);
	__fastcall virtual ~TCubicSpline();
	float __fastcall SplineX(const float t);
	float __fastcall SplineY(const float t);
	float __fastcall SplineZ(const float t);
	float __fastcall SplineW(const float t);
	void __fastcall SplineXY(const float t, /* out */ float &X, /* out */ float &Y);
	void __fastcall SplineXYZ(const float t, /* out */ float &X, /* out */ float &Y, /* out */ float &Z);
	void __fastcall SplineXYZW(const float t, /* out */ float &X, /* out */ float &Y, /* out */ float &Z, /* out */ float &W);
	Glvectorgeometry::TAffineVector __fastcall SplineAffineVector(const float t)/* overload */;
	void __fastcall SplineAffineVector(const float t, Glvectorgeometry::TAffineVector &vector)/* overload */;
	Glvectorgeometry::TVector __fastcall SplineVector(const float t)/* overload */;
	void __fastcall SplineVector(const float t, Glvectorgeometry::TVector &vector)/* overload */;
	float __fastcall SplineSlopeX(const float t);
	float __fastcall SplineSlopeY(const float t);
	float __fastcall SplineSlopeZ(const float t);
	float __fastcall SplineSlopeW(const float t);
	Glvectorgeometry::TAffineVector __fastcall SplineSlopeVector(const float t)/* overload */;
	bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z);
	bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z);
	bool __fastcall SplineIntersecXY(float Z, float &X, float &Y);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glspline */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSPLINE)
using namespace Glspline;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsplineHPP
