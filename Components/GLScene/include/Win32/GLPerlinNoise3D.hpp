// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPerlinNoise3D.pas' rev: 36.00 (Windows)

#ifndef Glperlinnoise3dHPP
#define Glperlinnoise3dHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glperlinnoise3d
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPerlin3DNoise;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPerlin3DNoise : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::StaticArray<int, 256> FPermutations;
	System::StaticArray<float, 768> FGradients;
	float __fastcall Lattice(int ix, int iy, int iz, float fx, float fy, float fz)/* overload */;
	float __fastcall Lattice(int ix, int iy, float fx, float fy)/* overload */;
	
public:
	__fastcall TGLPerlin3DNoise(int randomSeed);
	void __fastcall Initialize(int randomSeed);
	float __fastcall Noise(const float x, const float y)/* overload */;
	float __fastcall Noise(const float x, const float y, const float z)/* overload */;
	float __fastcall Noise(const Glvectorgeometry::TAffineVector &v)/* overload */;
	float __fastcall Noise(const Glvectorgeometry::TVector &v)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPerlin3DNoise() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word cPERLIN_TABLE_SIZE = System::Word(0x100);
}	/* namespace Glperlinnoise3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPERLINNOISE3D)
using namespace Glperlinnoise3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glperlinnoise3dHPP
