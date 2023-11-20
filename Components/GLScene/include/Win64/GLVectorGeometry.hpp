// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVectorGeometry.pas' rev: 35.00 (Windows)

#ifndef GlvectorgeometryHPP
#define GlvectorgeometryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.Math.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glvectorgeometry
{
//-- forward type declarations -----------------------------------------------
struct TTexPoint;
struct TQuaternion;
struct TRectangle;
struct DECLSPEC_DRECORD TFrustum
{
public:
	Glvectortypes::TVector4f pLeft;
	Glvectortypes::TVector4f pTop;
	Glvectortypes::TVector4f pRight;
	Glvectortypes::TVector4f pBottom;
	Glvectortypes::TVector4f pNear;
	Glvectortypes::TVector4f pFar;
};


//-- type declarations -------------------------------------------------------
typedef System::PSingle PFloat;

typedef TTexPoint *PTexPoint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TTexPoint
{
public:
	float S;
	float T;
};
#pragma pack(pop)


typedef System::StaticArray<System::Byte, 134217728> TByteVector;

typedef TByteVector *PByteVector;

typedef PByteVector PByteArray;

typedef System::StaticArray<System::Word, 134217728> TWordVector;

typedef TWordVector *PWordVector;

typedef System::StaticArray<int, 134217728> TIntegerVector;

typedef TIntegerVector *PIntegerVector;

typedef PIntegerVector PIntegerArray;

typedef System::StaticArray<float, 134217728> TFloatVector;

typedef TFloatVector *PFloatVector;

typedef PFloatVector PFloatArray;

typedef PFloatVector PSingleArray;

typedef System::DynamicArray<float> TSingleArray;

typedef System::StaticArray<double, 134217728> TDoubleVector;

typedef TDoubleVector *PDoubleVector;

typedef PDoubleVector PDoubleArray;

typedef System::StaticArray<System::Extended, 134217728> TExtendedVector;

typedef TExtendedVector *PExtendedVector;

typedef PExtendedVector PExtendedArray;

typedef System::StaticArray<void *, 134217728> TPointerVector;

typedef TPointerVector *PPointerVector;

typedef PPointerVector PPointerArray;

typedef System::StaticArray<unsigned, 134217728> TCardinalVector;

typedef TCardinalVector *PCardinalVector;

typedef PCardinalVector PCardinalArray;

typedef System::StaticArray<unsigned, 134217728> TLongWordVector;

typedef TLongWordVector *PLongWordVector;

typedef PLongWordVector PLongWordArray;

typedef Glvectortypes::TVector4b *PHomogeneousByteVector;

typedef Glvectortypes::TVector4b THomogeneousByteVector;

typedef Glvectortypes::TVector4w *PHomogeneousWordVector;

typedef Glvectortypes::TVector4w THomogeneousWordVector;

typedef Glvectortypes::TVector4i *PHomogeneousIntVector;

typedef Glvectortypes::TVector4i THomogeneousIntVector;

typedef Glvectortypes::TVector4f *PHomogeneousFltVector;

typedef Glvectortypes::TVector4f THomogeneousFltVector;

typedef Glvectortypes::TVector4d *PHomogeneousDblVector;

typedef Glvectortypes::TVector4d THomogeneousDblVector;

typedef Glvectortypes::TVector4e *PHomogeneousExtVector;

typedef Glvectortypes::TVector4e THomogeneousExtVector;

typedef Glvectortypes::TVector4p *PHomogeneousPtrVector;

typedef Glvectortypes::TVector4p THomogeneousPtrVector;

typedef Glvectortypes::TVector3b *PAffineByteVector;

typedef Glvectortypes::TVector3b TAffineByteVector;

typedef Glvectortypes::TVector3w *PAffineWordVector;

typedef Glvectortypes::TVector3w TAffineWordVector;

typedef Glvectortypes::TVector3i *PAffineIntVector;

typedef Glvectortypes::TVector3i TAffineIntVector;

typedef Glvectortypes::TVector3f *PAffineFltVector;

typedef Glvectortypes::TVector3f TAffineFltVector;

typedef Glvectortypes::TVector3d *PAffineDblVector;

typedef Glvectortypes::TVector3d TAffineDblVector;

typedef Glvectortypes::TVector3e *PAffineExtVector;

typedef Glvectortypes::TVector3e TAffineExtVector;

typedef Glvectortypes::TVector3p *PAffinePtrVector;

typedef Glvectortypes::TVector3p TAffinePtrVector;

typedef Glvectortypes::TVector2f *PVector2f;

typedef Glvectortypes::TVector4f *PVector;

typedef Glvectortypes::TVector4f TVector;

typedef Glvectortypes::TVector4f *PHomogeneousVector;

typedef Glvectortypes::TVector4f THomogeneousVector;

typedef Glvectortypes::TVector3f *PAffineVector;

typedef Glvectortypes::TVector3f TAffineVector;

typedef Glvectortypes::TVector3f *PVertex;

typedef Glvectortypes::TVector3f TVertex;

typedef System::StaticArray<Glvectortypes::TVector3f, 134217728> TAffineVectorArray;

typedef TAffineVectorArray *PAffineVectorArray;

typedef System::StaticArray<Glvectortypes::TVector4f, 67108864> TVectorArray;

typedef TVectorArray *PVectorArray;

typedef System::StaticArray<TTexPoint, 134217728> TTexPointArray;

typedef TTexPointArray *PTexPointArray;

typedef Glvectortypes::TMatrix4b THomogeneousByteMatrix;

typedef System::StaticArray<Glvectortypes::TVector4w, 4> THomogeneousWordMatrix;

typedef Glvectortypes::TMatrix4i THomogeneousIntMatrix;

typedef Glvectortypes::TMatrix4f THomogeneousFltMatrix;

typedef Glvectortypes::TMatrix4d THomogeneousDblMatrix;

typedef System::StaticArray<Glvectortypes::TVector4e, 4> THomogeneousExtMatrix;

typedef Glvectortypes::TMatrix3b TAffineByteMatrix;

typedef System::StaticArray<Glvectortypes::TVector3w, 3> TAffineWordMatrix;

typedef Glvectortypes::TMatrix3i TAffineIntMatrix;

typedef Glvectortypes::TMatrix3f TAffineFltMatrix;

typedef Glvectortypes::TMatrix3d TAffineDblMatrix;

typedef System::StaticArray<Glvectortypes::TVector3e, 3> TAffineExtMatrix;

typedef Glvectortypes::TMatrix4f *PMatrix;

typedef Glvectortypes::TMatrix4f TMatrix;

typedef System::StaticArray<Glvectortypes::TMatrix4f, 16777216> TMatrixArray;

typedef TMatrixArray *PMatrixArray;

typedef Glvectortypes::TMatrix4f *PHomogeneousMatrix;

typedef Glvectortypes::TMatrix4f THomogeneousMatrix;

typedef Glvectortypes::TMatrix3f *PAffineMatrix;

typedef Glvectortypes::TMatrix3f TAffineMatrix;

typedef Glvectortypes::TVector4f THmgPlane;

typedef Glvectortypes::TVector4d TDoubleHmgPlane;

typedef TQuaternion *PQuaternion;

struct DECLSPEC_DRECORD TQuaternion
{
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
			float W;
		};
		struct 
		{
			Glvectortypes::TVector3f ImagPart;
			float RealPart;
		};
		
	};
};


typedef System::StaticArray<TQuaternion, 67108864> TQuaternionArray;

typedef TQuaternionArray *PQuaternionArray;

struct DECLSPEC_DRECORD TRectangle
{
public:
	int Left;
	int Top;
	int Width;
	int Height;
};


typedef TFrustum *PFrustum;

enum DECLSPEC_DENUM TTransType : unsigned char { ttScaleX, ttScaleY, ttScaleZ, ttShearXY, ttShearXZ, ttShearYZ, ttRotateX, ttRotateY, ttRotateZ, ttTranslateX, ttTranslateY, ttTranslateZ, ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW };

typedef System::StaticArray<float, 16> TTransformations;

typedef System::StaticArray<short, 3> TPackedRotationMatrix;

enum DECLSPEC_DENUM TGLInterpolationType : unsigned char { itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp };

enum DECLSPEC_DENUM TEulerOrder : unsigned char { eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX };

//-- var, const, procedure ---------------------------------------------------
static const int cMaxArray = int(0x7ffffff);
static const double cColinearBias = 1.000000E-08;
extern DELPHI_PACKAGE TTexPoint XTexPoint;
extern DELPHI_PACKAGE TTexPoint YTexPoint;
extern DELPHI_PACKAGE TTexPoint XYTexPoint;
extern DELPHI_PACKAGE TTexPoint NullTexPoint;
extern DELPHI_PACKAGE TTexPoint MidTexPoint;
extern DELPHI_PACKAGE Glvectortypes::TVector3f XVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f YVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f ZVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f XYVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f XZVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f YZVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f XYZVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f NullVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f MinusXVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f MinusYVector;
extern DELPHI_PACKAGE Glvectortypes::TVector3f MinusZVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f YHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f ZHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f WHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XYHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f YZHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XZHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XYZHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XYZWHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f NullHmgVector;
extern DELPHI_PACKAGE Glvectortypes::TVector4f XHmgPoint;
extern DELPHI_PACKAGE Glvectortypes::TVector4f YHmgPoint;
extern DELPHI_PACKAGE Glvectortypes::TVector4f ZHmgPoint;
extern DELPHI_PACKAGE Glvectortypes::TVector4f WHmgPoint;
extern DELPHI_PACKAGE Glvectortypes::TVector4f NullHmgPoint;
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f IdentityMatrix;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f IdentityHmgMatrix;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4d IdentityHmgDblMatrix;
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f EmptyMatrix;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f EmptyHmgMatrix;
extern DELPHI_PACKAGE TQuaternion IdentityQuaternion;
extern DELPHI_PACKAGE float EPSILON;
extern DELPHI_PACKAGE float EPSILON2;
extern DELPHI_PACKAGE float cPI;
extern DELPHI_PACKAGE float cPIdiv180;
extern DELPHI_PACKAGE float c180divPI;
extern DELPHI_PACKAGE float c2PI;
extern DELPHI_PACKAGE float cPIdiv2;
extern DELPHI_PACKAGE float cPIdiv4;
extern DELPHI_PACKAGE float c3PIdiv2;
extern DELPHI_PACKAGE float c3PIdiv4;
extern DELPHI_PACKAGE float cInv2PI;
extern DELPHI_PACKAGE float cInv360;
extern DELPHI_PACKAGE float c180;
extern DELPHI_PACKAGE float c360;
extern DELPHI_PACKAGE float cOneHalf;
extern DELPHI_PACKAGE float cLn10;
static const double MinSingle = 1.500000E-45;
static const double MaxSingle = 3.400000E+38;
static const double MinDouble = 4.940656E-324;
static const double MaxDouble = 1.700000E+308;
static const double MinExtended = 0.000000E+00;
static const double MaxExtended = 1.700000E+308;
static const double MinComp = -9.223372E+18;
static const double MaxComp = 9.223372E+18;
extern DELPHI_PACKAGE System::Byte vSIMD;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GeometryOptimizationMode(void);
extern DELPHI_PACKAGE void __fastcall BeginFPUOnlySection(void);
extern DELPHI_PACKAGE void __fastcall EndFPUOnlySection(void);
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointMake(const float S, const float T);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall AffineVectorMake(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall AffineVectorMake(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetAffineVector(/* out */ Glvectortypes::TVector3f &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector3f &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector3f &V, const Glvectortypes::TVector4f &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector3d &V, const Glvectortypes::TVector3f &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector3d &V, const Glvectortypes::TVector4f &vSrc)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorMake(const Glvectortypes::TVector3f &V, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorMake(const float X, const float Y, const float Z, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorMake(const TQuaternion &Q)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointMake(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointMake(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointMake(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector4f &V, const float X, const float Y, const float Z, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector3f &av, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ Glvectortypes::TVector4f &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector3f &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ Glvectortypes::TVector3f &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ Glvectortypes::TVector4f &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector3f &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall RstVector(Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall RstVector(Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorAdd(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAdd(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, PAffineVector vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorAdd(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAdd(const Glvectortypes::TVector3f &V, const float f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorAdd(const Glvectortypes::TVector4f &V, const float f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointAdd(Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(Glvectortypes::TVector3f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(Glvectortypes::TVector3f &V, const float f)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(Glvectortypes::TVector4f &V, const float f)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddPoint(Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall TexPointArrayAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, PTexPointArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall TexPointArrayScaleAndAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, const TTexPoint &scale, PTexPointArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorArrayAdd(const PAffineVectorArray src, const Glvectortypes::TVector3f &delta, const int nb, PAffineVectorArray dest)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorSubtract(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorSubtract(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector3f &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector4f &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector4f &result)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorSubtract(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, Glvectortypes::TVector4f &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, Glvectortypes::TVector3f &result)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorSubtract(const Glvectortypes::TVector3f &V1, float delta)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorSubtract(const Glvectortypes::TVector4f &V1, float delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(Glvectortypes::TVector3f &vr, const Glvectortypes::TVector3f &V, float &f)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(Glvectortypes::TVector3f &vr, const Glvectortypes::TVector3f &V, System::PSingle pf)/* overload */;
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointCombine(const TTexPoint &t1, const TTexPoint &t2, float f1, float f2);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorCombine(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const float f1, const float f2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorCombine3(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3, const float f1, const float f2, const float F3)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine3(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3, const float f1, const float f2, const float F3, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(Glvectortypes::TVector4f &vr, const Glvectortypes::TVector4f &V, float &f)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(Glvectortypes::TVector4f &vr, const Glvectortypes::TVector3f &V, float &f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, const float F1, const float F2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector3f &V2, const float F1, const float F2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, const float F1, const float F2, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, const float F2, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector3f &V2, const float F1, const float F2, Glvectortypes::TVector4f &VR)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine3(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, const Glvectortypes::TVector4f &V3, const float F1, const float F2, const float F3)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine3(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, const Glvectortypes::TVector4f &V3, const float F1, const float F2, const float F3, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointProject(const Glvectortypes::TVector3f &p, const Glvectortypes::TVector3f &origin, const Glvectortypes::TVector3f &direction)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointProject(const Glvectortypes::TVector4f &p, const Glvectortypes::TVector4f &origin, const Glvectortypes::TVector4f &direction)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorCrossProduct(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCrossProduct(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE float __fastcall Lerp(const float start, const float stop, const float T);
extern DELPHI_PACKAGE float __fastcall AngleLerp(float start, float stop, float T);
extern DELPHI_PACKAGE float __fastcall DistanceBetweenAngles(float angle1, float angle2);
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointLerp(const TTexPoint &t1, const TTexPoint &t2, float T)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorLerp(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, float T)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorLerp(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, float T, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorLerp(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, float T)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorLerp(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2, float T, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAngleLerp(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, float T)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAngleCombine(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, float f)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorArrayLerp(const PVectorArray src1, const PVectorArray src2, float T, int n, PVectorArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorArrayLerp(const PAffineVectorArray src1, const PAffineVectorArray src2, float T, int n, PAffineVectorArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorArrayLerp(const PTexPointArray src1, const PTexPointArray src2, float T, int n, PTexPointArray dest)/* overload */;
extern DELPHI_PACKAGE float __fastcall InterpolateCombined(const float start, const float stop, const float delta, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern DELPHI_PACKAGE float __fastcall InterpolateCombinedFastPower(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree);
extern DELPHI_PACKAGE float __fastcall InterpolateCombinedSafe(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern DELPHI_PACKAGE float __fastcall InterpolateCombinedFast(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern DELPHI_PACKAGE float __fastcall InterpolateLn(const float start, const float stop, const float delta, const float DistortionDegree);
extern DELPHI_PACKAGE float __fastcall InterpolateExp(const float start, const float stop, const float delta, const float DistortionDegree);
extern DELPHI_PACKAGE float __fastcall InterpolateSinAlt(const float start, const float stop, const float delta);
extern DELPHI_PACKAGE float __fastcall InterpolateSin(const float start, const float stop, const float delta);
extern DELPHI_PACKAGE float __fastcall InterpolateTan(const float start, const float stop, const float delta);
extern DELPHI_PACKAGE float __fastcall InterpolatePower(const float start, const float stop, const float delta, const float DistortionDegree);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MatrixLerp(const Glvectortypes::TMatrix4f &m1, const Glvectortypes::TMatrix4f &m2, const float delta);
extern DELPHI_PACKAGE float __fastcall RSqrt(float V);
extern DELPHI_PACKAGE float __fastcall VectorLength(const float *V, const int V_High)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(float *V, const int V_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorNormalize(const Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorNormalize(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVectorArray(PAffineVectorArray list, int n)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorNormalize(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorAngleCosine(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorAngleCosine(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorNegate(const Glvectortypes::TVector3f &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorNegate(const Glvectortypes::TVector4f &Vector)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(float *V, const int V_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector2f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector3f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector4f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorScale(const Glvectortypes::TVector2f &V, float factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorScale(const Glvectortypes::TVector3f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const Glvectortypes::TVector3f &V, float factor, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorScale(const Glvectortypes::TVector4f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const Glvectortypes::TVector4f &V, float factor, Glvectortypes::TVector4f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const Glvectortypes::TVector4f &V, float factor, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorScale(const Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorScale(const Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall DivideVector(Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &divider)/* overload */;
extern DELPHI_PACKAGE void __fastcall DivideVector(Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &divider)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorDivide(const Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &divider)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorDivide(const Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &divider)/* overload */;
extern DELPHI_PACKAGE bool __fastcall TexpointEquals(const TTexPoint &p1, const TTexPoint &p2);
extern DELPHI_PACKAGE bool __fastcall RectEquals(const System::Types::TRect &Rect1, const System::Types::TRect &Rect2);
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall AffineVectorEquals(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorIsNull(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorIsNull(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const TTexPoint &V1, const TTexPoint &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance2(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance2(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorPerpendicular(const Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &n);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorReflect(const Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &n);
extern DELPHI_PACKAGE void __fastcall RotateVector(Glvectortypes::TVector4f &Vector, const Glvectortypes::TVector3f &axis, float angle)/* overload */;
extern DELPHI_PACKAGE void __fastcall RotateVector(Glvectortypes::TVector4f &Vector, const Glvectortypes::TVector4f &axis, float angle)/* overload */;
extern DELPHI_PACKAGE void __fastcall RotateVectorAroundY(Glvectortypes::TVector3f &V, float alpha);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorRotateAroundX(const Glvectortypes::TVector3f &V, float alpha)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorRotateAroundY(const Glvectortypes::TVector3f &V, float alpha)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorRotateAroundY(const Glvectortypes::TVector3f &V, float alpha, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorRotateAroundZ(const Glvectortypes::TVector3f &V, float alpha)/* overload */;
extern DELPHI_PACKAGE void __fastcall AbsVector(Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall AbsVector(Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorAbs(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAbs(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const Glvectortypes::TVector4f &V1, const Glvectortypes::TVector4f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(Glvectortypes::TMatrix4d &dest, const Glvectortypes::TMatrix4f &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(Glvectortypes::TMatrix3f &dest, const Glvectortypes::TMatrix4f &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(Glvectortypes::TMatrix4f &dest, const Glvectortypes::TMatrix3f &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrixRow(Glvectortypes::TMatrix4f &dest, int rowNb, const Glvectortypes::TVector4f &aRow)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateScaleMatrix(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateScaleMatrix(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateTranslationMatrix(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateTranslationMatrix(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateScaleAndTranslationMatrix(const Glvectortypes::TVector4f &scale, const Glvectortypes::TVector4f &offset)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixX(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixX(const float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixY(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixY(const float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixZ(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixZ(const float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrix(const Glvectortypes::TVector3f &anAxis, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrix(const Glvectortypes::TVector4f &anAxis, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f __fastcall CreateAffineRotationMatrix(const Glvectortypes::TVector3f &anAxis, float angle);
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f __fastcall MatrixMultiply(const Glvectortypes::TMatrix3f &m1, const Glvectortypes::TMatrix3f &m2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MatrixMultiply(const Glvectortypes::TMatrix4f &m1, const Glvectortypes::TMatrix4f &m2)/* overload */;
extern DELPHI_PACKAGE void __fastcall MatrixMultiply(const Glvectortypes::TMatrix4f &m1, const Glvectortypes::TMatrix4f &m2, Glvectortypes::TMatrix4f &MResult)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorTransform(const Glvectortypes::TVector4f &V, const Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorTransform(const Glvectortypes::TVector4f &V, const Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorTransform(const Glvectortypes::TVector3f &V, const Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorTransform(const Glvectortypes::TVector3f &V, const Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE float __fastcall MatrixDeterminant(const Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE float __fastcall MatrixDeterminant(const Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall AdjointMatrix(Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall AdjointMatrix(Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleMatrix(Glvectortypes::TMatrix3f &M, const float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleMatrix(Glvectortypes::TMatrix4f &M, const float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall TranslateMatrix(Glvectortypes::TMatrix4f &M, const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall TranslateMatrix(Glvectortypes::TMatrix4f &M, const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeMatrix(Glvectortypes::TMatrix4f &M);
extern DELPHI_PACKAGE void __fastcall TransposeMatrix(Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall TransposeMatrix(Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall InvertMatrix(Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MatrixInvert(const Glvectortypes::TMatrix4f &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall InvertMatrix(Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f __fastcall MatrixInvert(const Glvectortypes::TMatrix3f &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall AnglePreservingMatrixInvert(const Glvectortypes::TMatrix4f &mat);
extern DELPHI_PACKAGE bool __fastcall MatrixDecompose(const Glvectortypes::TMatrix4f &M, TTransformations &Tran);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateLookAtMatrix(const Glvectortypes::TVector4f &eye, const Glvectortypes::TVector4f &center, const Glvectortypes::TVector4f &normUp);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateMatrixFromFrustum(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreatePerspectiveMatrix(float FOV, float Aspect, float ZNear, float ZFar);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateOrthoMatrix(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreatePickMatrix(float X, float Y, float deltax, float deltay, const Glvectortypes::TVector4i &viewport);
extern DELPHI_PACKAGE bool __fastcall Project(const Glvectortypes::TVector4f &objectVector, const Glvectortypes::TMatrix4f &ViewProjMatrix, const Glvectortypes::TVector4i &viewport, /* out */ Glvectortypes::TVector4f &WindowVector);
extern DELPHI_PACKAGE bool __fastcall UnProject(const Glvectortypes::TVector4f &WindowVector, const Glvectortypes::TMatrix4f &ViewProjMatrix, const Glvectortypes::TVector4i &viewport, /* out */ Glvectortypes::TVector4f &objectVector);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall CalcPlaneNormal(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3)/* overload */;
extern DELPHI_PACKAGE void __fastcall CalcPlaneNormal(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall CalcPlaneNormal(const Glvectortypes::TVector4f &p1, const Glvectortypes::TVector4f &p2, const Glvectortypes::TVector4f &p3, Glvectortypes::TVector3f &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &normal)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const Glvectortypes::TVector4f &point, const Glvectortypes::TVector4f &normal)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const Glvectortypes::TVector4f &p1, const Glvectortypes::TVector4f &p2, const Glvectortypes::TVector4f &p3)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetPlane(Glvectortypes::TVector4d &dest, const Glvectortypes::TVector4f &src);
extern DELPHI_PACKAGE void __fastcall NormalizePlane(Glvectortypes::TVector4f &plane);
extern DELPHI_PACKAGE float __fastcall PlaneEvaluatePoint(const Glvectortypes::TVector4f &plane, const Glvectortypes::TVector3f &point)/* overload */;
extern DELPHI_PACKAGE float __fastcall PlaneEvaluatePoint(const Glvectortypes::TVector4f &plane, const Glvectortypes::TVector4f &point)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const Glvectortypes::TVector4f &point, const Glvectortypes::TVector4f &planePoint, const Glvectortypes::TVector4f &planeNormal)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &planePoint, const Glvectortypes::TVector3f &planeNormal)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector4f &plane)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const Glvectortypes::TVector4f &point, const Glvectortypes::TVector4f &planePoint, const Glvectortypes::TVector4f &planeNormal)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &planePoint, const Glvectortypes::TVector3f &planeNormal)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector4f &plane)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointPlaneOrthoProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector4f &plane, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointPlaneProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector4f &plane, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall SegmentPlaneIntersection(const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector4f &plane, Glvectortypes::TVector3f &inter);
extern DELPHI_PACKAGE bool __fastcall PointTriangleOrthoProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointTriangleProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall IsLineIntersectTriangle(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC);
extern DELPHI_PACKAGE bool __fastcall PointQuadOrthoProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC, const Glvectortypes::TVector3f &ptD, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointQuadProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC, const Glvectortypes::TVector3f &ptD, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall IsLineIntersectQuad(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector3f &ptA, const Glvectortypes::TVector3f &ptB, const Glvectortypes::TVector3f &ptC, const Glvectortypes::TVector3f &ptD);
extern DELPHI_PACKAGE bool __fastcall PointDiskOrthoProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &center, const Glvectortypes::TVector3f &up, const float radius, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointDiskProjection(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &direction, const Glvectortypes::TVector3f &center, const Glvectortypes::TVector3f &up, const float radius, Glvectortypes::TVector3f &inter, bool bothface = true);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall PointLineClosestPoint(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &linePoint, const Glvectortypes::TVector3f &lineDirection);
extern DELPHI_PACKAGE float __fastcall PointLineDistance(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &linePoint, const Glvectortypes::TVector3f &lineDirection);
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointSegmentClosestPoint(const Glvectortypes::TVector4f &point, const Glvectortypes::TVector4f &segmentStart, const Glvectortypes::TVector4f &segmentStop)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall PointSegmentClosestPoint(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &segmentStart, const Glvectortypes::TVector3f &segmentStop)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointSegmentDistance(const Glvectortypes::TVector3f &point, const Glvectortypes::TVector3f &segmentStart, const Glvectortypes::TVector3f &segmentStop);
extern DELPHI_PACKAGE void __fastcall SegmentSegmentClosestPoint(const Glvectortypes::TVector3f &S0Start, const Glvectortypes::TVector3f &S0Stop, const Glvectortypes::TVector3f &S1Start, const Glvectortypes::TVector3f &S1Stop, Glvectortypes::TVector3f &Segment0Closest, Glvectortypes::TVector3f &Segment1Closest);
extern DELPHI_PACKAGE float __fastcall SegmentSegmentDistance(const Glvectortypes::TVector3f &S0Start, const Glvectortypes::TVector3f &S0Stop, const Glvectortypes::TVector3f &S1Start, const Glvectortypes::TVector3f &S1Stop);
extern DELPHI_PACKAGE float __fastcall LineLineDistance(const Glvectortypes::TVector3f &linePt0, const Glvectortypes::TVector3f &lineDir0, const Glvectortypes::TVector3f &linePt1, const Glvectortypes::TVector3f &lineDir1);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const float *Imag, const int Imag_High, float Real)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const float X, const float Y, const float Z, const float W)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionConjugate(const TQuaternion &Q);
extern DELPHI_PACKAGE float __fastcall QuaternionMagnitude(const TQuaternion &Q);
extern DELPHI_PACKAGE void __fastcall NormalizeQuaternion(TQuaternion &Q);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromPoints(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromMatrix(const Glvectortypes::TMatrix4f &mat);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMultiply(const TQuaternion &qL, const TQuaternion &qR);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall QuaternionToMatrix(const TQuaternion &quat);
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f __fastcall QuaternionToAffineMatrix(const TQuaternion &quat);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromAngleAxis(const float angle, const Glvectortypes::TVector3f &axis);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromRollPitchYaw(const float r, const float p, const float Y);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromEuler(const float X, const float Y, const float Z, TEulerOrder eulerOrder);
extern DELPHI_PACKAGE void __fastcall QuaternionToPoints(const TQuaternion &Q, Glvectortypes::TVector3f &ArcFrom, Glvectortypes::TVector3f &ArcTo);
extern DELPHI_PACKAGE float __fastcall Logarithm2(const float X);
extern DELPHI_PACKAGE float __fastcall PowerSingle(const float Base, const float Exponent)/* overload */;
extern DELPHI_PACKAGE float __fastcall PowerInteger(float Base, int Exponent)/* overload */;
extern DELPHI_PACKAGE float __fastcall PowerInt64(float Base, __int64 Exponent)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall DegToRadian(const System::Extended Degrees)/* overload */;
extern DELPHI_PACKAGE float __fastcall DegToRadian(const float Degrees)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall RadianToDeg(const System::Extended Radians)/* overload */;
extern DELPHI_PACKAGE float __fastcall RadianToDeg(const float Radians)/* overload */;
extern DELPHI_PACKAGE float __fastcall NormalizeAngle(float angle);
extern DELPHI_PACKAGE float __fastcall NormalizeDegAngle(float angle);
extern DELPHI_PACKAGE void __fastcall SinCosine(const double Theta, /* out */ double &Sin, /* out */ double &Cos)/* overload */;
extern DELPHI_PACKAGE void __fastcall SinCosine(const float Theta, /* out */ float &Sin, /* out */ float &Cos)/* overload */;
extern DELPHI_PACKAGE void __fastcall SinCosine(const double Theta, const double radius, /* out */ double &Sin, /* out */ double &Cos)/* overload */;
extern DELPHI_PACKAGE void __fastcall SinCosine(const float Theta, const float radius, /* out */ float &Sin, /* out */ float &Cos)/* overload */;
extern DELPHI_PACKAGE void __fastcall PrepareSinCosCache(float *S, const int S_High, float *c, const int c_High, float startAngle, float stopAngle);
extern DELPHI_PACKAGE System::Extended __fastcall ArcCosine(const System::Extended X)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall ArcSine(const System::Extended X)/* overload */;
extern DELPHI_PACKAGE float __fastcall FastArcTangent2(float Y, float X);
extern DELPHI_PACKAGE int __fastcall ISqrt(int i);
extern DELPHI_PACKAGE int __fastcall ILength(int X, int Y)/* overload */;
extern DELPHI_PACKAGE int __fastcall ILength(int X, int Y, int Z)/* overload */;
extern DELPHI_PACKAGE float __fastcall RLength(float X, float Y);
extern DELPHI_PACKAGE void __fastcall RandomPointOnSphere(Glvectortypes::TVector3f &p);
extern DELPHI_PACKAGE float __fastcall RoundInt(float V)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall RoundInt(System::Extended V)/* overload */;
extern DELPHI_PACKAGE int __fastcall SignStrict(float X);
extern DELPHI_PACKAGE int __fastcall ScaleAndRound(int i, float &S);
extern DELPHI_PACKAGE bool __fastcall IsInRange(const float X, const float a, const float b)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInRange(const double X, const double a, const double b)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInCube(const Glvectortypes::TVector3f &p, const Glvectortypes::TVector3f &d)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInCube(const Glvectortypes::TVector4f &p, const Glvectortypes::TVector4f &d)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(PFloatVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(PDoubleVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(PExtendedVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float *V, const int V_High)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float V1, const float V2)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(const double V1, const double V2)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(const System::Extended V1, const System::Extended V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float V1, const float V2, const float V3)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(const double V1, const double V2, const double V3)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(const System::Extended V1, const System::Extended V2, const System::Extended V3)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(PFloatVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE double __fastcall MaxFloat(PDoubleVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(PExtendedVector values, int nbItems)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(const float *V, const int V_High)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(const float V1, const float V2)/* overload */;
extern DELPHI_PACKAGE double __fastcall MaxFloat(const double V1, const double V2)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(const System::Extended V1, const System::Extended V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(const float V1, const float V2, const float V3)/* overload */;
extern DELPHI_PACKAGE double __fastcall MaxFloat(const double V1, const double V2, const double V3)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(const System::Extended V1, const System::Extended V2, const System::Extended V3)/* overload */;
extern DELPHI_PACKAGE int __fastcall MinInteger(const int V1, const int V2)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall MinInteger(const unsigned V1, const unsigned V2)/* overload */;
extern DELPHI_PACKAGE int __fastcall MinInteger(const int V1, const int V2, const int V3)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall MinInteger(const unsigned V1, const unsigned V2, const unsigned V3)/* overload */;
extern DELPHI_PACKAGE int __fastcall MaxInteger(const int V1, const int V2)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall MaxInteger(const unsigned V1, const unsigned V2)/* overload */;
extern DELPHI_PACKAGE int __fastcall MaxInteger(const int V1, const int V2, const int V3)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall MaxInteger(const unsigned V1, const unsigned V2, const unsigned V3)/* overload */;
extern DELPHI_PACKAGE int __fastcall ClampInteger(const int value, const int min, const int max)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall ClampInteger(const unsigned value, const unsigned min, const unsigned max)/* overload */;
extern DELPHI_PACKAGE float __fastcall TriangleArea(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3)/* overload */;
extern DELPHI_PACKAGE float __fastcall PolygonArea(const PAffineVectorArray p, int nSides)/* overload */;
extern DELPHI_PACKAGE float __fastcall TriangleSignedArea(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3)/* overload */;
extern DELPHI_PACKAGE float __fastcall PolygonSignedArea(const PAffineVectorArray p, int nSides)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleFloatArray(PFloatVector values, int nb, float &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleFloatArray(TSingleArray &values, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(PFloatVector values, int nb, float &delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(float *values, const int values_High, float delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(PFloatVector valuesDest, PFloatVector valuesDelta, int nb)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxXYZComponent(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxXYZComponent(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinXYZComponent(const Glvectortypes::TVector4f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinXYZComponent(const Glvectortypes::TVector3f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxAbsXYZComponent(const Glvectortypes::TVector4f &V);
extern DELPHI_PACKAGE float __fastcall MinAbsXYZComponent(const Glvectortypes::TVector4f &V);
extern DELPHI_PACKAGE void __fastcall MaxVector(Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MaxVector(Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MinVector(Glvectortypes::TVector4f &V, const Glvectortypes::TVector4f &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MinVector(Glvectortypes::TVector3f &V, const Glvectortypes::TVector3f &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall SortArrayAscending(System::Extended *a, const int a_High);
extern DELPHI_PACKAGE float __fastcall ClampValue(const float aValue, const float aMin, const float aMax)/* overload */;
extern DELPHI_PACKAGE float __fastcall ClampValue(const float aValue, const float aMin)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall MakeAffineDblVector(double *V, const int V_High);
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall MakeDblVector(double *V, const int V_High);
extern DELPHI_PACKAGE bool __fastcall PointInPolygon(const float *xp, const int xp_High, const float *yp, const int yp_High, float X, float Y);
extern DELPHI_PACKAGE bool __fastcall IsPointInPolygon(const System::Types::TPoint *Polygon, const int Polygon_High, const System::Types::TPoint &p);
extern DELPHI_PACKAGE void __fastcall DivMod(int Dividend, System::Word Divisor, System::Word &result, System::Word &Remainder);
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall ConvertRotation(const Glvectortypes::TVector3f &Angles);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &QStart, const TQuaternion &QEnd, int Spin, float T)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &source, const TQuaternion &dest, const float T)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorDblToFlt(const Glvectortypes::TVector4d &V);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAffineDblToFlt(const Glvectortypes::TVector3d &V);
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall VectorAffineFltToDbl(const Glvectortypes::TVector3f &V);
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall VectorFltToDbl(const Glvectortypes::TVector4f &V);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Turn(const Glvectortypes::TMatrix4f &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Turn(const Glvectortypes::TMatrix4f &Matrix, const Glvectortypes::TVector3f &MasterUp, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Pitch(const Glvectortypes::TMatrix4f &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Pitch(const Glvectortypes::TMatrix4f &Matrix, const Glvectortypes::TVector3f &MasterRight, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Roll(const Glvectortypes::TMatrix4f &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Roll(const Glvectortypes::TMatrix4f &Matrix, const Glvectortypes::TVector3f &MasterDirection, float angle)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastPlaneIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const Glvectortypes::TVector4f &planePoint, const Glvectortypes::TVector4f &planeNormal, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastPlaneXZIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const float planeY, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastTriangleIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3, PVector intersectPoint = (PVector)(0x0), PVector intersectNormal = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE float __fastcall RayCastMinDistToPoint(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const Glvectortypes::TVector4f &point);
extern DELPHI_PACKAGE bool __fastcall RayCastIntersectsSphere(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const Glvectortypes::TVector4f &sphereCenter, const float SphereRadius)/* overload */;
extern DELPHI_PACKAGE int __fastcall RayCastSphereIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, const Glvectortypes::TVector4f &sphereCenter, const float SphereRadius, Glvectortypes::TVector4f &i1, Glvectortypes::TVector4f &i2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastBoxIntersect(const Glvectortypes::TVector3f &rayStart, const Glvectortypes::TVector3f &rayVector, const Glvectortypes::TVector3f &aMinExtent, const Glvectortypes::TVector3f &aMaxExtent, PAffineVector intersectPoint = (PAffineVector)(0x0));
extern DELPHI_PACKAGE float __fastcall SphereVisibleRadius(float distance, float radius);
extern DELPHI_PACKAGE int __fastcall IntersectLinePlane(const Glvectortypes::TVector4f &point, const Glvectortypes::TVector4f &direction, const Glvectortypes::TVector4f &plane, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectTriangleBox(const Glvectortypes::TVector3f &p1, const Glvectortypes::TVector3f &p2, const Glvectortypes::TVector3f &p3, const Glvectortypes::TVector3f &aMinExtent, const Glvectortypes::TVector3f &aMaxExtent);
extern DELPHI_PACKAGE bool __fastcall IntersectSphereBox(const Glvectortypes::TVector4f &SpherePos, const float SphereRadius, const Glvectortypes::TMatrix4f &BoxMatrix, const Glvectortypes::TVector3f &BoxScale, PAffineVector intersectPoint = (PAffineVector)(0x0), PAffineVector normal = (PAffineVector)(0x0), System::PSingle depth = (System::PSingle)(0x0));
extern DELPHI_PACKAGE TFrustum __fastcall ExtractFrustumFromModelViewProjection(const Glvectortypes::TMatrix4f &modelViewProj);
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const Glvectortypes::TVector3f &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const Glvectortypes::TVector4f &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const Glvectortypes::TVector3f &min, const Glvectortypes::TVector3f &max, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MakeParallelProjectionMatrix(const Glvectortypes::TVector4f &plane, const Glvectortypes::TVector4f &dir);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MakeShadowMatrix(const Glvectortypes::TVector4f &planePoint, const Glvectortypes::TVector4f &planeNormal, const Glvectortypes::TVector4f &lightPos);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MakeReflectionMatrix(const Glvectortypes::TVector3f &planePoint, const Glvectortypes::TVector3f &planeNormal);
extern DELPHI_PACKAGE TPackedRotationMatrix __fastcall PackRotationMatrix(const Glvectortypes::TMatrix4f &mat);
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall UnPackRotationMatrix(const TPackedRotationMatrix &packedMatrix);
extern DELPHI_PACKAGE bool __fastcall BarycentricCoordinates(const Glvectortypes::TVector3f &V1, const Glvectortypes::TVector3f &V2, const Glvectortypes::TVector3f &V3, const Glvectortypes::TVector3f &p, float &u, float &V);
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall Vector2fMake(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2i __fastcall Vector2iMake(const int X, const int Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2s __fastcall Vector2sMake(const short X, const short Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2d __fastcall Vector2dMake(const double X, const double Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2b __fastcall Vector2bMake(const System::Byte X, const System::Byte Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall Vector2fMake(const Glvectortypes::TVector3f &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2i __fastcall Vector2iMake(const Glvectortypes::TVector3i &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2s __fastcall Vector2sMake(const Glvectortypes::TVector3s &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2d __fastcall Vector2dMake(const Glvectortypes::TVector3d &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2b __fastcall Vector2bMake(const Glvectortypes::TVector3b Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall Vector2fMake(const Glvectortypes::TVector4f &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2i __fastcall Vector2iMake(const Glvectortypes::TVector4i &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2s __fastcall Vector2sMake(const Glvectortypes::TVector4s &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2d __fastcall Vector2dMake(const Glvectortypes::TVector4d &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2b __fastcall Vector2bMake(const Glvectortypes::TVector4b Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Vector3fMake(const float X, const float Y = 0.000000E+00f, const float Z = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const int X, const int Y = 0x0, const int Z = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Vector3fMake(const Glvectortypes::TVector2f &Vector, const float Z = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const Glvectortypes::TVector2i &Vector, const int Z = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const Glvectortypes::TVector2s Vector, const short Z = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const Glvectortypes::TVector2d &Vector, const double Z = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const Glvectortypes::TVector2b Vector, const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Vector3fMake(const Glvectortypes::TVector4f &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const Glvectortypes::TVector4i &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const Glvectortypes::TVector4s &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const Glvectortypes::TVector4d &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const Glvectortypes::TVector4b Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const float X, const float Y = 0.000000E+00f, const float Z = 0.000000E+00f, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const int X, const int Y = 0x0, const int Z = 0x0, const int W = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4s __fastcall Vector4sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0), const short W = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall Vector4dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00, const double W = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4b __fastcall Vector4bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0), const System::Byte W = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const Glvectortypes::TVector3f &Vector, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const Glvectortypes::TVector3i &Vector, const int W = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4s __fastcall Vector4sMake(const Glvectortypes::TVector3s &Vector, const short W = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall Vector4dMake(const Glvectortypes::TVector3d &Vector, const double W = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4b __fastcall Vector4bMake(const Glvectortypes::TVector3b Vector, const System::Byte W = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const Glvectortypes::TVector2f &Vector, const float Z = 0.000000E+00f, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const Glvectortypes::TVector2i &Vector, const int Z = 0x0, const int W = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4s __fastcall Vector4sMake(const Glvectortypes::TVector2s Vector, const short Z = (short)(0x0), const short W = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall Vector4dMake(const Glvectortypes::TVector2d &Vector, const double Z = 0.000000E+00, const double W = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4b __fastcall Vector4bMake(const Glvectortypes::TVector2b Vector, const System::Byte Z = (System::Byte)(0x0), const System::Byte W = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector2f &Vector1, const Glvectortypes::TVector2f &Vector2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector2i &Vector1, const Glvectortypes::TVector2i &Vector2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector2d &V1, const Glvectortypes::TVector2d &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector2s V1, const Glvectortypes::TVector2s V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector2b V1, const Glvectortypes::TVector2b V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector3i &V1, const Glvectortypes::TVector3i &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector3d &V1, const Glvectortypes::TVector3d &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector3s &V1, const Glvectortypes::TVector3s &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector3b V1, const Glvectortypes::TVector3b V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector4i &V1, const Glvectortypes::TVector4i &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector4d &V1, const Glvectortypes::TVector4d &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector4s &V1, const Glvectortypes::TVector4s &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const Glvectortypes::TVector4b V1, const Glvectortypes::TVector4b V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix3f &Matrix1, const Glvectortypes::TMatrix3f &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix3i &Matrix1, const Glvectortypes::TMatrix3i &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix3d &Matrix1, const Glvectortypes::TMatrix3d &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix3s &Matrix1, const Glvectortypes::TMatrix3s &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix3b &Matrix1, const Glvectortypes::TMatrix3b &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix4f &Matrix1, const Glvectortypes::TMatrix4f &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix4i &Matrix1, const Glvectortypes::TMatrix4i &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix4d &Matrix1, const Glvectortypes::TMatrix4d &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix4s &Matrix1, const Glvectortypes::TMatrix4s &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall MatrixEquals(const Glvectortypes::TMatrix4b &Matrix1, const Glvectortypes::TMatrix4b &Matrix2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3f &SourceVector, const Glvectortypes::TVector3f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3f &SourceVector, const Glvectortypes::TVector3f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3f &SourceVector, const Glvectortypes::TVector3f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3f &SourceVector, const Glvectortypes::TVector3f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4f &SourceVector, const Glvectortypes::TVector4f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4f &SourceVector, const Glvectortypes::TVector4f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4f &SourceVector, const Glvectortypes::TVector4f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4f &SourceVector, const Glvectortypes::TVector4f &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3i &SourceVector, const Glvectortypes::TVector3i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3i &SourceVector, const Glvectortypes::TVector3i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3i &SourceVector, const Glvectortypes::TVector3i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3i &SourceVector, const Glvectortypes::TVector3i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4i &SourceVector, const Glvectortypes::TVector4i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4i &SourceVector, const Glvectortypes::TVector4i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4i &SourceVector, const Glvectortypes::TVector4i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4i &SourceVector, const Glvectortypes::TVector4i &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3s &SourceVector, const Glvectortypes::TVector3s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3s &SourceVector, const Glvectortypes::TVector3s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3s &SourceVector, const Glvectortypes::TVector3s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3s &SourceVector, const Glvectortypes::TVector3s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4s &SourceVector, const Glvectortypes::TVector4s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4s &SourceVector, const Glvectortypes::TVector4s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4s &SourceVector, const Glvectortypes::TVector4s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4s &SourceVector, const Glvectortypes::TVector4s &ComparedVector)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreThen(const Glvectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorMoreEqualThen(const Glvectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessThen(const Glvectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorLessEqualThen(const Glvectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RectanglesIntersect(const Glvectortypes::TVector2f &ACenterOfRect1, const Glvectortypes::TVector2f &ACenterOfRect2, const Glvectortypes::TVector2f &ASizeOfRect1, const Glvectortypes::TVector2f &ASizeOfRect2);
extern DELPHI_PACKAGE bool __fastcall RectangleContains(const Glvectortypes::TVector2f &ACenterOfBigRect1, const Glvectortypes::TVector2f &ACenterOfSmallRect2, const Glvectortypes::TVector2f &ASizeOfBigRect1, const Glvectortypes::TVector2f &ASizeOfSmallRect2, const float AEps = 0.000000E+00f);
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall GetSafeTurnAngle(const Glvectortypes::TVector4f &AOriginalPosition, const Glvectortypes::TVector4f &AOriginalUpVector, const Glvectortypes::TVector4f &ATargetPosition, const Glvectortypes::TVector4f &AMoveAroundTargetCenter)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall GetSafeTurnAngle(const Glvectortypes::TVector3f &AOriginalPosition, const Glvectortypes::TVector3f &AOriginalUpVector, const Glvectortypes::TVector3f &ATargetPosition, const Glvectortypes::TVector3f &AMoveAroundTargetCenter)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall MoveObjectAround(const Glvectortypes::TVector4f &AMovingObjectPosition, const Glvectortypes::TVector4f &AMovingObjectUp, const Glvectortypes::TVector4f &ATargetPosition, float pitchDelta, float turnDelta);
extern DELPHI_PACKAGE float __fastcall AngleBetweenVectors(const Glvectortypes::TVector4f &a, const Glvectortypes::TVector4f &b, const Glvectortypes::TVector4f &ACenterPoint)/* overload */;
extern DELPHI_PACKAGE float __fastcall AngleBetweenVectors(const Glvectortypes::TVector3f &a, const Glvectortypes::TVector3f &b, const Glvectortypes::TVector3f &ACenterPoint)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall ShiftObjectFromCenter(const Glvectortypes::TVector4f &AOriginalPosition, const Glvectortypes::TVector4f &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall ShiftObjectFromCenter(const Glvectortypes::TVector3f &AOriginalPosition, const Glvectortypes::TVector3f &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
}	/* namespace Glvectorgeometry */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVECTORGEOMETRY)
using namespace Glvectorgeometry;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlvectorgeometryHPP
