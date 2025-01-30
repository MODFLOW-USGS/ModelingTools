// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVectorGeometry.pas' rev: 36.00 (Windows)

#ifndef GLVectorGeometryHPP
#define GLVectorGeometryHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
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
struct TFrustum;
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

typedef PFloatArray PSingleArray;

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
			TAffineVector ImagPart;
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


struct DECLSPEC_DRECORD TFrustum
{
public:
	THmgPlane pLeft;
	THmgPlane pTop;
	THmgPlane pRight;
	THmgPlane pBottom;
	THmgPlane pNear;
	THmgPlane pFar;
};


typedef TFrustum *PFrustum;

enum DECLSPEC_DENUM TTransType : unsigned char { ttScaleX, ttScaleY, ttScaleZ, ttShearXY, ttShearXZ, ttShearYZ, ttRotateX, ttRotateY, ttRotateZ, ttTranslateX, ttTranslateY, ttTranslateZ, ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW };

typedef System::StaticArray<float, 16> TTransformations;

typedef System::StaticArray<short, 3> TPackedRotationMatrix;

enum DECLSPEC_DENUM TGLInterpolationType : unsigned char { itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp };

enum DECLSPEC_DENUM TEulerOrder : unsigned char { eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX };

//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST int cMaxArray = int(0x7ffffff);
static const System::Extended cColinearBias = 1.000000E-08;
extern DELPHI_PACKAGE TTexPoint XTexPoint;
extern DELPHI_PACKAGE TTexPoint YTexPoint;
extern DELPHI_PACKAGE TTexPoint XYTexPoint;
extern DELPHI_PACKAGE TTexPoint NullTexPoint;
extern DELPHI_PACKAGE TTexPoint MidTexPoint;
extern DELPHI_PACKAGE TAffineVector XVector;
extern DELPHI_PACKAGE TAffineVector YVector;
extern DELPHI_PACKAGE TAffineVector ZVector;
extern DELPHI_PACKAGE TAffineVector XYVector;
extern DELPHI_PACKAGE TAffineVector XZVector;
extern DELPHI_PACKAGE TAffineVector YZVector;
extern DELPHI_PACKAGE TAffineVector XYZVector;
extern DELPHI_PACKAGE TAffineVector NullVector;
extern DELPHI_PACKAGE TAffineVector MinusXVector;
extern DELPHI_PACKAGE TAffineVector MinusYVector;
extern DELPHI_PACKAGE TAffineVector MinusZVector;
extern DELPHI_PACKAGE THomogeneousVector XHmgVector;
extern DELPHI_PACKAGE THomogeneousVector YHmgVector;
extern DELPHI_PACKAGE THomogeneousVector ZHmgVector;
extern DELPHI_PACKAGE THomogeneousVector WHmgVector;
extern DELPHI_PACKAGE THomogeneousVector XYHmgVector;
extern DELPHI_PACKAGE THomogeneousVector YZHmgVector;
extern DELPHI_PACKAGE THomogeneousVector XZHmgVector;
extern DELPHI_PACKAGE THomogeneousVector XYZHmgVector;
extern DELPHI_PACKAGE THomogeneousVector XYZWHmgVector;
extern DELPHI_PACKAGE THomogeneousVector NullHmgVector;
extern DELPHI_PACKAGE THomogeneousVector XHmgPoint;
extern DELPHI_PACKAGE THomogeneousVector YHmgPoint;
extern DELPHI_PACKAGE THomogeneousVector ZHmgPoint;
extern DELPHI_PACKAGE THomogeneousVector WHmgPoint;
extern DELPHI_PACKAGE THomogeneousVector NullHmgPoint;
extern DELPHI_PACKAGE TAffineMatrix IdentityMatrix;
extern DELPHI_PACKAGE TMatrix IdentityHmgMatrix;
extern DELPHI_PACKAGE THomogeneousDblMatrix IdentityHmgDblMatrix;
extern DELPHI_PACKAGE TAffineMatrix EmptyMatrix;
extern DELPHI_PACKAGE TMatrix EmptyHmgMatrix;
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
static const System::Extended MinSingle = 1.500000E-45;
static const System::Extended MaxSingle = 3.400000E+38;
static const System::Extended MinDouble = 5.000000E-324;
static const System::Extended MaxDouble = 1.700000E+308;
static const System::Extended MinExtended = 3.400000E-4932;
static const System::Extended MaxExtended = 1.700000E+308;
static const System::Extended MinComp = -9.223372E+18;
static const System::Extended MaxComp = 9.223372E+18;
extern DELPHI_PACKAGE System::Byte vSIMD;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GeometryOptimizationMode(void);
extern DELPHI_PACKAGE void __fastcall BeginFPUOnlySection(void);
extern DELPHI_PACKAGE void __fastcall EndFPUOnlySection(void);
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointMake(const float S, const float T);
extern DELPHI_PACKAGE TAffineVector __fastcall AffineVectorMake(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall AffineVectorMake(const TVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetAffineVector(/* out */ TAffineVector &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TAffineVector &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TAffineVector &V, const TVector &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TAffineVector &V, const TAffineVector &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TAffineDblVector &V, const TAffineVector &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TAffineDblVector &V, const TVector &vSrc)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall VectorMake(const TAffineVector &V, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorMake(const float X, const float Y, const float Z, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorMake(const TQuaternion &Q)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall PointMake(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointMake(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointMake(const TVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TVector &V, const float X, const float Y, const float Z, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TVector &V, const TAffineVector &av, float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetVector(/* out */ TVector &V, const TVector &vSrc)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ TVector &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ TVector &V, const TAffineVector &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakePoint(/* out */ TVector &V, const TVector &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ TAffineVector &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ TVector &V, const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ TVector &V, const TAffineVector &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall MakeVector(/* out */ TVector &V, const TVector &av)/* overload */;
extern DELPHI_PACKAGE void __fastcall RstVector(TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall RstVector(TVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorAdd(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAdd(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const TAffineVector &V1, const TAffineVector &V2, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const TAffineVector &V1, const TAffineVector &V2, PAffineVector vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorAdd(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorAdd(const TVector &V1, const TVector &V2, TVector &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAdd(const TAffineVector &V, const float f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorAdd(const TVector &V, const float f)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall PointAdd(TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(TAffineVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(TAffineVector &V, const float f)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddVector(TVector &V, const float f)/* overload */;
extern DELPHI_PACKAGE void __fastcall AddPoint(TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall TexPointArrayAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, PTexPointArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall TexPointArrayScaleAndAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, const TTexPoint &scale, PTexPointArray dest)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorArrayAdd(const PAffineVectorArray src, const TAffineVector &delta, const int nb, PAffineVectorArray dest)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorSubtract(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorSubtract(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const TAffineVector &V1, const TAffineVector &V2, TAffineVector &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const TAffineVector &V1, const TAffineVector &V2, TVector &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const TVector &V1, const TAffineVector &V2, TVector &result)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorSubtract(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const TVector &V1, const TVector &V2, TVector &result)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorSubtract(const TVector &V1, const TVector &V2, TAffineVector &result)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorSubtract(const TAffineVector &V1, float delta)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorSubtract(const TVector &V1, float delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SubtractVector(TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(TAffineVector &vr, const TAffineVector &V, float &f)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(TAffineVector &vr, const TAffineVector &V, PFloat pf)/* overload */;
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointCombine(const TTexPoint &t1, const TTexPoint &t2, float f1, float f2);
extern DELPHI_PACKAGE TAffineVector __fastcall VectorCombine(const TAffineVector &V1, const TAffineVector &V2, const float f1, const float f2)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorCombine3(const TAffineVector &V1, const TAffineVector &V2, const TAffineVector &V3, const float f1, const float f2, const float F3)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine3(const TAffineVector &V1, const TAffineVector &V2, const TAffineVector &V3, const float f1, const float f2, const float F3, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(TVector &vr, const TVector &V, float &f)/* overload */;
extern DELPHI_PACKAGE void __fastcall CombineVector(TVector &vr, const TAffineVector &V, float &f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine(const TVector &V1, const TVector &V2, const float F1, const float F2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine(const TVector &V1, const TAffineVector &V2, const float F1, const float F2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const TVector &V1, const TVector &V2, const float F1, const float F2, TVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const TVector &V1, const TVector &V2, const float F2, TVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine(const TVector &V1, const TAffineVector &V2, const float F1, const float F2, TVector &VR)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCombine3(const TVector &V1, const TVector &V2, const TVector &V3, const float F1, const float F2, const float F3)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCombine3(const TVector &V1, const TVector &V2, const TVector &V3, const float F1, const float F2, const float F3, TVector &vr)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDotProduct(const TVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointProject(const TAffineVector &p, const TAffineVector &origin, const TAffineVector &direction)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointProject(const TVector &p, const TVector &origin, const TVector &direction)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorCrossProduct(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorCrossProduct(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const TVector &V1, const TVector &V2, TVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const TAffineVector &V1, const TAffineVector &V2, TVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const TVector &V1, const TVector &V2, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorCrossProduct(const TAffineVector &V1, const TAffineVector &V2, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE float __fastcall Lerp(const float start, const float stop, const float T);
extern DELPHI_PACKAGE float __fastcall AngleLerp(float start, float stop, float T);
extern DELPHI_PACKAGE float __fastcall DistanceBetweenAngles(float angle1, float angle2);
extern DELPHI_PACKAGE TTexPoint __fastcall TexPointLerp(const TTexPoint &t1, const TTexPoint &t2, float T)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorLerp(const TAffineVector &V1, const TAffineVector &V2, float T)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorLerp(const TAffineVector &V1, const TAffineVector &V2, float T, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorLerp(const TVector &V1, const TVector &V2, float T)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorLerp(const TVector &V1, const TVector &V2, float T, TVector &vr)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorAngleLerp(const TAffineVector &V1, const TAffineVector &V2, float T)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorAngleCombine(const TAffineVector &V1, const TAffineVector &V2, float f)/* overload */;
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
extern DELPHI_PACKAGE TMatrix __fastcall MatrixLerp(const TMatrix &m1, const TMatrix &m2, const float delta);
extern DELPHI_PACKAGE float __fastcall RSqrt(float V);
extern DELPHI_PACKAGE float __fastcall VectorLength(const float *V, const System::NativeInt V_High)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const float X, const float Y, const float Z)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorLength(const TVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(const TVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorNorm(float *V, const System::NativeInt V_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorNormalize(const Glvectortypes::TVector2f &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorNormalize(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVectorArray(PAffineVectorArray list, int n)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeVector(TVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorNormalize(const TVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorAngleCosine(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorAngleCosine(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorNegate(const TAffineVector &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorNegate(const TVector &Vector)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(TVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NegateVector(float *V, const System::NativeInt V_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(Glvectortypes::TVector2f &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(TAffineVector &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(TVector &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(TAffineVector &V, const TAffineVector &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleVector(TVector &V, const TVector &factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall VectorScale(const Glvectortypes::TVector2f &V, float factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorScale(const TAffineVector &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const TAffineVector &V, float factor, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorScale(const TVector &V, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const TVector &V, float factor, TVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorScale(const TVector &V, float factor, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorScale(const TAffineVector &V, const TAffineVector &factor)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorScale(const TVector &V, const TVector &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall DivideVector(TVector &V, const TVector &divider)/* overload */;
extern DELPHI_PACKAGE void __fastcall DivideVector(TAffineVector &V, const TAffineVector &divider)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall VectorDivide(const TVector &V, const TVector &divider)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorDivide(const TAffineVector &V, const TAffineVector &divider)/* overload */;
extern DELPHI_PACKAGE bool __fastcall TexpointEquals(const TTexPoint &p1, const TTexPoint &p2);
extern DELPHI_PACKAGE bool __fastcall RectEquals(const System::Types::TRect &Rect1, const System::Types::TRect &Rect2);
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorEquals(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall AffineVectorEquals(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorIsNull(const TVector &V)/* overload */;
extern DELPHI_PACKAGE bool __fastcall VectorIsNull(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const TTexPoint &V1, const TTexPoint &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorSpacing(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance2(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall VectorDistance2(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorPerpendicular(const TAffineVector &V, const TAffineVector &n);
extern DELPHI_PACKAGE TAffineVector __fastcall VectorReflect(const TAffineVector &V, const TAffineVector &n);
extern DELPHI_PACKAGE void __fastcall RotateVector(TVector &Vector, const TAffineVector &axis, float angle)/* overload */;
extern DELPHI_PACKAGE void __fastcall RotateVector(TVector &Vector, const TVector &axis, float angle)/* overload */;
extern DELPHI_PACKAGE void __fastcall RotateVectorAroundY(TAffineVector &V, float alpha);
extern DELPHI_PACKAGE TAffineVector __fastcall VectorRotateAroundX(const TAffineVector &V, float alpha)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorRotateAroundY(const TAffineVector &V, float alpha)/* overload */;
extern DELPHI_PACKAGE void __fastcall VectorRotateAroundY(const TAffineVector &V, float alpha, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall VectorRotateAroundZ(const TAffineVector &V, float alpha)/* overload */;
extern DELPHI_PACKAGE void __fastcall AbsVector(TVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall AbsVector(TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall VectorAbs(const TVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorAbs(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const Glvectortypes::TVector2f &V1, const Glvectortypes::TVector2f &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const TAffineVector &V1, const TAffineVector &V2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsColinear(const TVector &V1, const TVector &V2)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(THomogeneousDblMatrix &dest, const TMatrix &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(TAffineMatrix &dest, const TMatrix &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrix(TMatrix &dest, const TAffineMatrix &src)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetMatrixRow(TMatrix &dest, int rowNb, const TVector &aRow)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateScaleMatrix(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateScaleMatrix(const TVector &V)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateTranslationMatrix(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateTranslationMatrix(const TVector &V)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateScaleAndTranslationMatrix(const TVector &scale, const TVector &offset)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateRotationMatrixX(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixX(const float angle)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateRotationMatrixY(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixY(const float angle)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateRotationMatrixZ(const float sine, const float cosine)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrixZ(const float angle)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall CreateRotationMatrix(const TAffineVector &anAxis, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall CreateRotationMatrix(const TVector &anAxis, float angle)/* overload */;
extern DELPHI_PACKAGE TAffineMatrix __fastcall CreateAffineRotationMatrix(const TAffineVector &anAxis, float angle);
extern DELPHI_PACKAGE TAffineMatrix __fastcall MatrixMultiply(const TAffineMatrix &m1, const TAffineMatrix &m2)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall MatrixMultiply(const TMatrix &m1, const TMatrix &m2)/* overload */;
extern DELPHI_PACKAGE void __fastcall MatrixMultiply(const TMatrix &m1, const TMatrix &m2, TMatrix &MResult)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall VectorTransform(const TVector &V, const TMatrix &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall VectorTransform(const TVector &V, const TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorTransform(const TAffineVector &V, const TMatrix &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall VectorTransform(const TAffineVector &V, const TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE float __fastcall MatrixDeterminant(const TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE float __fastcall MatrixDeterminant(const TMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall AdjointMatrix(TMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall AdjointMatrix(TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleMatrix(TAffineMatrix &M, const float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleMatrix(TMatrix &M, const float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall TranslateMatrix(TMatrix &M, const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall TranslateMatrix(TMatrix &M, const TVector &V)/* overload */;
extern DELPHI_PACKAGE void __fastcall NormalizeMatrix(TMatrix &M);
extern DELPHI_PACKAGE void __fastcall TransposeMatrix(TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall TransposeMatrix(TMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall InvertMatrix(TMatrix &M)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall MatrixInvert(const TMatrix &M)/* overload */;
extern DELPHI_PACKAGE void __fastcall InvertMatrix(TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix3f __fastcall MatrixInvert(const TAffineMatrix &M)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall AnglePreservingMatrixInvert(const TMatrix &mat);
extern DELPHI_PACKAGE bool __fastcall MatrixDecompose(const TMatrix &M, TTransformations &Tran);
extern DELPHI_PACKAGE TMatrix __fastcall CreateLookAtMatrix(const TVector &eye, const TVector &center, const TVector &normUp);
extern DELPHI_PACKAGE TMatrix __fastcall CreateMatrixFromFrustum(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern DELPHI_PACKAGE TMatrix __fastcall CreatePerspectiveMatrix(float FOV, float Aspect, float ZNear, float ZFar);
extern DELPHI_PACKAGE TMatrix __fastcall CreateOrthoMatrix(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern DELPHI_PACKAGE TMatrix __fastcall CreatePickMatrix(float X, float Y, float deltax, float deltay, const Glvectortypes::TVector4i &viewport);
extern DELPHI_PACKAGE bool __fastcall Project(const TVector &objectVector, const TMatrix &ViewProjMatrix, const Glvectortypes::TVector4i &viewport, /* out */ TVector &WindowVector);
extern DELPHI_PACKAGE bool __fastcall UnProject(const TVector &WindowVector, const TMatrix &ViewProjMatrix, const Glvectortypes::TVector4i &viewport, /* out */ TVector &objectVector);
extern DELPHI_PACKAGE TAffineVector __fastcall CalcPlaneNormal(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3)/* overload */;
extern DELPHI_PACKAGE void __fastcall CalcPlaneNormal(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE void __fastcall CalcPlaneNormal(const TVector &p1, const TVector &p2, const TVector &p3, TAffineVector &vr)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const TAffineVector &point, const TAffineVector &normal)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const TVector &point, const TVector &normal)/* overload */;
extern DELPHI_PACKAGE THmgPlane __fastcall PlaneMake(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PlaneMake(const TVector &p1, const TVector &p2, const TVector &p3)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetPlane(TDoubleHmgPlane &dest, const THmgPlane &src);
extern DELPHI_PACKAGE void __fastcall NormalizePlane(THmgPlane &plane);
extern DELPHI_PACKAGE float __fastcall PlaneEvaluatePoint(const THmgPlane &plane, const TAffineVector &point)/* overload */;
extern DELPHI_PACKAGE float __fastcall PlaneEvaluatePoint(const THmgPlane &plane, const TVector &point)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const TVector &point, const TVector &planePoint, const TVector &planeNormal)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const TAffineVector &point, const TAffineVector &planePoint, const TAffineVector &planeNormal)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointIsInHalfSpace(const TAffineVector &point, const THmgPlane &plane)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const TVector &point, const TVector &planePoint, const TVector &planeNormal)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const TAffineVector &point, const TAffineVector &planePoint, const TAffineVector &planeNormal)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointPlaneDistance(const TAffineVector &point, const THmgPlane &plane)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PointPlaneOrthoProjection(const TAffineVector &point, const THmgPlane &plane, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointPlaneProjection(const TAffineVector &point, const TAffineVector &direction, const THmgPlane &plane, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall SegmentPlaneIntersection(const TAffineVector &ptA, const TAffineVector &ptB, const THmgPlane &plane, TAffineVector &inter);
extern DELPHI_PACKAGE bool __fastcall PointTriangleOrthoProjection(const TAffineVector &point, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointTriangleProjection(const TAffineVector &point, const TAffineVector &direction, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall IsLineIntersectTriangle(const TAffineVector &point, const TAffineVector &direction, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC);
extern DELPHI_PACKAGE bool __fastcall PointQuadOrthoProjection(const TAffineVector &point, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC, const TAffineVector &ptD, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointQuadProjection(const TAffineVector &point, const TAffineVector &direction, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC, const TAffineVector &ptD, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall IsLineIntersectQuad(const TAffineVector &point, const TAffineVector &direction, const TAffineVector &ptA, const TAffineVector &ptB, const TAffineVector &ptC, const TAffineVector &ptD);
extern DELPHI_PACKAGE bool __fastcall PointDiskOrthoProjection(const TAffineVector &point, const TAffineVector &center, const TAffineVector &up, const float radius, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE bool __fastcall PointDiskProjection(const TAffineVector &point, const TAffineVector &direction, const TAffineVector &center, const TAffineVector &up, const float radius, TAffineVector &inter, bool bothface = true);
extern DELPHI_PACKAGE TAffineVector __fastcall PointLineClosestPoint(const TAffineVector &point, const TAffineVector &linePoint, const TAffineVector &lineDirection);
extern DELPHI_PACKAGE float __fastcall PointLineDistance(const TAffineVector &point, const TAffineVector &linePoint, const TAffineVector &lineDirection);
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall PointSegmentClosestPoint(const TVector &point, const TVector &segmentStart, const TVector &segmentStop)/* overload */;
extern DELPHI_PACKAGE TAffineVector __fastcall PointSegmentClosestPoint(const TAffineVector &point, const TAffineVector &segmentStart, const TAffineVector &segmentStop)/* overload */;
extern DELPHI_PACKAGE float __fastcall PointSegmentDistance(const TAffineVector &point, const TAffineVector &segmentStart, const TAffineVector &segmentStop);
extern DELPHI_PACKAGE void __fastcall SegmentSegmentClosestPoint(const TAffineVector &S0Start, const TAffineVector &S0Stop, const TAffineVector &S1Start, const TAffineVector &S1Stop, TAffineVector &Segment0Closest, TAffineVector &Segment1Closest);
extern DELPHI_PACKAGE float __fastcall SegmentSegmentDistance(const TAffineVector &S0Start, const TAffineVector &S0Stop, const TAffineVector &S1Start, const TAffineVector &S1Stop);
extern DELPHI_PACKAGE float __fastcall LineLineDistance(const TAffineVector &linePt0, const TAffineVector &lineDir0, const TAffineVector &linePt1, const TAffineVector &lineDir1);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const float *Imag, const System::NativeInt Imag_High, float Real)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const float X, const float Y, const float Z, const float W)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMake(const TVector &V)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionConjugate(const TQuaternion &Q);
extern DELPHI_PACKAGE float __fastcall QuaternionMagnitude(const TQuaternion &Q);
extern DELPHI_PACKAGE void __fastcall NormalizeQuaternion(TQuaternion &Q);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromPoints(const TAffineVector &V1, const TAffineVector &V2);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromMatrix(const TMatrix &mat);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionMultiply(const TQuaternion &qL, const TQuaternion &qR);
extern DELPHI_PACKAGE TMatrix __fastcall QuaternionToMatrix(const TQuaternion &quat);
extern DELPHI_PACKAGE TAffineMatrix __fastcall QuaternionToAffineMatrix(const TQuaternion &quat);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromAngleAxis(const float angle, const TAffineVector &axis);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromRollPitchYaw(const float r, const float p, const float Y);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionFromEuler(const float X, const float Y, const float Z, TEulerOrder eulerOrder);
extern DELPHI_PACKAGE void __fastcall QuaternionToPoints(const TQuaternion &Q, TAffineVector &ArcFrom, TAffineVector &ArcTo);
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
extern DELPHI_PACKAGE void __fastcall PrepareSinCosCache(float *S, const System::NativeInt S_High, float *c, const System::NativeInt c_High, float startAngle, float stopAngle);
extern DELPHI_PACKAGE System::Extended __fastcall ArcCosine(const System::Extended X)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall ArcSine(const System::Extended X)/* overload */;
extern DELPHI_PACKAGE float __fastcall FastArcTangent2(float Y, float X);
extern DELPHI_PACKAGE int __fastcall ISqrt(int i);
extern DELPHI_PACKAGE int __fastcall ILength(int X, int Y)/* overload */;
extern DELPHI_PACKAGE int __fastcall ILength(int X, int Y, int Z)/* overload */;
extern DELPHI_PACKAGE float __fastcall RLength(float X, float Y);
extern DELPHI_PACKAGE void __fastcall RandomPointOnSphere(TAffineVector &p);
extern DELPHI_PACKAGE float __fastcall RoundInt(float V)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall RoundInt(System::Extended V)/* overload */;
extern DELPHI_PACKAGE int __fastcall SignStrict(float X);
extern DELPHI_PACKAGE int __fastcall ScaleAndRound(int i, float &S);
extern DELPHI_PACKAGE bool __fastcall IsInRange(const float X, const float a, const float b)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInRange(const double X, const double a, const double b)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInCube(const TAffineVector &p, const TAffineVector &d)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsInCube(const TVector &p, const TVector &d)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(PSingleArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(PDoubleArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(PExtendedArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float *V, const System::NativeInt V_High)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float V1, const float V2)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(const double V1, const double V2)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(const System::Extended V1, const System::Extended V2)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinFloat(const float V1, const float V2, const float V3)/* overload */;
extern DELPHI_PACKAGE double __fastcall MinFloat(const double V1, const double V2, const double V3)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(const System::Extended V1, const System::Extended V2, const System::Extended V3)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(PSingleArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE double __fastcall MaxFloat(PDoubleArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(PExtendedArray values, int nbItems)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxFloat(const float *V, const System::NativeInt V_High)/* overload */;
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
extern DELPHI_PACKAGE float __fastcall TriangleArea(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3)/* overload */;
extern DELPHI_PACKAGE float __fastcall PolygonArea(const PAffineVectorArray p, int nSides)/* overload */;
extern DELPHI_PACKAGE float __fastcall TriangleSignedArea(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3)/* overload */;
extern DELPHI_PACKAGE float __fastcall PolygonSignedArea(const PAffineVectorArray p, int nSides)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleFloatArray(PSingleArray values, int nb, float &factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall ScaleFloatArray(TSingleArray &values, float factor)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(PSingleArray values, int nb, float &delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(float *values, const System::NativeInt values_High, float delta)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetFloatArray(PSingleArray valuesDest, PSingleArray valuesDelta, int nb)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxXYZComponent(const TVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxXYZComponent(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinXYZComponent(const TVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MinXYZComponent(const TAffineVector &V)/* overload */;
extern DELPHI_PACKAGE float __fastcall MaxAbsXYZComponent(const TVector &V);
extern DELPHI_PACKAGE float __fastcall MinAbsXYZComponent(const TVector &V);
extern DELPHI_PACKAGE void __fastcall MaxVector(TVector &V, const TVector &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MaxVector(TAffineVector &V, const TAffineVector &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MinVector(TVector &V, const TVector &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall MinVector(TAffineVector &V, const TAffineVector &V1)/* overload */;
extern DELPHI_PACKAGE void __fastcall SortArrayAscending(System::Extended *a, const System::NativeInt a_High);
extern DELPHI_PACKAGE float __fastcall ClampValue(const float aValue, const float aMin, const float aMax)/* overload */;
extern DELPHI_PACKAGE float __fastcall ClampValue(const float aValue, const float aMin)/* overload */;
extern DELPHI_PACKAGE TAffineDblVector __fastcall MakeAffineDblVector(double *V, const System::NativeInt V_High);
extern DELPHI_PACKAGE THomogeneousDblVector __fastcall MakeDblVector(double *V, const System::NativeInt V_High);
extern DELPHI_PACKAGE bool __fastcall PointInPolygon(const float *xp, const System::NativeInt xp_High, const float *yp, const System::NativeInt yp_High, float X, float Y);
extern DELPHI_PACKAGE bool __fastcall IsPointInPolygon(const System::Types::TPoint *Polygon, const System::NativeInt Polygon_High, const System::Types::TPoint &p);
extern DELPHI_PACKAGE void __fastcall DivMod(int Dividend, System::Word Divisor, System::Word &result, System::Word &Remainder);
extern DELPHI_PACKAGE TVector __fastcall ConvertRotation(const TAffineVector &Angles);
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &QStart, const TQuaternion &QEnd, int Spin, float T)/* overload */;
extern DELPHI_PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &source, const TQuaternion &dest, const float T)/* overload */;
extern DELPHI_PACKAGE THomogeneousVector __fastcall VectorDblToFlt(const THomogeneousDblVector &V);
extern DELPHI_PACKAGE TAffineVector __fastcall VectorAffineDblToFlt(const TAffineDblVector &V);
extern DELPHI_PACKAGE TAffineDblVector __fastcall VectorAffineFltToDbl(const TAffineVector &V);
extern DELPHI_PACKAGE THomogeneousDblVector __fastcall VectorFltToDbl(const TVector &V);
extern DELPHI_PACKAGE TMatrix __fastcall Turn(const TMatrix &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Turn(const TMatrix &Matrix, const TAffineVector &MasterUp, float angle)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall Pitch(const TMatrix &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Pitch(const TMatrix &Matrix, const TAffineVector &MasterRight, float angle)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall Roll(const TMatrix &Matrix, float angle)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TMatrix4f __fastcall Roll(const TMatrix &Matrix, const TAffineVector &MasterDirection, float angle)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastPlaneIntersect(const TVector &rayStart, const TVector &rayVector, const TVector &planePoint, const TVector &planeNormal, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastPlaneXZIntersect(const TVector &rayStart, const TVector &rayVector, const float planeY, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastTriangleIntersect(const TVector &rayStart, const TVector &rayVector, const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3, PVector intersectPoint = (PVector)(0x0), PVector intersectNormal = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE float __fastcall RayCastMinDistToPoint(const TVector &rayStart, const TVector &rayVector, const TVector &point);
extern DELPHI_PACKAGE bool __fastcall RayCastIntersectsSphere(const TVector &rayStart, const TVector &rayVector, const TVector &sphereCenter, const float SphereRadius)/* overload */;
extern DELPHI_PACKAGE int __fastcall RayCastSphereIntersect(const TVector &rayStart, const TVector &rayVector, const TVector &sphereCenter, const float SphereRadius, TVector &i1, TVector &i2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall RayCastBoxIntersect(const TAffineVector &rayStart, const TAffineVector &rayVector, const TAffineVector &aMinExtent, const TAffineVector &aMaxExtent, PAffineVector intersectPoint = (PAffineVector)(0x0));
extern DELPHI_PACKAGE float __fastcall SphereVisibleRadius(float distance, float radius);
extern DELPHI_PACKAGE int __fastcall IntersectLinePlane(const TVector &point, const TVector &direction, const THmgPlane &plane, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectTriangleBox(const TAffineVector &p1, const TAffineVector &p2, const TAffineVector &p3, const TAffineVector &aMinExtent, const TAffineVector &aMaxExtent);
extern DELPHI_PACKAGE bool __fastcall IntersectSphereBox(const TVector &SpherePos, const float SphereRadius, const TMatrix &BoxMatrix, const TAffineVector &BoxScale, PAffineVector intersectPoint = (PAffineVector)(0x0), PAffineVector normal = (PAffineVector)(0x0), System::PSingle depth = (System::PSingle)(0x0));
extern DELPHI_PACKAGE TFrustum __fastcall ExtractFrustumFromModelViewProjection(const TMatrix &modelViewProj);
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const TAffineVector &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const TVector &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsVolumeClipped(const TAffineVector &min, const TAffineVector &max, const TFrustum &Frustum)/* overload */;
extern DELPHI_PACKAGE TMatrix __fastcall MakeParallelProjectionMatrix(const THmgPlane &plane, const TVector &dir);
extern DELPHI_PACKAGE TMatrix __fastcall MakeShadowMatrix(const TVector &planePoint, const TVector &planeNormal, const TVector &lightPos);
extern DELPHI_PACKAGE TMatrix __fastcall MakeReflectionMatrix(const TAffineVector &planePoint, const TAffineVector &planeNormal);
extern DELPHI_PACKAGE TPackedRotationMatrix __fastcall PackRotationMatrix(const TMatrix &mat);
extern DELPHI_PACKAGE TMatrix __fastcall UnPackRotationMatrix(const TPackedRotationMatrix &packedMatrix);
extern DELPHI_PACKAGE bool __fastcall BarycentricCoordinates(const TAffineVector &V1, const TAffineVector &V2, const TAffineVector &V3, const TAffineVector &p, float &u, float &V);
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall Vector2fMake(const float X, const float Y)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2i __fastcall Vector2iMake(const System::LongInt X, const System::LongInt Y)/* overload */;
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
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const System::LongInt X, const System::LongInt Y = 0x0, const System::LongInt Z = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Vector3fMake(const Glvectortypes::TVector2f &Vector, const float Z = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const Glvectortypes::TVector2i &Vector, const System::LongInt Z = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const Glvectortypes::TVector2s Vector, const short Z = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const Glvectortypes::TVector2d &Vector, const double Z = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const Glvectortypes::TVector2b Vector, const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall Vector3fMake(const Glvectortypes::TVector4f &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3i __fastcall Vector3iMake(const Glvectortypes::TVector4i &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3s __fastcall Vector3sMake(const Glvectortypes::TVector4s &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3d __fastcall Vector3dMake(const Glvectortypes::TVector4d &Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3b __fastcall Vector3bMake(const Glvectortypes::TVector4b Vector)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const float X, const float Y = 0.000000E+00f, const float Z = 0.000000E+00f, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const System::LongInt X, const System::LongInt Y = 0x0, const System::LongInt Z = 0x0, const System::LongInt W = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4s __fastcall Vector4sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0), const short W = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall Vector4dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00, const double W = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4b __fastcall Vector4bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0), const System::Byte W = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const Glvectortypes::TVector3f &Vector, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const Glvectortypes::TVector3i &Vector, const System::LongInt W = 0x0)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4s __fastcall Vector4sMake(const Glvectortypes::TVector3s &Vector, const short W = (short)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4d __fastcall Vector4dMake(const Glvectortypes::TVector3d &Vector, const double W = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4b __fastcall Vector4bMake(const Glvectortypes::TVector3b Vector, const System::Byte W = (System::Byte)(0x0))/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4f __fastcall Vector4fMake(const Glvectortypes::TVector2f &Vector, const float Z = 0.000000E+00f, const float W = 0.000000E+00f)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector4i __fastcall Vector4iMake(const Glvectortypes::TVector2i &Vector, const System::LongInt Z = 0x0, const System::LongInt W = 0x0)/* overload */;
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
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall GetSafeTurnAngle(const TVector &AOriginalPosition, const TVector &AOriginalUpVector, const TVector &ATargetPosition, const TVector &AMoveAroundTargetCenter)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector2f __fastcall GetSafeTurnAngle(const TAffineVector &AOriginalPosition, const TAffineVector &AOriginalUpVector, const TAffineVector &ATargetPosition, const TAffineVector &AMoveAroundTargetCenter)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall MoveObjectAround(const TVector &AMovingObjectPosition, const TVector &AMovingObjectUp, const TVector &ATargetPosition, float pitchDelta, float turnDelta);
extern DELPHI_PACKAGE float __fastcall AngleBetweenVectors(const TVector &a, const TVector &b, const TVector &ACenterPoint)/* overload */;
extern DELPHI_PACKAGE float __fastcall AngleBetweenVectors(const TAffineVector &a, const TAffineVector &b, const TAffineVector &ACenterPoint)/* overload */;
extern DELPHI_PACKAGE TVector __fastcall ShiftObjectFromCenter(const TVector &AOriginalPosition, const TVector &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall ShiftObjectFromCenter(const TAffineVector &AOriginalPosition, const TAffineVector &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
}	/* namespace Glvectorgeometry */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVECTORGEOMETRY)
using namespace Glvectorgeometry;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLVectorGeometryHPP
