// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTypes.pas' rev: 36.00 (Windows)

#ifndef GLTypesHPP
#define GLTypesHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Rtti.hpp>
#include <System.Math.hpp>
#include <System.Math.Vectors.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltypes
{
//-- forward type declarations -----------------------------------------------
struct TxQuaternion;
struct TxVector;
struct TxMatrix;
struct TxQuatHelper /* Helper for record 'TxQuaternion' */;
struct TxVecHelper /* Helper for record 'TxVector' */;
class DELPHICLASS TxDim;
struct TxVertex;
struct TxFace;
struct TxPoint2D;
struct TxPoint3D;
struct TxVoxel;
struct TxVector2D;
struct TxVector3D;
struct TxMatrix2D;
struct TxMatrix3D;
struct TxTriangle;
struct TxMesh2DVertex;
struct TxMesh3DVertex;
struct TxQuaternion3D;
struct TxBox;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Extended> TAbstractVector;

typedef System::DynamicArray<System::Extended> Gltypes__1;

typedef System::DynamicArray<System::DynamicArray<System::Extended> > TAbstractMatrix;

struct DECLSPEC_DRECORD TxQuaternion
{
public:
	System::Extended operator[](System::Byte index) { return this->Element[index]; }
	
private:
	System::StaticArray<System::Extended, 4> FData;
	void __fastcall SetElement(System::Byte Index, System::Extended Value);
	System::Extended __fastcall GetElement(System::Byte Index);
	
public:
	__fastcall TxQuaternion(TAbstractVector Q);
	static TxQuaternion __fastcall _op_Multiply(const TxQuaternion &Q1, const TxQuaternion &Q2);
	static TxQuaternion __fastcall _op_Multiply(const TxQuaternion &Q, System::Extended Sc);
	static TxQuaternion __fastcall _op_Multiply(System::Extended Scalar, const TxQuaternion &Q);
	static TxQuaternion __fastcall _op_Implicit(TAbstractVector V);
	TxQuaternion __fastcall Inv();
	TxQuaternion __fastcall TruncateSTI();
	__property System::Extended Element[System::Byte index] = {read=GetElement, write=SetElement};
	TxQuaternion() {}
	
	friend TxQuaternion operator *(const TxQuaternion &Q1, const TxQuaternion &Q2) { return TxQuaternion::_op_Multiply(Q1, Q2); }
	friend TxQuaternion operator *(const TxQuaternion &Q, System::Extended Sc) { return TxQuaternion::_op_Multiply(Q, Sc); }
	friend TxQuaternion operator *(System::Extended Scalar, const TxQuaternion &Q) { return TxQuaternion::_op_Multiply(Scalar, Q); }
	TxQuaternion& operator =(TAbstractVector V) { *this = TxQuaternion::_op_Implicit(V); return *this; }
};


typedef TxVector *PxVector;

struct DECLSPEC_DRECORD TxVector
{
public:
	System::Extended operator[](System::Word index) { return this->Elements[index]; }
	
private:
	TAbstractVector FData;
	System::Word FCount;
	void __fastcall SetElement(System::Word Index, System::Extended Value);
	System::Extended __fastcall GetElement(System::Word Index);
	void __fastcall CheckUnique();
	
public:
	__fastcall TxVector(System::Word ElementsCount)/* overload */;
	__fastcall TxVector(TAbstractVector V)/* overload */;
	static TxVector __fastcall _op_Addition(const TxVector &V1, const TxVector &V2);
	static TxVector __fastcall _op_Addition(const TxVector &V, System::Extended Scalar);
	static TxVector __fastcall _op_Addition(System::Extended Scalar, const TxVector &V);
	static TxVector __fastcall _op_Subtraction(const TxVector &V1, const TxVector &V2);
	static TxVector __fastcall _op_Subtraction(System::Extended Scalar, const TxVector &V);
	static TxVector __fastcall _op_Subtraction(const TxVector &V, System::Extended Scalar);
	static TxVector __fastcall _op_Multiply(const TxVector &V1, const TxVector &V2);
	static TxVector __fastcall _op_Multiply(const TxVector &V, System::Extended Scalar);
	static TxVector __fastcall _op_Multiply(System::Extended Scalar, const TxVector &V);
	static TxVector __fastcall _op_Division(const TxVector &V, System::Extended Scalar);
	static TxVector __fastcall _op_Division(const TxVector &V1, const TxVector &V2);
	static TxVector __fastcall _op_Implicit(TAbstractVector V);
	System::Extended __fastcall Norm();
	System::Extended __fastcall SumOfSquares();
	System::Extended __fastcall SumOfElments();
	TxVector __fastcall TruncateSTI();
	TxQuaternion __fastcall ToQuat();
	void __fastcall Fill(System::Extended Value);
	System::Extended __fastcall ScalarMult(const TxVector &V);
	__property System::Word Count = {read=FCount};
	__property System::Extended Elements[System::Word index] = {read=GetElement, write=SetElement};
	TxVector() {}
	
	friend TxVector operator +(const TxVector &V1, const TxVector &V2) { return TxVector::_op_Addition(V1, V2); }
	friend TxVector operator +(const TxVector &V, System::Extended Scalar) { return TxVector::_op_Addition(V, Scalar); }
	friend TxVector operator +(System::Extended Scalar, const TxVector &V) { return TxVector::_op_Addition(Scalar, V); }
	friend TxVector operator -(const TxVector &V1, const TxVector &V2) { return TxVector::_op_Subtraction(V1, V2); }
	friend TxVector operator -(System::Extended Scalar, const TxVector &V) { return TxVector::_op_Subtraction(Scalar, V); }
	friend TxVector operator -(const TxVector &V, System::Extended Scalar) { return TxVector::_op_Subtraction(V, Scalar); }
	friend TxVector operator *(const TxVector &V1, const TxVector &V2) { return TxVector::_op_Multiply(V1, V2); }
	friend TxVector operator *(const TxVector &V, System::Extended Scalar) { return TxVector::_op_Multiply(V, Scalar); }
	friend TxVector operator *(System::Extended Scalar, const TxVector &V) { return TxVector::_op_Multiply(Scalar, V); }
	friend TxVector operator /(const TxVector &V, System::Extended Scalar) { return TxVector::_op_Division(V, Scalar); }
	friend TxVector operator /(const TxVector &V1, const TxVector &V2) { return TxVector::_op_Division(V1, V2); }
	TxVector& operator =(TAbstractVector V) { *this = TxVector::_op_Implicit(V); return *this; }
};


typedef TxMatrix *PxMatrix;

struct DECLSPEC_DRECORD TxMatrix
{
private:
	TAbstractMatrix FData;
	System::Word FRowsCount;
	System::Word FColsCount;
	void __fastcall SetElement(System::Word Row, System::Word Col, System::Extended Value);
	System::Extended __fastcall GetElement(System::Word Row, System::Word Col);
	TxVector __fastcall GetRow(System::Word Row);
	void __fastcall SetRow(System::Word Row, const TxVector &Value);
	TxVector __fastcall GetCol(System::Word Col);
	void __fastcall SetCol(System::Word Col, const TxVector &Value);
	TxMatrix __fastcall Del(const TxMatrix &A, int I, int J, int M);
	System::Extended __fastcall Det(const TxMatrix &A, int M);
	void __fastcall CheckUnique();
	
public:
	__fastcall TxMatrix(System::Word RowsCount, System::Word ColsCount)/* overload */;
	__fastcall TxMatrix(System::Word Dim, System::Extended Value);
	__fastcall TxMatrix(TAbstractMatrix M)/* overload */;
	static TxMatrix __fastcall _op_Addition(const TxMatrix &M1, const TxMatrix &M2);
	static TxMatrix __fastcall _op_Subtraction(const TxMatrix &M1, const TxMatrix &M2);
	static TxMatrix __fastcall _op_Multiply(const TxMatrix &M1, const TxMatrix &M2);
	static TxVector __fastcall _op_Multiply(const TxMatrix &M, const TxVector &V);
	static TxVector __fastcall _op_Multiply(const TxVector &V, const TxMatrix &M);
	static TxMatrix __fastcall _op_Multiply(const TxMatrix &M, System::Extended Scalar);
	static TxMatrix __fastcall _op_Multiply(System::Extended Scalar, const TxMatrix &M);
	static TxQuaternion __fastcall _op_Multiply(const TxMatrix &M, const TxQuaternion &Q);
	static TxMatrix __fastcall _op_Implicit(TAbstractMatrix M);
	TxMatrix __fastcall Transp();
	TxMatrix __fastcall Inv();
	TxQuaternion __fastcall ToQuat();
	System::Extended __fastcall Determinant();
	TxMatrix __fastcall TruncateSTI();
	System::Extended __fastcall Trace();
	void __fastcall Fill(System::Extended Scalar);
	__property System::Word RowCount = {read=FRowsCount};
	__property System::Word ColCount = {read=FColsCount};
	__property TxVector Row[System::Word Row] = {read=GetRow, write=SetRow};
	__property TxVector Col[System::Word Col] = {read=GetCol, write=SetCol};
	__property System::Extended Elements[System::Word Row][System::Word Col] = {read=GetElement, write=SetElement};
	TxMatrix() {}
	
	friend TxMatrix operator +(const TxMatrix &M1, const TxMatrix &M2) { return TxMatrix::_op_Addition(M1, M2); }
	friend TxMatrix operator -(const TxMatrix &M1, const TxMatrix &M2) { return TxMatrix::_op_Subtraction(M1, M2); }
	friend TxMatrix operator *(const TxMatrix &M1, const TxMatrix &M2) { return TxMatrix::_op_Multiply(M1, M2); }
	friend TxVector operator *(const TxMatrix &M, const TxVector &V) { return TxMatrix::_op_Multiply(M, V); }
	friend TxVector operator *(const TxVector &V, const TxMatrix &M) { return TxMatrix::_op_Multiply(V, M); }
	friend TxMatrix operator *(const TxMatrix &M, System::Extended Scalar) { return TxMatrix::_op_Multiply(M, Scalar); }
	friend TxMatrix operator *(System::Extended Scalar, const TxMatrix &M) { return TxMatrix::_op_Multiply(Scalar, M); }
	friend TxQuaternion operator *(const TxMatrix &M, const TxQuaternion &Q) { return TxMatrix::_op_Multiply(M, Q); }
	TxMatrix& operator =(TAbstractMatrix M) { *this = TxMatrix::_op_Implicit(M); return *this; }
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TxDim : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	int FRowCount;
	int FColCount;
	
public:
	__fastcall TxDim(int ARowCount, int AColCount)/* overload */;
	__property int RowCount = {read=FRowCount, nodefault};
	__property int ColCount = {read=FColCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TxDim() { }
	
};

#pragma pack(pop)

typedef float TxScalarValue;

typedef TxScalarValue __fastcall (*TxScalarField)(float X, float Y, float Z);

typedef TxScalarValue __fastcall (__closure *TxScalarFieldInt)(int iX, int iY, int iZ);

struct DECLSPEC_DRECORD TxVertex
{
public:
	Glvectortypes::TVector3f P;
	Glvectortypes::TVector3f N;
	float Density;
};


struct DECLSPEC_DRECORD TxFace
{
public:
	Glvectortypes::TVector3f Normal;
	Glvectortypes::TVector3f V1;
	Glvectortypes::TVector3f V2;
	Glvectortypes::TVector3f V3;
	System::StaticArray<System::Byte, 2> Padding;
};


typedef TxPoint2D *PxPoint2D;

struct DECLSPEC_DRECORD TxPoint2D
{
public:
	float X;
	float Y;
	TxPoint2D __fastcall Create(float X, float Y);
	void __fastcall SetPosition(const float X, const float Y);
	TxPoint2D __fastcall Add(const TxPoint2D &APoint2D);
	float __fastcall Length();
	float __fastcall Distance(const TxPoint2D &APoint2D);
	static bool __fastcall PointInCircle(const TxPoint2D &Point, const TxPoint2D &Center, const int Radius);
	void __fastcall Offset(const float ADeltaX, const float ADeltaY);
};


typedef TxPoint3D *PxPoint3D;

struct DECLSPEC_DRECORD TxPoint3D
{
public:
	float X;
	float Y;
	float Z;
	TxPoint3D __fastcall Create(float X, float Y, float Z);
	void __fastcall SetPosition(const float X, const float Y, const float Z);
	TxPoint3D __fastcall Add(const TxPoint3D &AGLPoint3D);
	float __fastcall Length();
	float __fastcall Distance(const TxPoint3D &APoint3D);
	void __fastcall Offset(const float ADeltaX, const float ADeltaY, const float ADeltaZ);
};


typedef System::DynamicArray<TxPoint2D> TxPoint2DArray;

typedef System::DynamicArray<TxPoint3D> TxPoint3DArray;

enum DECLSPEC_DENUM TxVoxelStatus : unsigned char { bpExternal, bpInternal };

struct DECLSPEC_DRECORD TxVoxel
{
public:
	Glvectortypes::TVector3f P;
	TxScalarValue Density;
	TxVoxelStatus Status;
};


typedef TxVoxel *PxVoxel;

typedef System::StaticArray<TxVoxel, 8388608> TxVoxelData;

typedef TxVoxelData *PxVoxelData;

typedef System::StaticArray<float, 2> TxVector2DType;

typedef System::StaticArray<float, 3> TxVector3DType;

struct DECLSPEC_DRECORD TxVector2D
{
public:
	TxVector2D __fastcall Create(const float AX, const float AY, const float AW);
	TxVector2D __fastcall Add(const TxVector2D &AVector2D);
	float __fastcall Length();
	float __fastcall Norm();
	TxVector2D __fastcall Normalize();
	TxVector2D __fastcall CrossProduct(const TxVector2D &AVector);
	float __fastcall DotProduct(const TxVector2D &AVector);
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
			float W;
		};
		struct 
		{
			TxVector2DType V;
		};
		
	};
};


struct DECLSPEC_DRECORD TxVector3D
{
public:
	TxVector3D __fastcall Create(const float AX, const float AY, const float AZ, const float AW);
	TxVector3D __fastcall Add(const TxVector3D &AVector3D);
	float __fastcall Length();
	float __fastcall Norm();
	TxVector3D __fastcall Normalize();
	Glvectortypes::TVector3d __fastcall CrossProduct(const Glvectortypes::TVector3d &AVector3D);
	float __fastcall DotProduct(const Glvectortypes::TVector3d &AVector3D);
	
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
			TxVector3DType V;
		};
		
	};
};


typedef System::DynamicArray<TxVector2D> TxVector2DArray;

typedef System::DynamicArray<TxVector3D> TxVector3DArray;

typedef System::StaticArray<TxVector2D, 4> TxMatrix2DType;

typedef System::StaticArray<TxVector3D, 4> TxMatrix3DType;

struct DECLSPEC_DRECORD TxMatrix2D
{
	
public:
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e21;
			float e22;
			float e23;
			float e31;
			float e32;
			float e33;
		};
		struct 
		{
			TxMatrix2DType M;
		};
		
	};
};


struct DECLSPEC_DRECORD TxMatrix3D
{
	
public:
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e14;
			float e21;
			float e22;
			float e23;
			float e24;
			float e31;
			float e32;
			float e33;
			float e34;
			float e41;
			float e42;
			float e43;
			float e44;
		};
		struct 
		{
			TxMatrix3DType M;
		};
		
	};
};


typedef System::DynamicArray<TxMatrix2D> TxMatrix2DArray;

typedef System::DynamicArray<TxMatrix3D> TxMatrix3DArray;

typedef TxPoint2DArray TxPolygon2D;

typedef TxPoint3DArray TxPolygon3D;

typedef System::StaticArray<TxVertex, 8388608> TxVertexArray;

typedef TxVertexArray *PxVertexArray;

struct DECLSPEC_DRECORD TxTriangle
{
public:
	int v1;
	int v2;
	int v3;
};


typedef System::StaticArray<TxTriangle, 8388608> TxTriangleArray;

typedef TxTriangleArray *PxTriangleArray;

typedef System::DynamicArray<TxPoint3DArray> TxPolyhedron;

struct DECLSPEC_DRECORD TxMesh2DVertex
{
public:
	float X;
	float Y;
	float NX;
	float NY;
	float tU;
	float tV;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TxMesh3DVertex
{
public:
	float X;
	float Y;
	float Z;
	float NX;
	float NY;
	float NZ;
	float tU;
	float tV;
};
#pragma pack(pop)


typedef System::DynamicArray<TxMesh2DVertex> TxMesh2D;

typedef System::DynamicArray<TxMesh3DVertex> TxMesh3D;

struct DECLSPEC_DRECORD TxQuaternion3D
{
public:
	TxVector3D ImPart;
	float RePart;
};


typedef System::DynamicArray<TxQuaternion3D> TxQuaternionArray;

struct DECLSPEC_DRECORD TxBox
{
public:
	float ALeft;
	float ATop;
	float ANear;
	float ARight;
	float ABottom;
	float AFar;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TxPoint2D ClosedPolygon2D;
extern DELPHI_PACKAGE TxPoint3D ClosedPolygon3D;
#define sWRONG_ELEMENT L"Wrong element"
#define sWRONG_SIZE L"Wrong size"
#define sNOT_QUAD L"Matrix not quadratic"
#define sSINGULAR L"Singular matrix founded"
extern DELPHI_PACKAGE TxVector __fastcall TxVec(TAbstractVector V);
extern DELPHI_PACKAGE TxMatrix __fastcall TxMat(TAbstractMatrix M);
extern DELPHI_PACKAGE TxQuaternion __fastcall TxQuat(TAbstractVector Q);
extern DELPHI_PACKAGE void __fastcall Init(void * Obj, void * TypeInfoOfObj, int Offset = 0x0);
}	/* namespace Gltypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTYPES)
using namespace Gltypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLTypesHPP
