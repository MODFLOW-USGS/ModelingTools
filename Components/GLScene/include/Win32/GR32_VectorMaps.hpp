// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_VectorMaps.pas' rev: 35.00 (Windows)

#ifndef Gr32_vectormapsHPP
#define Gr32_vectormapsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <GR32.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_vectormaps
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVectorMap;
//-- type declarations -------------------------------------------------------
typedef Gr32::TFixedPoint TFixedVector;

typedef Gr32::TFixedPoint *PFixedVector;

typedef Gr32::TFloatPoint TFloatVector;

typedef Gr32::TFloatPoint *PFloatVector;

typedef System::DynamicArray<Gr32::TFixedPoint> TArrayOfFixedVector;

typedef TArrayOfFixedVector *PArrayOfFixedVector;

typedef System::DynamicArray<Gr32::TFloatPoint> TArrayOfFloatVector;

typedef TArrayOfFixedVector *PArrayOfFloatVector;

enum DECLSPEC_DENUM TVectorCombineMode : unsigned char { vcmAdd, vcmReplace, vcmCustom };

typedef void __fastcall (__closure *TVectorCombineEvent)(const Gr32::TFixedPoint &F, const Gr32::TFixedPoint &P, Gr32::TFixedPoint &B);

class PASCALIMPLEMENTATION TVectorMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	TArrayOfFixedVector FVectors;
	TVectorCombineEvent FOnVectorCombine;
	TVectorCombineMode FVectorCombineMode;
	Gr32::PFixedPointArray __fastcall GetVectors();
	Gr32::TFixedPoint __fastcall GetFixedVector(int X, int Y);
	Gr32::TFixedPoint __fastcall GetFixedVectorS(int X, int Y);
	Gr32::TFixedPoint __fastcall GetFixedVectorX(Gr32::TFixed X, Gr32::TFixed Y);
	Gr32::TFixedPoint __fastcall GetFixedVectorXS(Gr32::TFixed X, Gr32::TFixed Y);
	Gr32::TFloatPoint __fastcall GetFloatVector(int X, int Y);
	Gr32::TFloatPoint __fastcall GetFloatVectorS(int X, int Y);
	Gr32::TFloatPoint __fastcall GetFloatVectorF(float X, float Y);
	Gr32::TFloatPoint __fastcall GetFloatVectorFS(float X, float Y);
	void __fastcall SetFixedVector(int X, int Y, const Gr32::TFixedPoint &Point);
	void __fastcall SetFixedVectorS(int X, int Y, const Gr32::TFixedPoint &Point);
	void __fastcall SetFixedVectorX(Gr32::TFixed X, Gr32::TFixed Y, const Gr32::TFixedPoint &Point);
	void __fastcall SetFixedVectorXS(Gr32::TFixed X, Gr32::TFixed Y, const Gr32::TFixedPoint &Point);
	void __fastcall SetFloatVector(int X, int Y, const Gr32::TFloatPoint &Point);
	void __fastcall SetFloatVectorS(int X, int Y, const Gr32::TFloatPoint &Point);
	void __fastcall SetFloatVectorF(float X, float Y, const Gr32::TFloatPoint &Point);
	void __fastcall SetFloatVectorFS(float X, float Y, const Gr32::TFloatPoint &Point);
	void __fastcall SetVectorCombineMode(const TVectorCombineMode Value);
	
protected:
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TVectorMap();
	void __fastcall Clear();
	void __fastcall Merge(int DstLeft, int DstTop, TVectorMap* Src, const System::Types::TRect &SrcRect);
	__property Gr32::PFixedPointArray Vectors = {read=GetVectors};
	System::Types::TRect __fastcall BoundsRect();
	System::Types::TRect __fastcall GetTrimmedBounds();
	virtual bool __fastcall Empty();
	void __fastcall LoadFromFile(const System::UnicodeString FileName);
	void __fastcall SaveToFile(const System::UnicodeString FileName);
	__property Gr32::TFixedPoint FixedVector[int X][int Y] = {read=GetFixedVector, write=SetFixedVector/*, default*/};
	__property Gr32::TFixedPoint FixedVectorS[int X][int Y] = {read=GetFixedVectorS, write=SetFixedVectorS};
	__property Gr32::TFixedPoint FixedVectorX[Gr32::TFixed X][Gr32::TFixed Y] = {read=GetFixedVectorX, write=SetFixedVectorX};
	__property Gr32::TFixedPoint FixedVectorXS[Gr32::TFixed X][Gr32::TFixed Y] = {read=GetFixedVectorXS, write=SetFixedVectorXS};
	__property Gr32::TFloatPoint FloatVector[int X][int Y] = {read=GetFloatVector, write=SetFloatVector};
	__property Gr32::TFloatPoint FloatVectorS[int X][int Y] = {read=GetFloatVectorS, write=SetFloatVectorS};
	__property Gr32::TFloatPoint FloatVectorF[float X][float Y] = {read=GetFloatVectorF, write=SetFloatVectorF};
	__property Gr32::TFloatPoint FloatVectorFS[float X][float Y] = {read=GetFloatVectorFS, write=SetFloatVectorFS};
	
__published:
	__property TVectorCombineMode VectorCombineMode = {read=FVectorCombineMode, write=SetVectorCombineMode, nodefault};
	__property TVectorCombineEvent OnVectorCombine = {read=FOnVectorCombine, write=FOnVectorCombine};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TVectorMap() : Gr32::TCustomMap() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gr32_vectormaps */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_VECTORMAPS)
using namespace Gr32_vectormaps;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_vectormapsHPP
