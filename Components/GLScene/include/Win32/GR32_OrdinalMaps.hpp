// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_OrdinalMaps.pas' rev: 36.00 (Windows)

#ifndef Gr32_ordinalmapsHPP
#define Gr32_ordinalmapsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_ordinalmaps
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBooleanMap;
class DELPHICLASS TByteMap;
class DELPHICLASS TWordMap;
class DELPHICLASS TIntegerMap;
class DELPHICLASS TFloatMap;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TConversionType : unsigned char { ctRed, ctGreen, ctBlue, ctAlpha, ctUniformRGB, ctWeightedRGB };

class PASCALIMPLEMENTATION TBooleanMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	Gr32::TArrayOfByte FBits;
	bool __fastcall GetValue(int X, int Y);
	void __fastcall SetValue(int X, int Y, const bool Value);
	Gr32::PByteArray __fastcall GetBits();
	
protected:
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TBooleanMap();
	virtual bool __fastcall Empty();
	void __fastcall Clear(System::Byte FillValue);
	void __fastcall ToggleBit(int X, int Y);
	__property bool Value[int X][int Y] = {read=GetValue, write=SetValue/*, default*/};
	__property Gr32::PByteArray Bits = {read=GetBits};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TBooleanMap() : Gr32::TCustomMap() { }
	
};


class PASCALIMPLEMENTATION TByteMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	Gr32::TArrayOfByte FBits;
	System::Byte __fastcall GetValue(int X, int Y);
	Winapi::Windows::PByte __fastcall GetValPtr(int X, int Y);
	void __fastcall SetValue(int X, int Y, System::Byte Value);
	Gr32::PByteArray __fastcall GetBits();
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TByteMap();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall Empty();
	void __fastcall Clear(System::Byte FillValue);
	void __fastcall ReadFrom(Gr32::TCustomBitmap32* Source, TConversionType Conversion);
	void __fastcall WriteTo(Gr32::TCustomBitmap32* Dest, TConversionType Conversion)/* overload */;
	void __fastcall WriteTo(Gr32::TCustomBitmap32* Dest, const Gr32::TPalette32 &Palette)/* overload */;
	__property Gr32::PByteArray Bits = {read=GetBits};
	__property Winapi::Windows::PByte ValPtr[int X][int Y] = {read=GetValPtr};
	__property System::Byte Value[int X][int Y] = {read=GetValue, write=SetValue/*, default*/};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TByteMap() : Gr32::TCustomMap() { }
	
};


class PASCALIMPLEMENTATION TWordMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	Gr32::TArrayOfWord FBits;
	PWORD __fastcall GetValPtr(int X, int Y);
	System::Word __fastcall GetValue(int X, int Y);
	void __fastcall SetValue(int X, int Y, const System::Word Value);
	Gr32::PWordArray __fastcall GetBits();
	
protected:
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TWordMap();
	virtual bool __fastcall Empty();
	void __fastcall Clear(System::Word FillValue);
	__property PWORD ValPtr[int X][int Y] = {read=GetValPtr};
	__property System::Word Value[int X][int Y] = {read=GetValue, write=SetValue/*, default*/};
	__property Gr32::PWordArray Bits = {read=GetBits};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TWordMap() : Gr32::TCustomMap() { }
	
};


class PASCALIMPLEMENTATION TIntegerMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	Gr32::TArrayOfInteger FBits;
	System::PInteger __fastcall GetValPtr(int X, int Y);
	int __fastcall GetValue(int X, int Y);
	void __fastcall SetValue(int X, int Y, const int Value);
	Gr32::PIntegerArray __fastcall GetBits();
	
protected:
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TIntegerMap();
	virtual bool __fastcall Empty();
	void __fastcall Clear(int FillValue);
	__property System::PInteger ValPtr[int X][int Y] = {read=GetValPtr};
	__property int Value[int X][int Y] = {read=GetValue, write=SetValue/*, default*/};
	__property Gr32::PIntegerArray Bits = {read=GetBits};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TIntegerMap() : Gr32::TCustomMap() { }
	
};


class PASCALIMPLEMENTATION TFloatMap : public Gr32::TCustomMap
{
	typedef Gr32::TCustomMap inherited;
	
private:
	Gr32::TArrayOfFloat FBits;
	Gr32::PFloat __fastcall GetValPtr(int X, int Y);
	Gr32::TFloat __fastcall GetValue(int X, int Y);
	void __fastcall SetValue(int X, int Y, const Gr32::TFloat Value);
	Gr32::PFloatArray __fastcall GetBits();
	
protected:
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	__fastcall virtual ~TFloatMap();
	virtual bool __fastcall Empty();
	void __fastcall Clear()/* overload */;
	void __fastcall Clear(Gr32::TFloat FillValue)/* overload */;
	__property Gr32::PFloat ValPtr[int X][int Y] = {read=GetValPtr};
	__property Gr32::TFloat Value[int X][int Y] = {read=GetValue, write=SetValue/*, default*/};
	__property Gr32::PFloatArray Bits = {read=GetBits};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TFloatMap() : Gr32::TCustomMap() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gr32_ordinalmaps */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_ORDINALMAPS)
using namespace Gr32_ordinalmaps;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_ordinalmapsHPP
