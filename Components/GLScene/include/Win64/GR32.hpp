// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Gr32.pas' rev: 36.00 (Windows)

#ifndef Gr32HPP
#define Gr32HPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Gr32_system.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32
{
//-- forward type declarations -----------------------------------------------
struct TColor32Entry;
struct TFixedRec;
struct TFloatPoint;
struct TFixedPoint;
struct TFloatRect;
struct TFixedRect;
class DELPHICLASS TPlainInterfacedPersistent;
class DELPHICLASS TNotifiablePersistent;
class DELPHICLASS TThreadPersistent;
class DELPHICLASS TCustomMap;
class DELPHICLASS TCustomBitmap32;
class DELPHICLASS TBitmap32;
class DELPHICLASS TCustomBackend;
class DELPHICLASS TCustomSampler;
class DELPHICLASS TCustomResampler;
//-- type declarations -------------------------------------------------------
typedef unsigned TColor32;

typedef TColor32 *PColor32;

typedef System::StaticArray<TColor32, 1> TColor32Array;

typedef TColor32Array *PColor32Array;

typedef System::DynamicArray<TColor32> TArrayOfColor32;

enum DECLSPEC_DENUM TColor32Component : unsigned char { ccBlue, ccGreen, ccRed, ccAlpha };

typedef System::Set<TColor32Component, TColor32Component::ccBlue, TColor32Component::ccAlpha> TColor32Components;

typedef TColor32Entry *PColor32Entry;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TColor32Entry
{
	
public:
	union
	{
		struct 
		{
			System::StaticArray<System::Byte, 4> Components;
		};
		struct 
		{
			System::StaticArray<System::Byte, 4> Planes;
		};
		struct 
		{
			TColor32 ARGB;
		};
		struct 
		{
			System::Byte B;
			System::Byte G;
			System::Byte R;
			System::Byte A;
		};
		
	};
};
#pragma pack(pop)


typedef System::StaticArray<TColor32Entry, 1> TColor32EntryArray;

typedef TColor32EntryArray *PColor32EntryArray;

typedef System::DynamicArray<TColor32Entry> TArrayOfColor32Entry;

typedef System::StaticArray<TColor32, 256> TPalette32;

typedef TPalette32 *PPalette32;

typedef int TFixed;

typedef TFixed *PFixed;

typedef TFixedRec *PFixedRec;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TFixedRec
{
	
public:
	union
	{
		struct 
		{
			System::Word Frac;
			short Int;
		};
		struct 
		{
			TFixed Fixed;
		};
		
	};
};
#pragma pack(pop)


typedef System::StaticArray<TFixed, 1> TFixedArray;

typedef TFixedArray *PFixedArray;

typedef System::DynamicArray<TFixed> TArrayOfFixed;

typedef TArrayOfFixed *PArrayOfFixed;

typedef System::DynamicArray<TArrayOfFixed> TArrayOfArrayOfFixed;

typedef TArrayOfArrayOfFixed *PArrayOfArrayOfFixed;

typedef float *PFloat;

typedef float TFloat;

typedef System::StaticArray<System::Byte, 1> TByteArray;

typedef TByteArray *PByteArray;

typedef System::DynamicArray<System::Byte> TArrayOfByte;

typedef TArrayOfByte *PArrayOfByte;

typedef System::StaticArray<System::Word, 1> TWordArray;

typedef TWordArray *PWordArray;

typedef System::DynamicArray<System::Word> TArrayOfWord;

typedef TArrayOfWord *PArrayOfWord;

typedef System::StaticArray<int, 1> TIntegerArray;

typedef TIntegerArray *PIntegerArray;

typedef System::DynamicArray<int> TArrayOfInteger;

typedef TArrayOfInteger *PArrayOfInteger;

typedef System::DynamicArray<TArrayOfInteger> TArrayOfArrayOfInteger;

typedef TArrayOfArrayOfInteger *PArrayOfArrayOfInteger;

typedef System::StaticArray<float, 1> TSingleArray;

typedef TSingleArray *PSingleArray;

typedef System::DynamicArray<float> TArrayOfSingle;

typedef TArrayOfSingle *PArrayOfSingle;

typedef System::StaticArray<float, 1> TFloatArray;

typedef TFloatArray *PFloatArray;

typedef System::DynamicArray<float> TArrayOfFloat;

typedef TArrayOfFloat *PArrayOfFloat;

typedef System::Types::TPoint *PPoint;

using Winapi::Windows::TPoint;

typedef System::StaticArray<System::Types::TPoint, 1> TPointArray;

typedef TPointArray *PPointArray;

typedef System::DynamicArray<System::Types::TPoint> TArrayOfPoint;

typedef TArrayOfPoint *PArrayOfPoint;

typedef System::DynamicArray<TArrayOfPoint> TArrayOfArrayOfPoint;

typedef TArrayOfArrayOfPoint *PArrayOfArrayOfPoint;

typedef TFloatPoint *PFloatPoint;

struct DECLSPEC_DRECORD TFloatPoint
{
public:
	TFloat X;
	TFloat Y;
};


typedef System::StaticArray<TFloatPoint, 1> TFloatPointArray;

typedef TFloatPointArray *PFloatPointArray;

typedef System::DynamicArray<TFloatPoint> TArrayOfFloatPoint;

typedef TArrayOfFloatPoint *PArrayOfFloatPoint;

typedef System::DynamicArray<TArrayOfFloatPoint> TArrayOfArrayOfFloatPoint;

typedef TArrayOfArrayOfFloatPoint *PArrayOfArrayOfFloatPoint;

typedef TFixedPoint *PFixedPoint;

struct DECLSPEC_DRECORD TFixedPoint
{
public:
	TFixed X;
	TFixed Y;
};


typedef System::StaticArray<TFixedPoint, 1> TFixedPointArray;

typedef TFixedPointArray *PFixedPointArray;

typedef System::DynamicArray<TFixedPoint> TArrayOfFixedPoint;

typedef TArrayOfFixedPoint *PArrayOfFixedPoint;

typedef System::DynamicArray<TArrayOfFixedPoint> TArrayOfArrayOfFixedPoint;

typedef TArrayOfArrayOfFixedPoint *PArrayOfArrayOfFixedPoint;

using Winapi::Windows::PRect;

using Winapi::Windows::TRect;

typedef TFloatRect *PFloatRect;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TFloatRect
{
	
public:
	union
	{
		struct 
		{
			TFloatPoint TopLeft;
			TFloatPoint BottomRight;
		};
		struct 
		{
			TFloat Left;
			TFloat Top;
			TFloat Right;
			TFloat Bottom;
		};
		
	};
};
#pragma pack(pop)


typedef TFixedRect *PFixedRect;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TFixedRect
{
	
public:
	union
	{
		struct 
		{
			TFixedPoint TopLeft;
			TFixedPoint BottomRight;
		};
		struct 
		{
			TFixed Left;
			TFixed Top;
			TFixed Right;
			TFixed Bottom;
		};
		
	};
};
#pragma pack(pop)


enum DECLSPEC_DENUM TRectRounding : unsigned char { rrClosest, rrOutside, rrInside };

enum DECLSPEC_DENUM TDrawMode : unsigned char { dmOpaque, dmBlend, dmCustom, dmTransparent };

enum DECLSPEC_DENUM TCombineMode : unsigned char { cmBlend, cmMerge };

enum DECLSPEC_DENUM TWrapMode : unsigned char { wmClamp, wmRepeat, wmMirror };

typedef int __fastcall (*TWrapProc)(int Value, int Max);

typedef int __fastcall (*TWrapProcEx)(int Value, int Min, int Max);

class PASCALIMPLEMENTATION TPlainInterfacedPersistent : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FRefCounted;
	int FRefCount;
	
protected:
	int __stdcall _AddRef();
	int __stdcall _Release();
	virtual HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	__property bool RefCounted = {read=FRefCounted, write=FRefCounted, nodefault};
	
public:
	virtual void __fastcall AfterConstruction();
	virtual void __fastcall BeforeDestruction();
	__classmethod virtual System::TObject* __fastcall NewInstance();
	__property int RefCount = {read=FRefCount, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TPlainInterfacedPersistent() { }
	
public:
	/* TObject.Create */ inline __fastcall TPlainInterfacedPersistent() : System::Classes::TPersistent() { }
	
private:
	void *__IInterface;	// System::IInterface 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000000-0000-0000-C000-000000000046}
	operator System::_di_IInterface()
	{
		System::_di_IInterface intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator System::IInterface*(void) { return (System::IInterface*)&__IInterface; }
	#endif
	
};


class PASCALIMPLEMENTATION TNotifiablePersistent : public TPlainInterfacedPersistent
{
	typedef TPlainInterfacedPersistent inherited;
	
private:
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnChange;
	
protected:
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	
public:
	virtual void __fastcall Changed();
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TNotifiablePersistent() { }
	
public:
	/* TObject.Create */ inline __fastcall TNotifiablePersistent() : TPlainInterfacedPersistent() { }
	
};


class PASCALIMPLEMENTATION TThreadPersistent : public TNotifiablePersistent
{
	typedef TNotifiablePersistent inherited;
	
private:
	int FLockCount;
	
protected:
	Winapi::Windows::TRTLCriticalSection FLock;
	__property int LockCount = {read=FLockCount, nodefault};
	
public:
	__fastcall virtual TThreadPersistent();
	__fastcall virtual ~TThreadPersistent();
	void __fastcall Lock();
	void __fastcall Unlock();
};


class PASCALIMPLEMENTATION TCustomMap : public TThreadPersistent
{
	typedef TThreadPersistent inherited;
	
protected:
	int FHeight;
	int FWidth;
	System::Classes::TNotifyEvent FOnResize;
	virtual void __fastcall SetHeight(int NewHeight);
	virtual void __fastcall SetWidth(int NewWidth);
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	
public:
	virtual void __fastcall Delete();
	virtual bool __fastcall Empty();
	virtual void __fastcall Resized();
	bool __fastcall SetSizeFrom(System::Classes::TPersistent* Source);
	virtual bool __fastcall SetSize(int NewWidth, int NewHeight);
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
public:
	/* TThreadPersistent.Create */ inline __fastcall virtual TCustomMap() : TThreadPersistent() { }
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TCustomMap() { }
	
};


typedef void __fastcall (__closure *TPixelCombineEvent)(TColor32 F, TColor32 &B, TColor32 M);

typedef void __fastcall (__closure *TAreaChangedEvent)(System::TObject* Sender, const TRect &Area, const unsigned Info);

_DECLARE_METACLASS(System::TMetaClass, TCustomBackendClass);

class PASCALIMPLEMENTATION TCustomBitmap32 : public TCustomMap
{
	typedef TCustomMap inherited;
	
private:
	TCustomBackend* FBackend;
	PColor32Array FBits;
	TRect FClipRect;
	TFixedRect FFixedClipRect;
	TRect F256ClipRect;
	bool FClipping;
	TDrawMode FDrawMode;
	TCombineMode FCombineMode;
	TWrapMode FWrapMode;
	unsigned FMasterAlpha;
	TColor32 FOuterColor;
	TColor32 FPenColor;
	float FStippleCounter;
	TArrayOfColor32 FStipplePattern;
	float FStippleStep;
	TPixelCombineEvent FOnPixelCombine;
	TAreaChangedEvent FOnAreaChanged;
	TAreaChangedEvent FOldOnAreaChanged;
	bool FMeasuringMode;
	TCustomResampler* FResampler;
	virtual void __fastcall BackendChangedHandler(System::TObject* Sender);
	virtual void __fastcall BackendChangingHandler(System::TObject* Sender);
	PColor32 __fastcall GetPixelPtr(int X, int Y);
	PColor32Array __fastcall GetScanLine(int Y);
	void __fastcall SetCombineMode(const TCombineMode Value);
	void __fastcall SetDrawMode(TDrawMode Value);
	void __fastcall SetWrapMode(TWrapMode Value);
	void __fastcall SetMasterAlpha(unsigned Value);
	void __fastcall SetClipRect(const TRect &Value);
	void __fastcall SetResampler(TCustomResampler* Resampler);
	System::UnicodeString __fastcall GetResamplerClassName();
	void __fastcall SetResamplerClassName(System::UnicodeString Value);
	
protected:
	TWrapProcEx WrapProcHorz;
	TWrapProcEx WrapProcVert;
	void *BlendProc;
	int RasterX;
	int RasterY;
	TFixed RasterXF;
	TFixed RasterYF;
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight);
	virtual void __fastcall CopyMapTo(TCustomBitmap32* Dst);
	virtual void __fastcall CopyPropertiesTo(TCustomBitmap32* Dst);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	bool __fastcall Equal(TCustomBitmap32* B);
	void __fastcall SET_T256(int X, int Y, TColor32 C);
	void __fastcall SET_TS256(int X, int Y, TColor32 C);
	TColor32 __fastcall GET_T256(int X, int Y);
	TColor32 __fastcall GET_TS256(int X, int Y);
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall InitializeBackend();
	virtual void __fastcall FinalizeBackend();
	virtual void __fastcall SetBackend(TCustomBackend* const Backend);
	virtual HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	TColor32 __fastcall GetPixel(int X, int Y);
	TColor32 __fastcall GetPixelS(int X, int Y);
	TColor32 __fastcall GetPixelW(int X, int Y);
	TColor32 __fastcall GetPixelF(float X, float Y);
	TColor32 __fastcall GetPixelFS(float X, float Y);
	TColor32 __fastcall GetPixelFW(float X, float Y);
	TColor32 __fastcall GetPixelX(TFixed X, TFixed Y);
	TColor32 __fastcall GetPixelXS(TFixed X, TFixed Y);
	TColor32 __fastcall GetPixelXW(TFixed X, TFixed Y);
	TColor32 __fastcall GetPixelFR(float X, float Y);
	TColor32 __fastcall GetPixelXR(TFixed X, TFixed Y);
	TColor32 __fastcall GetPixelB(int X, int Y);
	void __fastcall SetPixel(int X, int Y, TColor32 Value);
	void __fastcall SetPixelS(int X, int Y, TColor32 Value);
	void __fastcall SetPixelW(int X, int Y, TColor32 Value);
	void __fastcall SetPixelF(float X, float Y, TColor32 Value);
	void __fastcall SetPixelFS(float X, float Y, TColor32 Value);
	void __fastcall SetPixelFW(float X, float Y, TColor32 Value);
	void __fastcall SetPixelX(TFixed X, TFixed Y, TColor32 Value);
	void __fastcall SetPixelXS(TFixed X, TFixed Y, TColor32 Value);
	void __fastcall SetPixelXW(TFixed X, TFixed Y, TColor32 Value);
	
public:
	__fastcall virtual TCustomBitmap32();
	__fastcall virtual ~TCustomBitmap32();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TRect __fastcall BoundsRect();
	virtual bool __fastcall Empty();
	void __fastcall Clear()/* overload */;
	void __fastcall Clear(TColor32 FillColor)/* overload */;
	virtual void __fastcall Delete();
	void __fastcall BeginMeasuring(const TAreaChangedEvent Callback);
	void __fastcall EndMeasuring();
	TCustomBackend* __fastcall ReleaseBackend();
	virtual void __fastcall PropertyChanged();
	virtual void __fastcall Changed()/* overload */;
	HIDESBASE virtual void __fastcall Changed(const TRect &Area, const unsigned Info = (unsigned)(0x80000000))/* overload */;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream, bool SaveTopDown = false);
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName);
	virtual void __fastcall SaveToFile(const System::UnicodeString FileName, bool SaveTopDown = false);
	void __fastcall LoadFromResourceID(Winapi::Windows::THandle Instance, int ResID);
	void __fastcall LoadFromResourceName(Winapi::Windows::THandle Instance, const System::UnicodeString ResName);
	void __fastcall ResetAlpha()/* overload */;
	void __fastcall ResetAlpha(const System::Byte AlphaValue)/* overload */;
	void __fastcall Draw(int DstX, int DstY, TCustomBitmap32* Src)/* overload */;
	void __fastcall Draw(int DstX, int DstY, const TRect &SrcRect, TCustomBitmap32* Src)/* overload */;
	void __fastcall Draw(const TRect &DstRect, const TRect &SrcRect, TCustomBitmap32* Src)/* overload */;
	void __fastcall SetPixelT(int X, int Y, TColor32 Value)/* overload */;
	void __fastcall SetPixelT(PColor32 &Ptr, TColor32 Value)/* overload */;
	void __fastcall SetPixelTS(int X, int Y, TColor32 Value);
	void __fastcall DrawTo(TCustomBitmap32* Dst)/* overload */;
	void __fastcall DrawTo(TCustomBitmap32* Dst, int DstX, int DstY, const TRect &SrcRect)/* overload */;
	void __fastcall DrawTo(TCustomBitmap32* Dst, int DstX, int DstY)/* overload */;
	void __fastcall DrawTo(TCustomBitmap32* Dst, const TRect &DstRect)/* overload */;
	void __fastcall DrawTo(TCustomBitmap32* Dst, const TRect &DstRect, const TRect &SrcRect)/* overload */;
	void __fastcall SetStipple(TArrayOfColor32 NewStipple)/* overload */;
	void __fastcall SetStipple(TColor32 *NewStipple, const System::NativeInt NewStipple_High)/* overload */;
	void __fastcall AdvanceStippleCounter(float LengthPixels);
	TColor32 __fastcall GetStippleColor();
	void __fastcall HorzLine(int X1, int Y, int X2, TColor32 Value);
	void __fastcall HorzLineS(int X1, int Y, int X2, TColor32 Value);
	void __fastcall HorzLineT(int X1, int Y, int X2, TColor32 Value);
	void __fastcall HorzLineTS(int X1, int Y, int X2, TColor32 Value);
	void __fastcall HorzLineTSP(int X1, int Y, int X2);
	void __fastcall HorzLineX(TFixed X1, TFixed Y, TFixed X2, TColor32 Value);
	void __fastcall HorzLineXS(TFixed X1, TFixed Y, TFixed X2, TColor32 Value);
	void __fastcall VertLine(int X, int Y1, int Y2, TColor32 Value);
	void __fastcall VertLineS(int X, int Y1, int Y2, TColor32 Value);
	void __fastcall VertLineT(int X, int Y1, int Y2, TColor32 Value);
	void __fastcall VertLineTS(int X, int Y1, int Y2, TColor32 Value);
	void __fastcall VertLineTSP(int X, int Y1, int Y2);
	void __fastcall VertLineX(TFixed X, TFixed Y1, TFixed Y2, TColor32 Value);
	void __fastcall VertLineXS(TFixed X, TFixed Y1, TFixed Y2, TColor32 Value);
	void __fastcall Line(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineS(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineT(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineTS(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineA(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineAS(int X1, int Y1, int X2, int Y2, TColor32 Value, bool L = false);
	void __fastcall LineX(TFixed X1, TFixed Y1, TFixed X2, TFixed Y2, TColor32 Value, bool L = false)/* overload */;
	void __fastcall LineF(float X1, float Y1, float X2, float Y2, TColor32 Value, bool L = false)/* overload */;
	void __fastcall LineXS(TFixed X1, TFixed Y1, TFixed X2, TFixed Y2, TColor32 Value, bool L = false)/* overload */;
	void __fastcall LineFS(float X1, float Y1, float X2, float Y2, TColor32 Value, bool L = false)/* overload */;
	void __fastcall LineXP(TFixed X1, TFixed Y1, TFixed X2, TFixed Y2, bool L = false)/* overload */;
	void __fastcall LineFP(float X1, float Y1, float X2, float Y2, bool L = false)/* overload */;
	void __fastcall LineXSP(TFixed X1, TFixed Y1, TFixed X2, TFixed Y2, bool L = false)/* overload */;
	void __fastcall LineFSP(float X1, float Y1, float X2, float Y2, bool L = false)/* overload */;
	__property TColor32 PenColor = {read=FPenColor, write=FPenColor, nodefault};
	void __fastcall MoveTo(int X, int Y);
	void __fastcall LineToS(int X, int Y);
	void __fastcall LineToTS(int X, int Y);
	void __fastcall LineToAS(int X, int Y);
	void __fastcall MoveToX(TFixed X, TFixed Y);
	void __fastcall MoveToF(float X, float Y);
	void __fastcall LineToXS(TFixed X, TFixed Y);
	void __fastcall LineToFS(float X, float Y);
	void __fastcall LineToXSP(TFixed X, TFixed Y);
	void __fastcall LineToFSP(float X, float Y);
	void __fastcall FillRect(int X1, int Y1, int X2, int Y2, TColor32 Value);
	void __fastcall FillRectS(int X1, int Y1, int X2, int Y2, TColor32 Value)/* overload */;
	void __fastcall FillRectT(int X1, int Y1, int X2, int Y2, TColor32 Value);
	void __fastcall FillRectTS(int X1, int Y1, int X2, int Y2, TColor32 Value)/* overload */;
	void __fastcall FillRectS(const TRect &ARect, TColor32 Value)/* overload */;
	void __fastcall FillRectTS(const TRect &ARect, TColor32 Value)/* overload */;
	void __fastcall FrameRectS(int X1, int Y1, int X2, int Y2, TColor32 Value)/* overload */;
	void __fastcall FrameRectTS(int X1, int Y1, int X2, int Y2, TColor32 Value)/* overload */;
	void __fastcall FrameRectTSP(int X1, int Y1, int X2, int Y2);
	void __fastcall FrameRectS(const TRect &ARect, TColor32 Value)/* overload */;
	void __fastcall FrameRectTS(const TRect &ARect, TColor32 Value)/* overload */;
	void __fastcall RaiseRectTS(int X1, int Y1, int X2, int Y2, int Contrast)/* overload */;
	void __fastcall RaiseRectTS(const TRect &ARect, int Contrast)/* overload */;
	void __fastcall Roll(int Dx, int Dy, bool FillBack, TColor32 FillColor);
	void __fastcall FlipHorz(TCustomBitmap32* Dst = (TCustomBitmap32*)(0x0));
	void __fastcall FlipVert(TCustomBitmap32* Dst = (TCustomBitmap32*)(0x0));
	void __fastcall Rotate90(TCustomBitmap32* Dst = (TCustomBitmap32*)(0x0));
	void __fastcall Rotate180(TCustomBitmap32* Dst = (TCustomBitmap32*)(0x0));
	void __fastcall Rotate270(TCustomBitmap32* Dst = (TCustomBitmap32*)(0x0));
	void __fastcall ResetClipRect();
	__property TColor32 Pixel[int X][int Y] = {read=GetPixel, write=SetPixel/*, default*/};
	__property TColor32 PixelS[int X][int Y] = {read=GetPixelS, write=SetPixelS};
	__property TColor32 PixelW[int X][int Y] = {read=GetPixelW, write=SetPixelW};
	__property TColor32 PixelX[TFixed X][TFixed Y] = {read=GetPixelX, write=SetPixelX};
	__property TColor32 PixelXS[TFixed X][TFixed Y] = {read=GetPixelXS, write=SetPixelXS};
	__property TColor32 PixelXW[TFixed X][TFixed Y] = {read=GetPixelXW, write=SetPixelXW};
	__property TColor32 PixelF[float X][float Y] = {read=GetPixelF, write=SetPixelF};
	__property TColor32 PixelFS[float X][float Y] = {read=GetPixelFS, write=SetPixelFS};
	__property TColor32 PixelFW[float X][float Y] = {read=GetPixelFW, write=SetPixelFW};
	__property TColor32 PixelFR[float X][float Y] = {read=GetPixelFR};
	__property TColor32 PixelXR[TFixed X][TFixed Y] = {read=GetPixelXR};
	__property TCustomBackend* Backend = {read=FBackend, write=SetBackend};
	__property PColor32Array Bits = {read=FBits};
	__property TRect ClipRect = {read=FClipRect, write=SetClipRect};
	__property bool Clipping = {read=FClipping, nodefault};
	__property PColor32 PixelPtr[int X][int Y] = {read=GetPixelPtr};
	__property PColor32Array ScanLine[int Y] = {read=GetScanLine};
	__property float StippleCounter = {read=FStippleCounter, write=FStippleCounter};
	__property float StippleStep = {read=FStippleStep, write=FStippleStep};
	__property bool MeasuringMode = {read=FMeasuringMode, nodefault};
	
__published:
	__property TDrawMode DrawMode = {read=FDrawMode, write=SetDrawMode, default=0};
	__property TCombineMode CombineMode = {read=FCombineMode, write=SetCombineMode, default=0};
	__property TWrapMode WrapMode = {read=FWrapMode, write=SetWrapMode, default=0};
	__property unsigned MasterAlpha = {read=FMasterAlpha, write=SetMasterAlpha, default=255};
	__property TColor32 OuterColor = {read=FOuterColor, write=FOuterColor, default=0};
	__property System::UnicodeString ResamplerClassName = {read=GetResamplerClassName, write=SetResamplerClassName};
	__property TCustomResampler* Resampler = {read=FResampler, write=SetResampler};
	__property OnChange;
	__property TPixelCombineEvent OnPixelCombine = {read=FOnPixelCombine, write=FOnPixelCombine};
	__property TAreaChangedEvent OnAreaChanged = {read=FOnAreaChanged, write=FOnAreaChanged};
	__property OnResize;
};


class PASCALIMPLEMENTATION TBitmap32 : public TCustomBitmap32
{
	typedef TCustomBitmap32 inherited;
	
private:
	System::Classes::TNotifyEvent FOnHandleChanged;
	virtual void __fastcall BackendChangedHandler(System::TObject* Sender);
	virtual void __fastcall BackendChangingHandler(System::TObject* Sender);
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall CanvasChanged(System::TObject* Sender);
	Vcl::Graphics::TCanvas* __fastcall GetCanvas();
	Winapi::Windows::TBitmapInfo __fastcall GetBitmapInfo();
	HBITMAP __fastcall GetHandle();
	HDC __fastcall GetHDC();
	Vcl::Graphics::TFont* __fastcall GetFont();
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	
protected:
	virtual void __fastcall InitializeBackend();
	virtual void __fastcall FinalizeBackend();
	virtual void __fastcall SetBackend(TCustomBackend* const Backend);
	virtual void __fastcall HandleChanged();
	virtual void __fastcall CopyPropertiesTo(TCustomBitmap32* Dst);
	
public:
	HIDESBASE void __fastcall Draw(const TRect &DstRect, const TRect &SrcRect, HDC hSrc)/* overload */;
	HIDESBASE void __fastcall DrawTo(HDC hDst, int DstX, int DstY)/* overload */;
	HIDESBASE void __fastcall DrawTo(HDC hDst, const TRect &DstRect, const TRect &SrcRect)/* overload */;
	void __fastcall TileTo(HDC hDst, const TRect &DstRect, const TRect &SrcRect);
	void __fastcall UpdateFont();
	void __fastcall Textout(int X, int Y, const System::UnicodeString Text)/* overload */;
	void __fastcall Textout(int X, int Y, const TRect &ClipRect, const System::UnicodeString Text)/* overload */;
	void __fastcall Textout(const TRect &DstRect, const unsigned Flags, const System::UnicodeString Text)/* overload */;
	System::Types::TSize __fastcall TextExtent(const System::UnicodeString Text);
	int __fastcall TextHeight(const System::UnicodeString Text);
	int __fastcall TextWidth(const System::UnicodeString Text);
	void __fastcall RenderText(int X, int Y, const System::UnicodeString Text, int AALevel, TColor32 Color);
	void __fastcall TextoutW(int X, int Y, const System::WideString Text)/* overload */;
	void __fastcall TextoutW(int X, int Y, const TRect &ClipRect, const System::WideString Text)/* overload */;
	void __fastcall TextoutW(const TRect &DstRect, const unsigned Flags, const System::WideString Text)/* overload */;
	System::Types::TSize __fastcall TextExtentW(const System::WideString Text);
	int __fastcall TextHeightW(const System::WideString Text);
	int __fastcall TextWidthW(const System::WideString Text);
	void __fastcall RenderTextW(int X, int Y, const System::WideString Text, int AALevel, TColor32 Color);
	__property Vcl::Graphics::TCanvas* Canvas = {read=GetCanvas};
	bool __fastcall CanvasAllocated();
	void __fastcall DeleteCanvas();
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property HBITMAP BitmapHandle = {read=GetHandle};
	__property Winapi::Windows::TBitmapInfo BitmapInfo = {read=GetBitmapInfo};
	__property HDC Handle = {read=GetHDC};
	
__published:
	__property System::Classes::TNotifyEvent OnHandleChanged = {read=FOnHandleChanged, write=FOnHandleChanged};
public:
	/* TCustomBitmap32.Create */ inline __fastcall virtual TBitmap32() : TCustomBitmap32() { }
	/* TCustomBitmap32.Destroy */ inline __fastcall virtual ~TBitmap32() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Draw(int DstX, int DstY, TCustomBitmap32* Src){ TCustomBitmap32::Draw(DstX, DstY, Src); }
	inline void __fastcall  Draw(int DstX, int DstY, const TRect &SrcRect, TCustomBitmap32* Src){ TCustomBitmap32::Draw(DstX, DstY, SrcRect, Src); }
	inline void __fastcall  Draw(const TRect &DstRect, const TRect &SrcRect, TCustomBitmap32* Src){ TCustomBitmap32::Draw(DstRect, SrcRect, Src); }
	inline void __fastcall  DrawTo(TCustomBitmap32* Dst){ TCustomBitmap32::DrawTo(Dst); }
	inline void __fastcall  DrawTo(TCustomBitmap32* Dst, int DstX, int DstY, const TRect &SrcRect){ TCustomBitmap32::DrawTo(Dst, DstX, DstY, SrcRect); }
	inline void __fastcall  DrawTo(TCustomBitmap32* Dst, int DstX, int DstY){ TCustomBitmap32::DrawTo(Dst, DstX, DstY); }
	inline void __fastcall  DrawTo(TCustomBitmap32* Dst, const TRect &DstRect){ TCustomBitmap32::DrawTo(Dst, DstRect); }
	inline void __fastcall  DrawTo(TCustomBitmap32* Dst, const TRect &DstRect, const TRect &SrcRect){ TCustomBitmap32::DrawTo(Dst, DstRect, SrcRect); }
	
};


class PASCALIMPLEMENTATION TCustomBackend : public TThreadPersistent
{
	typedef TThreadPersistent inherited;
	
protected:
	PColor32Array FBits;
	TCustomBitmap32* FOwner;
	System::Classes::TNotifyEvent FOnChanging;
	virtual void __fastcall Changing();
	virtual void __fastcall InitializeSurface(int NewWidth, int NewHeight, bool ClearBuffer);
	virtual void __fastcall FinalizeSurface();
	
public:
	__fastcall virtual TCustomBackend()/* overload */;
	__fastcall virtual TCustomBackend(TCustomBitmap32* Owner)/* overload */;
	__fastcall virtual ~TCustomBackend();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual bool __fastcall Empty();
	virtual void __fastcall ChangeSize(int &Width, int &Height, int NewWidth, int NewHeight, bool ClearBuffer = true);
	__property PColor32Array Bits = {read=FBits};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
};


class PASCALIMPLEMENTATION TCustomSampler : public TNotifiablePersistent
{
	typedef TNotifiablePersistent inherited;
	
public:
	virtual TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual TColor32 __fastcall GetSampleFixed(TFixed X, TFixed Y);
	virtual TColor32 __fastcall GetSampleFloat(TFloat X, TFloat Y);
	virtual void __fastcall PrepareSampling();
	virtual void __fastcall FinalizeSampling();
	virtual bool __fastcall HasBounds();
	virtual TFloatRect __fastcall GetSampleBounds();
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomSampler() { }
	
public:
	/* TObject.Create */ inline __fastcall TCustomSampler() : TNotifiablePersistent() { }
	
};


enum DECLSPEC_DENUM TPixelAccessMode : unsigned char { pamUnsafe, pamSafe, pamWrap, pamTransparentEdge };

class PASCALIMPLEMENTATION TCustomResampler : public TCustomSampler
{
	typedef TCustomSampler inherited;
	
private:
	TCustomBitmap32* FBitmap;
	TRect FClipRect;
	TPixelAccessMode FPixelAccessMode;
	void __fastcall SetPixelAccessMode(const TPixelAccessMode Value);
	
protected:
	virtual TFloat __fastcall GetWidth();
	virtual void __fastcall Resample(TCustomBitmap32* Dst, const TRect &DstRect, const TRect &DstClip, TCustomBitmap32* Src, const TRect &SrcRect, TDrawMode CombineOp, TPixelCombineEvent CombineCallBack) = 0 ;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	__property TRect ClipRect = {read=FClipRect};
	
public:
	__fastcall virtual TCustomResampler()/* overload */;
	__fastcall virtual TCustomResampler(TCustomBitmap32* ABitmap)/* overload */;
	virtual void __fastcall Changed();
	virtual void __fastcall PrepareSampling();
	virtual bool __fastcall HasBounds();
	virtual TFloatRect __fastcall GetSampleBounds();
	__property TCustomBitmap32* Bitmap = {read=FBitmap, write=FBitmap};
	__property TFloat Width = {read=GetWidth};
	
__published:
	__property TPixelAccessMode PixelAccessMode = {read=FPixelAccessMode, write=SetPixelAccessMode, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomResampler() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TCustomResamplerClass);

//-- var, const, procedure ---------------------------------------------------
#define Graphics32Version L"1.9.1"
static _DELPHI_CONST TColor32 clBlack32 = TColor32(0xff000000);
static _DELPHI_CONST TColor32 clDimGray32 = TColor32(0xff3f3f3f);
static _DELPHI_CONST TColor32 clGray32 = TColor32(0xff7f7f7f);
static _DELPHI_CONST TColor32 clLightGray32 = TColor32(0xffbfbfbf);
static _DELPHI_CONST TColor32 clWhite32 = TColor32(0xffffffff);
static _DELPHI_CONST TColor32 clMaroon32 = TColor32(0xff7f0000);
static _DELPHI_CONST TColor32 clGreen32 = TColor32(0xff007f00);
static _DELPHI_CONST TColor32 clOlive32 = TColor32(0xff7f7f00);
static _DELPHI_CONST TColor32 clNavy32 = TColor32(0xff00007f);
static _DELPHI_CONST TColor32 clPurple32 = TColor32(0xff7f007f);
static _DELPHI_CONST TColor32 clTeal32 = TColor32(0xff007f7f);
static _DELPHI_CONST TColor32 clRed32 = TColor32(0xffff0000);
static _DELPHI_CONST TColor32 clLime32 = TColor32(0xff00ff00);
static _DELPHI_CONST TColor32 clYellow32 = TColor32(0xffffff00);
static _DELPHI_CONST TColor32 clBlue32 = TColor32(0xff0000ff);
static _DELPHI_CONST TColor32 clFuchsia32 = TColor32(0xffff00ff);
static _DELPHI_CONST TColor32 clAqua32 = TColor32(0xff00ffff);
static _DELPHI_CONST TColor32 clAliceBlue32 = TColor32(0xfff0f8ff);
static _DELPHI_CONST TColor32 clAntiqueWhite32 = TColor32(0xfffaebd7);
static _DELPHI_CONST TColor32 clAquamarine32 = TColor32(0xff7fffd4);
static _DELPHI_CONST TColor32 clAzure32 = TColor32(0xfff0ffff);
static _DELPHI_CONST TColor32 clBeige32 = TColor32(0xfff5f5dc);
static _DELPHI_CONST TColor32 clBisque32 = TColor32(0xffffe4c4);
static _DELPHI_CONST TColor32 clBlancheDalmond32 = TColor32(0xffffebcd);
static _DELPHI_CONST TColor32 clBlueViolet32 = TColor32(0xff8a2be2);
static _DELPHI_CONST TColor32 clBrown32 = TColor32(0xffa52a2a);
static _DELPHI_CONST TColor32 clBurlyWood32 = TColor32(0xffdeb887);
static _DELPHI_CONST TColor32 clCadetblue32 = TColor32(0xff5f9ea0);
static _DELPHI_CONST TColor32 clChartReuse32 = TColor32(0xff7fff00);
static _DELPHI_CONST TColor32 clChocolate32 = TColor32(0xffd2691e);
static _DELPHI_CONST TColor32 clCoral32 = TColor32(0xffff7f50);
static _DELPHI_CONST TColor32 clCornFlowerBlue32 = TColor32(0xff6495ed);
static _DELPHI_CONST TColor32 clCornSilk32 = TColor32(0xfffff8dc);
static _DELPHI_CONST TColor32 clCrimson32 = TColor32(0xffdc143c);
static _DELPHI_CONST TColor32 clDarkBlue32 = TColor32(0xff00008b);
static _DELPHI_CONST TColor32 clDarkCyan32 = TColor32(0xff008b8b);
static _DELPHI_CONST TColor32 clDarkGoldenRod32 = TColor32(0xffb8860b);
static _DELPHI_CONST TColor32 clDarkGray32 = TColor32(0xffa9a9a9);
static _DELPHI_CONST TColor32 clDarkGreen32 = TColor32(0xff006400);
static _DELPHI_CONST TColor32 clDarkGrey32 = TColor32(0xffa9a9a9);
static _DELPHI_CONST TColor32 clDarkKhaki32 = TColor32(0xffbdb76b);
static _DELPHI_CONST TColor32 clDarkMagenta32 = TColor32(0xff8b008b);
static _DELPHI_CONST TColor32 clDarkOliveGreen32 = TColor32(0xff556b2f);
static _DELPHI_CONST TColor32 clDarkOrange32 = TColor32(0xffff8c00);
static _DELPHI_CONST TColor32 clDarkOrchid32 = TColor32(0xff9932cc);
static _DELPHI_CONST TColor32 clDarkRed32 = TColor32(0xff8b0000);
static _DELPHI_CONST TColor32 clDarkSalmon32 = TColor32(0xffe9967a);
static _DELPHI_CONST TColor32 clDarkSeaGreen32 = TColor32(0xff8fbc8f);
static _DELPHI_CONST TColor32 clDarkSlateBlue32 = TColor32(0xff483d8b);
static _DELPHI_CONST TColor32 clDarkSlateGray32 = TColor32(0xff2f4f4f);
static _DELPHI_CONST TColor32 clDarkSlateGrey32 = TColor32(0xff2f4f4f);
static _DELPHI_CONST TColor32 clDarkTurquoise32 = TColor32(0xff00ced1);
static _DELPHI_CONST TColor32 clDarkViolet32 = TColor32(0xff9400d3);
static _DELPHI_CONST TColor32 clDeepPink32 = TColor32(0xffff1493);
static _DELPHI_CONST TColor32 clDeepSkyBlue32 = TColor32(0xff00bfff);
static _DELPHI_CONST TColor32 clDodgerBlue32 = TColor32(0xff1e90ff);
static _DELPHI_CONST TColor32 clFireBrick32 = TColor32(0xffb22222);
static _DELPHI_CONST TColor32 clFloralWhite32 = TColor32(0xfffffaf0);
static _DELPHI_CONST TColor32 clGainsBoro32 = TColor32(0xffdcdcdc);
static _DELPHI_CONST TColor32 clGhostWhite32 = TColor32(0xfff8f8ff);
static _DELPHI_CONST TColor32 clGold32 = TColor32(0xffffd700);
static _DELPHI_CONST TColor32 clGoldenRod32 = TColor32(0xffdaa520);
static _DELPHI_CONST TColor32 clGreenYellow32 = TColor32(0xffadff2f);
static _DELPHI_CONST TColor32 clGrey32 = TColor32(0xff808080);
static _DELPHI_CONST TColor32 clHoneyDew32 = TColor32(0xfff0fff0);
static _DELPHI_CONST TColor32 clHotPink32 = TColor32(0xffff69b4);
static _DELPHI_CONST TColor32 clIndianRed32 = TColor32(0xffcd5c5c);
static _DELPHI_CONST TColor32 clIndigo32 = TColor32(0xff4b0082);
static _DELPHI_CONST TColor32 clIvory32 = TColor32(0xfffffff0);
static _DELPHI_CONST TColor32 clKhaki32 = TColor32(0xfff0e68c);
static _DELPHI_CONST TColor32 clLavender32 = TColor32(0xffe6e6fa);
static _DELPHI_CONST TColor32 clLavenderBlush32 = TColor32(0xfffff0f5);
static _DELPHI_CONST TColor32 clLawnGreen32 = TColor32(0xff7cfc00);
static _DELPHI_CONST TColor32 clLemonChiffon32 = TColor32(0xfffffacd);
static _DELPHI_CONST TColor32 clLightBlue32 = TColor32(0xffadd8e6);
static _DELPHI_CONST TColor32 clLightCoral32 = TColor32(0xfff08080);
static _DELPHI_CONST TColor32 clLightCyan32 = TColor32(0xffe0ffff);
static _DELPHI_CONST TColor32 clLightGoldenRodYellow32 = TColor32(0xfffafad2);
static _DELPHI_CONST TColor32 clLightGreen32 = TColor32(0xff90ee90);
static _DELPHI_CONST TColor32 clLightGrey32 = TColor32(0xffd3d3d3);
static _DELPHI_CONST TColor32 clLightPink32 = TColor32(0xffffb6c1);
static _DELPHI_CONST TColor32 clLightSalmon32 = TColor32(0xffffa07a);
static _DELPHI_CONST TColor32 clLightSeagreen32 = TColor32(0xff20b2aa);
static _DELPHI_CONST TColor32 clLightSkyblue32 = TColor32(0xff87cefa);
static _DELPHI_CONST TColor32 clLightSlategray32 = TColor32(0xff778899);
static _DELPHI_CONST TColor32 clLightSlategrey32 = TColor32(0xff778899);
static _DELPHI_CONST TColor32 clLightSteelblue32 = TColor32(0xffb0c4de);
static _DELPHI_CONST TColor32 clLightYellow32 = TColor32(0xffffffe0);
static _DELPHI_CONST TColor32 clLtGray32 = TColor32(0xffc0c0c0);
static _DELPHI_CONST TColor32 clMedGray32 = TColor32(0xffa0a0a4);
static _DELPHI_CONST TColor32 clDkGray32 = TColor32(0xff808080);
static _DELPHI_CONST TColor32 clMoneyGreen32 = TColor32(0xffc0dcc0);
static _DELPHI_CONST TColor32 clLegacySkyBlue32 = TColor32(0xffa6caf0);
static _DELPHI_CONST TColor32 clCream32 = TColor32(0xfffffbf0);
static _DELPHI_CONST TColor32 clLimeGreen32 = TColor32(0xff32cd32);
static _DELPHI_CONST TColor32 clLinen32 = TColor32(0xfffaf0e6);
static _DELPHI_CONST TColor32 clMediumAquamarine32 = TColor32(0xff66cdaa);
static _DELPHI_CONST TColor32 clMediumBlue32 = TColor32(0xff0000cd);
static _DELPHI_CONST TColor32 clMediumOrchid32 = TColor32(0xffba55d3);
static _DELPHI_CONST TColor32 clMediumPurple32 = TColor32(0xff9370db);
static _DELPHI_CONST TColor32 clMediumSeaGreen32 = TColor32(0xff3cb371);
static _DELPHI_CONST TColor32 clMediumSlateBlue32 = TColor32(0xff7b68ee);
static _DELPHI_CONST TColor32 clMediumSpringGreen32 = TColor32(0xff00fa9a);
static _DELPHI_CONST TColor32 clMediumTurquoise32 = TColor32(0xff48d1cc);
static _DELPHI_CONST TColor32 clMediumVioletRed32 = TColor32(0xffc71585);
static _DELPHI_CONST TColor32 clMidnightBlue32 = TColor32(0xff191970);
static _DELPHI_CONST TColor32 clMintCream32 = TColor32(0xfff5fffa);
static _DELPHI_CONST TColor32 clMistyRose32 = TColor32(0xffffe4e1);
static _DELPHI_CONST TColor32 clMoccasin32 = TColor32(0xffffe4b5);
static _DELPHI_CONST TColor32 clNavajoWhite32 = TColor32(0xffffdead);
static _DELPHI_CONST TColor32 clOldLace32 = TColor32(0xfffdf5e6);
static _DELPHI_CONST TColor32 clOliveDrab32 = TColor32(0xff6b8e23);
static _DELPHI_CONST TColor32 clOrange32 = TColor32(0xffffa500);
static _DELPHI_CONST TColor32 clOrangeRed32 = TColor32(0xffff4500);
static _DELPHI_CONST TColor32 clOrchid32 = TColor32(0xffda70d6);
static _DELPHI_CONST TColor32 clPaleGoldenRod32 = TColor32(0xffeee8aa);
static _DELPHI_CONST TColor32 clPaleGreen32 = TColor32(0xff98fb98);
static _DELPHI_CONST TColor32 clPaleTurquoise32 = TColor32(0xffafeeee);
static _DELPHI_CONST TColor32 clPaleVioletred32 = TColor32(0xffdb7093);
static _DELPHI_CONST TColor32 clPapayaWhip32 = TColor32(0xffffefd5);
static _DELPHI_CONST TColor32 clPeachPuff32 = TColor32(0xffffdab9);
static _DELPHI_CONST TColor32 clPeru32 = TColor32(0xffcd853f);
static _DELPHI_CONST TColor32 clPlum32 = TColor32(0xffdda0dd);
static _DELPHI_CONST TColor32 clPowderBlue32 = TColor32(0xffb0e0e6);
static _DELPHI_CONST TColor32 clRosyBrown32 = TColor32(0xffbc8f8f);
static _DELPHI_CONST TColor32 clRoyalBlue32 = TColor32(0xff4169e1);
static _DELPHI_CONST TColor32 clSaddleBrown32 = TColor32(0xff8b4513);
static _DELPHI_CONST TColor32 clSalmon32 = TColor32(0xfffa8072);
static _DELPHI_CONST TColor32 clSandyBrown32 = TColor32(0xfff4a460);
static _DELPHI_CONST TColor32 clSeaGreen32 = TColor32(0xff2e8b57);
static _DELPHI_CONST TColor32 clSeaShell32 = TColor32(0xfffff5ee);
static _DELPHI_CONST TColor32 clSienna32 = TColor32(0xffa0522d);
static _DELPHI_CONST TColor32 clSilver32 = TColor32(0xffc0c0c0);
static _DELPHI_CONST TColor32 clSkyblue32 = TColor32(0xff87ceeb);
static _DELPHI_CONST TColor32 clSlateBlue32 = TColor32(0xff6a5acd);
static _DELPHI_CONST TColor32 clSlateGray32 = TColor32(0xff708090);
static _DELPHI_CONST TColor32 clSlateGrey32 = TColor32(0xff708090);
static _DELPHI_CONST TColor32 clSnow32 = TColor32(0xfffffafa);
static _DELPHI_CONST TColor32 clSpringgreen32 = TColor32(0xff00ff7f);
static _DELPHI_CONST TColor32 clSteelblue32 = TColor32(0xff4682b4);
static _DELPHI_CONST TColor32 clTan32 = TColor32(0xffd2b48c);
static _DELPHI_CONST TColor32 clThistle32 = TColor32(0xffd8bfd8);
static _DELPHI_CONST TColor32 clTomato32 = TColor32(0xffff6347);
static _DELPHI_CONST TColor32 clTurquoise32 = TColor32(0xff40e0d0);
static _DELPHI_CONST TColor32 clViolet32 = TColor32(0xffee82ee);
static _DELPHI_CONST TColor32 clWheat32 = TColor32(0xfff5deb3);
static _DELPHI_CONST TColor32 clWhitesmoke32 = TColor32(0xfff5f5f5);
static _DELPHI_CONST TColor32 clYellowgreen32 = TColor32(0xff9acd32);
static _DELPHI_CONST TColor32 clTrWhite32 = TColor32(0x7fffffff);
static _DELPHI_CONST TColor32 clTrBlack32 = TColor32(0x7f000000);
static _DELPHI_CONST TColor32 clTrRed32 = TColor32(0x7fff0000);
static _DELPHI_CONST TColor32 clTrGreen32 = TColor32(0x7f00ff00);
static _DELPHI_CONST TColor32 clTrBlue32 = TColor32(0x7f0000ff);
static _DELPHI_CONST int FixedOne = int(0x10000);
static _DELPHI_CONST System::Word FixedHalf = System::Word(0x7fff);
static _DELPHI_CONST __int64 FixedPI = 0x000000000003243fLL;
static const double FixedToFloat = 1.525879E-05;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 256> GAMMA_TABLE;
static _DELPHI_CONST unsigned AREAINFO_RECT = unsigned(0x80000000);
static _DELPHI_CONST int AREAINFO_LINE = int(0x40000000);
static _DELPHI_CONST int AREAINFO_ELLIPSE = int(0x20000000);
static _DELPHI_CONST int AREAINFO_ABSOLUTE = int(0x10000000);
static _DELPHI_CONST unsigned AREAINFO_MASK = unsigned(0xff000000);
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* StockBitmap;
extern DELPHI_PACKAGE TColor32 __fastcall Color32(System::Uitypes::TColor WinColor)/* overload */;
extern DELPHI_PACKAGE TColor32 __fastcall Color32(System::Byte R, System::Byte G, System::Byte B, System::Byte A = (System::Byte)(0xff))/* overload */;
extern DELPHI_PACKAGE TColor32 __fastcall Color32(System::Byte Index, TPalette32 &Palette)/* overload */;
extern DELPHI_PACKAGE TColor32 __fastcall Gray32(System::Byte Intensity, System::Byte Alpha = (System::Byte)(0xff));
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall WinColor(TColor32 Color32);
extern DELPHI_PACKAGE TArrayOfColor32 __fastcall ArrayOfColor32(TColor32 *Colors, const System::NativeInt Colors_High);
extern DELPHI_PACKAGE void __fastcall Color32ToRGB(TColor32 Color32, System::Byte &R, System::Byte &G, System::Byte &B);
extern DELPHI_PACKAGE void __fastcall Color32ToRGBA(TColor32 Color32, System::Byte &R, System::Byte &G, System::Byte &B, System::Byte &A);
extern DELPHI_PACKAGE TColor32Components __fastcall Color32Components(bool R, bool G, bool B, bool A);
extern DELPHI_PACKAGE int __fastcall RedComponent(TColor32 Color32);
extern DELPHI_PACKAGE int __fastcall GreenComponent(TColor32 Color32);
extern DELPHI_PACKAGE int __fastcall BlueComponent(TColor32 Color32);
extern DELPHI_PACKAGE int __fastcall AlphaComponent(TColor32 Color32);
extern DELPHI_PACKAGE int __fastcall Intensity(TColor32 Color32);
extern DELPHI_PACKAGE TColor32 __fastcall SetAlpha(TColor32 Color32, int NewAlpha);
extern DELPHI_PACKAGE TColor32 __fastcall HSLtoRGB(float H, float S, float L)/* overload */;
extern DELPHI_PACKAGE void __fastcall RGBtoHSL(TColor32 RGB, /* out */ float &H, /* out */ float &S, /* out */ float &L)/* overload */;
extern DELPHI_PACKAGE TColor32 __fastcall HSLtoRGB(int H, int S, int L)/* overload */;
extern DELPHI_PACKAGE void __fastcall RGBtoHSL(TColor32 RGB, /* out */ System::Byte &H, /* out */ System::Byte &S, /* out */ System::Byte &L)/* overload */;
extern DELPHI_PACKAGE HPALETTE __fastcall WinPalette(const TPalette32 &P);
extern DELPHI_PACKAGE TFixed __fastcall Fixed(float S)/* overload */;
extern DELPHI_PACKAGE TFixed __fastcall Fixed(int I)/* overload */;
extern DELPHI_PACKAGE TPoint __fastcall Point(int X, int Y)/* overload */;
extern DELPHI_PACKAGE System::Types::TPoint __fastcall Point(const TFloatPoint &FP)/* overload */;
extern DELPHI_PACKAGE System::Types::TPoint __fastcall Point(const TFixedPoint &FXP)/* overload */;
extern DELPHI_PACKAGE TFloatPoint __fastcall FloatPoint(float X, float Y)/* overload */;
extern DELPHI_PACKAGE TFloatPoint __fastcall FloatPoint(const TPoint &P)/* overload */;
extern DELPHI_PACKAGE TFloatPoint __fastcall FloatPoint(const TFixedPoint &FXP)/* overload */;
extern DELPHI_PACKAGE TFixedPoint __fastcall FixedPoint(int X, int Y)/* overload */;
extern DELPHI_PACKAGE TFixedPoint __fastcall FixedPoint(float X, float Y)/* overload */;
extern DELPHI_PACKAGE TFixedPoint __fastcall FixedPoint(const TPoint &P)/* overload */;
extern DELPHI_PACKAGE TFixedPoint __fastcall FixedPoint(const TFloatPoint &FP)/* overload */;
extern DELPHI_PACKAGE TRect __fastcall MakeRect(const int L, const int T, const int R, const int B)/* overload */;
extern DELPHI_PACKAGE System::Types::TRect __fastcall MakeRect(const TFloatRect &FR, TRectRounding Rounding = (TRectRounding)(0x0))/* overload */;
extern DELPHI_PACKAGE System::Types::TRect __fastcall MakeRect(const TFixedRect &FXR, TRectRounding Rounding = (TRectRounding)(0x0))/* overload */;
extern DELPHI_PACKAGE TFixedRect __fastcall FixedRect(const TFixed L, const TFixed T, const TFixed R, const TFixed B)/* overload */;
extern DELPHI_PACKAGE TFixedRect __fastcall FixedRect(const TRect &ARect)/* overload */;
extern DELPHI_PACKAGE TFixedRect __fastcall FixedRect(const TFloatRect &FR)/* overload */;
extern DELPHI_PACKAGE TFloatRect __fastcall FloatRect(const TFloat L, const TFloat T, const TFloat R, const TFloat B)/* overload */;
extern DELPHI_PACKAGE TFloatRect __fastcall FloatRect(const TRect &ARect)/* overload */;
extern DELPHI_PACKAGE TFloatRect __fastcall FloatRect(const TFixedRect &FXR)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectRect(/* out */ TRect &Dst, const TRect &R1, const TRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectRect(/* out */ TFloatRect &Dst, const TFloatRect &FR1, const TFloatRect &FR2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall UnionRect(/* out */ TRect &Rect, const TRect &R1, const TRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall UnionRect(/* out */ TFloatRect &Rect, const TFloatRect &R1, const TFloatRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRect(const TRect &R1, const TRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRect(const TFloatRect &R1, const TFloatRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRectSize(const TRect &R1, const TRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRectSize(const TFloatRect &R1, const TFloatRect &R2)/* overload */;
extern DELPHI_PACKAGE void __fastcall InflateRect(TRect &R, int Dx, int Dy)/* overload */;
extern DELPHI_PACKAGE void __fastcall InflateRect(TFloatRect &FR, TFloat Dx, TFloat Dy)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetRect(TRect &R, int Dx, int Dy)/* overload */;
extern DELPHI_PACKAGE void __fastcall OffsetRect(TFloatRect &FR, TFloat Dx, TFloat Dy)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsRectEmpty(const TRect &R)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsRectEmpty(const TFloatRect &FR)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PtInRect(const TRect &R, const TPoint &P)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PtInRect(const TFloatRect &R, const TPoint &P)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PtInRect(const TRect &R, const TFloatPoint &P)/* overload */;
extern DELPHI_PACKAGE bool __fastcall PtInRect(const TFloatRect &R, const TFloatPoint &P)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetGamma(float Gamma = 7.000000E-01f);
extern DELPHI_PACKAGE TCustomBackendClass __fastcall GetPlatformBackendClass(void);
}	/* namespace Gr32 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32)
using namespace Gr32;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32HPP
