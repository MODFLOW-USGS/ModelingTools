// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Resamplers.pas' rev: 36.00 (Windows)

#ifndef Gr32_resamplersHPP
#define Gr32_resamplersHPP

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
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>
#include <GR32_Transforms.hpp>
#include <GR32_Containers.hpp>
#include <GR32_OrdinalMaps.hpp>
#include <GR32_Blend.hpp>
#include <GR32_System.hpp>
#include <GR32_Bindings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_resamplers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EBitmapException;
class DELPHICLASS ESrcInvalidException;
class DELPHICLASS ENestedException;
class DELPHICLASS TCustomKernel;
class DELPHICLASS TBoxKernel;
class DELPHICLASS TLinearKernel;
class DELPHICLASS TCosineKernel;
class DELPHICLASS TSplineKernel;
class DELPHICLASS TMitchellKernel;
class DELPHICLASS TCubicKernel;
class DELPHICLASS THermiteKernel;
class DELPHICLASS TWindowedSincKernel;
class DELPHICLASS TAlbrechtKernel;
class DELPHICLASS TLanczosKernel;
class DELPHICLASS TGaussianKernel;
class DELPHICLASS TBlackmanKernel;
class DELPHICLASS THannKernel;
class DELPHICLASS THammingKernel;
class DELPHICLASS TSinshKernel;
class DELPHICLASS TNearestResampler;
class DELPHICLASS TLinearResampler;
class DELPHICLASS TDraftResampler;
class DELPHICLASS TKernelResampler;
class DELPHICLASS TNestedSampler;
class DELPHICLASS TTransformer;
class DELPHICLASS TSuperSampler;
class DELPHICLASS TAdaptiveSuperSampler;
class DELPHICLASS TPatternSampler;
struct TBufferEntry;
class DELPHICLASS TKernelSampler;
class DELPHICLASS TConvolver;
class DELPHICLASS TSelectiveConvolver;
class DELPHICLASS TMorphologicalSampler;
class DELPHICLASS TDilater;
class DELPHICLASS TEroder;
class DELPHICLASS TExpander;
class DELPHICLASS TContracter;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<int, 33> TKernelEntry;

typedef TKernelEntry *PKernelEntry;

typedef System::DynamicArray<Gr32::TArrayOfInteger> TArrayOfKernelEntry;

typedef System::StaticArray<Gr32::TArrayOfInteger, 1> TKernelEntryArray;

typedef TKernelEntryArray *PKernelEntryArray;

typedef Gr32::TFloat __fastcall (__closure *TFilterMethod)(Gr32::TFloat Value);

class PASCALIMPLEMENTATION EBitmapException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EBitmapException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EBitmapException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EBitmapException(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EBitmapException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EBitmapException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EBitmapException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EBitmapException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EBitmapException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBitmapException(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EBitmapException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBitmapException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EBitmapException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EBitmapException() { }
	
};


class PASCALIMPLEMENTATION ESrcInvalidException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESrcInvalidException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESrcInvalidException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESrcInvalidException(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESrcInvalidException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESrcInvalidException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESrcInvalidException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESrcInvalidException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESrcInvalidException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESrcInvalidException(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESrcInvalidException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESrcInvalidException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESrcInvalidException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESrcInvalidException() { }
	
};


class PASCALIMPLEMENTATION ENestedException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ENestedException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ENestedException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ENestedException(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ENestedException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ENestedException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ENestedException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ENestedException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ENestedException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ENestedException(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ENestedException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ENestedException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ENestedException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ENestedException() { }
	
};


typedef Gr32::TColor32 __fastcall (__closure *TGetSampleInt)(int X, int Y);

typedef Gr32::TColor32 __fastcall (__closure *TGetSampleFloat)(Gr32::TFloat X, Gr32::TFloat Y);

typedef Gr32::TColor32 __fastcall (__closure *TGetSampleFixed)(Gr32::TFixed X, Gr32::TFixed Y);

class PASCALIMPLEMENTATION TCustomKernel : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	Gr32::TNotifiablePersistent* FObserver;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	virtual bool __fastcall RangeCheck();
	
public:
	__fastcall virtual TCustomKernel();
	void __fastcall Changed();
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value) = 0 ;
	virtual Gr32::TFloat __fastcall GetWidth() = 0 ;
	__property Gr32::TNotifiablePersistent* Observer = {read=FObserver};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomKernel() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TCustomKernelClass);

class PASCALIMPLEMENTATION TBoxKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
public:
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
public:
	/* TCustomKernel.Create */ inline __fastcall virtual TBoxKernel() : TCustomKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TBoxKernel() { }
	
};


class PASCALIMPLEMENTATION TLinearKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
public:
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
public:
	/* TCustomKernel.Create */ inline __fastcall virtual TLinearKernel() : TCustomKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TLinearKernel() { }
	
};


class PASCALIMPLEMENTATION TCosineKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
public:
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
public:
	/* TCustomKernel.Create */ inline __fastcall virtual TCosineKernel() : TCustomKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCosineKernel() { }
	
};


class PASCALIMPLEMENTATION TSplineKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
protected:
	virtual bool __fastcall RangeCheck();
	
public:
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
public:
	/* TCustomKernel.Create */ inline __fastcall virtual TSplineKernel() : TCustomKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSplineKernel() { }
	
};


class PASCALIMPLEMENTATION TMitchellKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
protected:
	virtual bool __fastcall RangeCheck();
	
public:
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
public:
	/* TCustomKernel.Create */ inline __fastcall virtual TMitchellKernel() : TCustomKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TMitchellKernel() { }
	
};


class PASCALIMPLEMENTATION TCubicKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
private:
	Gr32::TFloat FCoeff;
	void __fastcall SetCoeff(const Gr32::TFloat Value);
	
protected:
	virtual bool __fastcall RangeCheck();
	
public:
	__fastcall virtual TCubicKernel();
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
	
__published:
	__property Gr32::TFloat Coeff = {read=FCoeff, write=SetCoeff};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCubicKernel() { }
	
};


class PASCALIMPLEMENTATION THermiteKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
private:
	Gr32::TFloat FBias;
	Gr32::TFloat FTension;
	void __fastcall SetBias(const Gr32::TFloat Value);
	void __fastcall SetTension(const Gr32::TFloat Value);
	
protected:
	virtual bool __fastcall RangeCheck();
	
public:
	__fastcall virtual THermiteKernel();
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
	
__published:
	__property Gr32::TFloat Bias = {read=FBias, write=SetBias};
	__property Gr32::TFloat Tension = {read=FTension, write=SetTension};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~THermiteKernel() { }
	
};


class PASCALIMPLEMENTATION TWindowedSincKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
private:
	Gr32::TFloat FWidth;
	Gr32::TFloat FWidthReciprocal;
	
protected:
	virtual bool __fastcall RangeCheck();
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value) = 0 ;
	
public:
	__fastcall virtual TWindowedSincKernel();
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	void __fastcall SetWidth(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
	__property Gr32::TFloat WidthReciprocal = {read=FWidthReciprocal};
	
__published:
	__property Gr32::TFloat Width = {read=FWidth, write=SetWidth};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TWindowedSincKernel() { }
	
};


class PASCALIMPLEMENTATION TAlbrechtKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
private:
	int FTerms;
	System::StaticArray<double, 12> FCoefPointer;
	void __fastcall SetTerms(int Value);
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
	
public:
	__fastcall virtual TAlbrechtKernel();
	
__published:
	__property int Terms = {read=FTerms, write=SetTerms, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TAlbrechtKernel() { }
	
};


class PASCALIMPLEMENTATION TLanczosKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
public:
	/* TWindowedSincKernel.Create */ inline __fastcall virtual TLanczosKernel() : TWindowedSincKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TLanczosKernel() { }
	
};


class PASCALIMPLEMENTATION TGaussianKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
private:
	Gr32::TFloat FSigma;
	Gr32::TFloat FSigmaReciprocalLn2;
	void __fastcall SetSigma(const Gr32::TFloat Value);
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
	
public:
	__fastcall virtual TGaussianKernel();
	
__published:
	__property Gr32::TFloat Sigma = {read=FSigma, write=SetSigma};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGaussianKernel() { }
	
};


class PASCALIMPLEMENTATION TBlackmanKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
public:
	/* TWindowedSincKernel.Create */ inline __fastcall virtual TBlackmanKernel() : TWindowedSincKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TBlackmanKernel() { }
	
};


class PASCALIMPLEMENTATION THannKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
public:
	/* TWindowedSincKernel.Create */ inline __fastcall virtual THannKernel() : TWindowedSincKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~THannKernel() { }
	
};


class PASCALIMPLEMENTATION THammingKernel : public TWindowedSincKernel
{
	typedef TWindowedSincKernel inherited;
	
protected:
	virtual Gr32::TFloat __fastcall Window(Gr32::TFloat Value);
public:
	/* TWindowedSincKernel.Create */ inline __fastcall virtual THammingKernel() : TWindowedSincKernel() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~THammingKernel() { }
	
};


class PASCALIMPLEMENTATION TSinshKernel : public TCustomKernel
{
	typedef TCustomKernel inherited;
	
private:
	Gr32::TFloat FWidth;
	Gr32::TFloat FCoeff;
	void __fastcall SetCoeff(const Gr32::TFloat Value);
	
protected:
	virtual bool __fastcall RangeCheck();
	
public:
	__fastcall virtual TSinshKernel();
	void __fastcall SetWidth(Gr32::TFloat Value);
	virtual Gr32::TFloat __fastcall GetWidth();
	virtual Gr32::TFloat __fastcall Filter(Gr32::TFloat Value);
	
__published:
	__property Gr32::TFloat Coeff = {read=FCoeff, write=SetCoeff};
	__property Gr32::TFloat Width = {read=GetWidth, write=SetWidth};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSinshKernel() { }
	
};


class PASCALIMPLEMENTATION TNearestResampler : public Gr32::TCustomResampler
{
	typedef Gr32::TCustomResampler inherited;
	
private:
	TGetSampleInt FGetSampleInt;
	
protected:
	Gr32::TColor32 __fastcall GetPixelTransparentEdge(int X, int Y);
	virtual Gr32::TFloat __fastcall GetWidth();
	virtual void __fastcall Resample(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack);
	
public:
	virtual Gr32::TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	virtual Gr32::TColor32 __fastcall GetSampleFloat(Gr32::TFloat X, Gr32::TFloat Y);
	virtual void __fastcall PrepareSampling();
public:
	/* TCustomResampler.Create */ inline __fastcall virtual TNearestResampler()/* overload */ : Gr32::TCustomResampler() { }
	/* TCustomResampler.Create */ inline __fastcall virtual TNearestResampler(Gr32::TCustomBitmap32* ABitmap)/* overload */ : Gr32::TCustomResampler(ABitmap) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TNearestResampler() { }
	
};


class PASCALIMPLEMENTATION TLinearResampler : public Gr32::TCustomResampler
{
	typedef Gr32::TCustomResampler inherited;
	
private:
	TLinearKernel* FLinearKernel;
	TGetSampleFixed FGetSampleFixed;
	
protected:
	virtual Gr32::TFloat __fastcall GetWidth();
	Gr32::TColor32 __fastcall GetPixelTransparentEdge(Gr32::TFixed X, Gr32::TFixed Y);
	virtual void __fastcall Resample(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack);
	
public:
	__fastcall virtual TLinearResampler()/* overload */;
	__fastcall virtual ~TLinearResampler();
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	virtual Gr32::TColor32 __fastcall GetSampleFloat(Gr32::TFloat X, Gr32::TFloat Y);
	virtual void __fastcall PrepareSampling();
public:
	/* TCustomResampler.Create */ inline __fastcall virtual TLinearResampler(Gr32::TCustomBitmap32* ABitmap)/* overload */ : Gr32::TCustomResampler(ABitmap) { }
	
};


class PASCALIMPLEMENTATION TDraftResampler : public TLinearResampler
{
	typedef TLinearResampler inherited;
	
protected:
	virtual void __fastcall Resample(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack);
public:
	/* TLinearResampler.Create */ inline __fastcall virtual TDraftResampler()/* overload */ : TLinearResampler() { }
	/* TLinearResampler.Destroy */ inline __fastcall virtual ~TDraftResampler() { }
	
public:
	/* TCustomResampler.Create */ inline __fastcall virtual TDraftResampler(Gr32::TCustomBitmap32* ABitmap)/* overload */ : TLinearResampler(ABitmap) { }
	
};


enum DECLSPEC_DENUM TKernelMode : unsigned char { kmDynamic, kmTableNearest, kmTableLinear };

class PASCALIMPLEMENTATION TKernelResampler : public Gr32::TCustomResampler
{
	typedef Gr32::TCustomResampler inherited;
	
private:
	TCustomKernel* FKernel;
	TKernelMode FKernelMode;
	Gr32_ordinalmaps::TIntegerMap* FWeightTable;
	int FTableSize;
	Gr32::TColor32 FOuterColor;
	void __fastcall SetKernel(TCustomKernel* const Value);
	System::UnicodeString __fastcall GetKernelClassName();
	void __fastcall SetKernelClassName(System::UnicodeString Value);
	void __fastcall SetKernelMode(const TKernelMode Value);
	void __fastcall SetTableSize(int Value);
	
protected:
	virtual Gr32::TFloat __fastcall GetWidth();
	
public:
	__fastcall virtual TKernelResampler()/* overload */;
	__fastcall virtual ~TKernelResampler();
	virtual Gr32::TColor32 __fastcall GetSampleFloat(Gr32::TFloat X, Gr32::TFloat Y);
	virtual void __fastcall Resample(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack);
	virtual void __fastcall PrepareSampling();
	virtual void __fastcall FinalizeSampling();
	
__published:
	__property System::UnicodeString KernelClassName = {read=GetKernelClassName, write=SetKernelClassName};
	__property TCustomKernel* Kernel = {read=FKernel, write=SetKernel};
	__property TKernelMode KernelMode = {read=FKernelMode, write=SetKernelMode, nodefault};
	__property int TableSize = {read=FTableSize, write=SetTableSize, nodefault};
public:
	/* TCustomResampler.Create */ inline __fastcall virtual TKernelResampler(Gr32::TCustomBitmap32* ABitmap)/* overload */ : Gr32::TCustomResampler(ABitmap) { }
	
};


class PASCALIMPLEMENTATION TNestedSampler : public Gr32::TCustomSampler
{
	typedef Gr32::TCustomSampler inherited;
	
private:
	Gr32::TCustomSampler* FSampler;
	TGetSampleInt FGetSampleInt;
	TGetSampleFixed FGetSampleFixed;
	TGetSampleFloat FGetSampleFloat;
	void __fastcall SetSampler(Gr32::TCustomSampler* const Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	
public:
	__fastcall virtual TNestedSampler(Gr32::TCustomSampler* ASampler);
	virtual void __fastcall PrepareSampling();
	virtual void __fastcall FinalizeSampling();
	virtual bool __fastcall HasBounds();
	virtual Gr32::TFloatRect __fastcall GetSampleBounds();
	
__published:
	__property Gr32::TCustomSampler* Sampler = {read=FSampler, write=SetSampler};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TNestedSampler() { }
	
};


typedef void __fastcall (__closure *TReverseTransformInt)(int DstX, int DstY, /* out */ int &SrcX, /* out */ int &SrcY);

typedef void __fastcall (__closure *TReverseTransformFixed)(Gr32::TFixed DstX, Gr32::TFixed DstY, /* out */ Gr32::TFixed &SrcX, /* out */ Gr32::TFixed &SrcY);

typedef void __fastcall (__closure *TReverseTransformFloat)(Gr32::TFloat DstX, Gr32::TFloat DstY, /* out */ Gr32::TFloat &SrcX, /* out */ Gr32::TFloat &SrcY);

class PASCALIMPLEMENTATION TTransformer : public TNestedSampler
{
	typedef TNestedSampler inherited;
	
private:
	Gr32_transforms::TTransformation* FTransformation;
	TReverseTransformInt FTransformationReverseTransformInt;
	TReverseTransformFixed FTransformationReverseTransformFixed;
	TReverseTransformFloat FTransformationReverseTransformFloat;
	void __fastcall SetTransformation(Gr32_transforms::TTransformation* const Value);
	
public:
	__fastcall TTransformer(Gr32::TCustomSampler* ASampler, Gr32_transforms::TTransformation* ATransformation);
	virtual void __fastcall PrepareSampling();
	virtual Gr32::TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	virtual Gr32::TColor32 __fastcall GetSampleFloat(Gr32::TFloat X, Gr32::TFloat Y);
	virtual bool __fastcall HasBounds();
	virtual Gr32::TFloatRect __fastcall GetSampleBounds();
	
__published:
	__property Gr32_transforms::TTransformation* Transformation = {read=FTransformation, write=SetTransformation};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTransformer() { }
	
};


typedef int TSamplingRange;

class PASCALIMPLEMENTATION TSuperSampler : public TNestedSampler
{
	typedef TNestedSampler inherited;
	
private:
	TSamplingRange FSamplingY;
	TSamplingRange FSamplingX;
	Gr32::TFixed FDistanceX;
	Gr32::TFixed FDistanceY;
	Gr32::TFixed FOffsetX;
	Gr32::TFixed FOffsetY;
	Gr32::TFixed FScale;
	void __fastcall SetSamplingX(const TSamplingRange Value);
	void __fastcall SetSamplingY(const TSamplingRange Value);
	
public:
	__fastcall virtual TSuperSampler(Gr32::TCustomSampler* Sampler);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	
__published:
	__property TSamplingRange SamplingX = {read=FSamplingX, write=SetSamplingX, nodefault};
	__property TSamplingRange SamplingY = {read=FSamplingY, write=SetSamplingY, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSuperSampler() { }
	
};


typedef Gr32::TColor32 __fastcall (__closure *TRecurseProc)(Gr32::TFixed X, Gr32::TFixed Y, Gr32::TFixed W, const Gr32::TColor32 C1, const Gr32::TColor32 C2);

class PASCALIMPLEMENTATION TAdaptiveSuperSampler : public TNestedSampler
{
	typedef TNestedSampler inherited;
	
private:
	Gr32::TFixed FMinOffset;
	int FLevel;
	int FTolerance;
	void __fastcall SetLevel(const int Value);
	Gr32::TColor32 __fastcall DoRecurse(Gr32::TFixed X, Gr32::TFixed Y, Gr32::TFixed Offset, const Gr32::TColor32 A, const Gr32::TColor32 B, const Gr32::TColor32 C, const Gr32::TColor32 D, const Gr32::TColor32 E);
	Gr32::TColor32 __fastcall QuadrantColor(const Gr32::TColor32 C1, const Gr32::TColor32 C2, Gr32::TFixed X, Gr32::TFixed Y, Gr32::TFixed Offset, TRecurseProc Proc);
	Gr32::TColor32 __fastcall RecurseAC(Gr32::TFixed X, Gr32::TFixed Y, Gr32::TFixed Offset, const Gr32::TColor32 A, const Gr32::TColor32 C);
	Gr32::TColor32 __fastcall RecurseBD(Gr32::TFixed X, Gr32::TFixed Y, Gr32::TFixed Offset, const Gr32::TColor32 B, const Gr32::TColor32 D);
	
protected:
	virtual bool __fastcall CompareColors(Gr32::TColor32 C1, Gr32::TColor32 C2);
	
public:
	__fastcall virtual TAdaptiveSuperSampler(Gr32::TCustomSampler* Sampler);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	
__published:
	__property int Level = {read=FLevel, write=SetLevel, nodefault};
	__property int Tolerance = {read=FTolerance, write=FTolerance, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TAdaptiveSuperSampler() { }
	
};


typedef System::DynamicArray<Gr32::TArrayOfFloatPoint> Gr32_resamplers__82;

typedef System::DynamicArray<System::DynamicArray<Gr32::TArrayOfFloatPoint> > TFloatSamplePattern;

typedef System::DynamicArray<Gr32::TArrayOfFixedPoint> Gr32_resamplers__92;

typedef System::DynamicArray<System::DynamicArray<Gr32::TArrayOfFixedPoint> > TFixedSamplePattern;

class PASCALIMPLEMENTATION TPatternSampler : public TNestedSampler
{
	typedef TNestedSampler inherited;
	
private:
	TFixedSamplePattern FPattern;
	void __fastcall SetPattern(const TFixedSamplePattern Value);
	
protected:
	Gr32::TWrapProc WrapProcVert;
	
public:
	__fastcall virtual ~TPatternSampler();
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	__property TFixedSamplePattern Pattern = {read=FPattern, write=SetPattern};
public:
	/* TNestedSampler.Create */ inline __fastcall virtual TPatternSampler(Gr32::TCustomSampler* ASampler) : TNestedSampler(ASampler) { }
	
};


typedef TBufferEntry *PBufferEntry;

struct DECLSPEC_DRECORD TBufferEntry
{
public:
	int B;
	int G;
	int R;
	int A;
};


class PASCALIMPLEMENTATION TKernelSampler : public TNestedSampler
{
	typedef TNestedSampler inherited;
	
private:
	Gr32_ordinalmaps::TIntegerMap* FKernel;
	TBufferEntry FStartEntry;
	int FCenterX;
	int FCenterY;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight) = 0 ;
	virtual Gr32::TColor32 __fastcall ConvertBuffer(TBufferEntry &Buffer);
	
public:
	__fastcall virtual TKernelSampler(Gr32::TCustomSampler* ASampler);
	__fastcall virtual ~TKernelSampler();
	virtual Gr32::TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	
__published:
	__property Gr32_ordinalmaps::TIntegerMap* Kernel = {read=FKernel, write=FKernel};
	__property int CenterX = {read=FCenterX, write=FCenterX, nodefault};
	__property int CenterY = {read=FCenterY, write=FCenterY, nodefault};
};


class PASCALIMPLEMENTATION TConvolver : public TKernelSampler
{
	typedef TKernelSampler inherited;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
public:
	/* TKernelSampler.Create */ inline __fastcall virtual TConvolver(Gr32::TCustomSampler* ASampler) : TKernelSampler(ASampler) { }
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TConvolver() { }
	
};


class PASCALIMPLEMENTATION TSelectiveConvolver : public TConvolver
{
	typedef TConvolver inherited;
	
private:
	Gr32::TColor32 FRefColor;
	int FDelta;
	TBufferEntry FWeightSum;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
	virtual Gr32::TColor32 __fastcall ConvertBuffer(TBufferEntry &Buffer);
	
public:
	__fastcall virtual TSelectiveConvolver(Gr32::TCustomSampler* ASampler);
	virtual Gr32::TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
	
__published:
	__property int Delta = {read=FDelta, write=FDelta, nodefault};
public:
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TSelectiveConvolver() { }
	
};


class PASCALIMPLEMENTATION TMorphologicalSampler : public TKernelSampler
{
	typedef TKernelSampler inherited;
	
protected:
	virtual Gr32::TColor32 __fastcall ConvertBuffer(TBufferEntry &Buffer);
public:
	/* TKernelSampler.Create */ inline __fastcall virtual TMorphologicalSampler(Gr32::TCustomSampler* ASampler) : TKernelSampler(ASampler) { }
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TMorphologicalSampler() { }
	
};


class PASCALIMPLEMENTATION TDilater : public TMorphologicalSampler
{
	typedef TMorphologicalSampler inherited;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
public:
	/* TKernelSampler.Create */ inline __fastcall virtual TDilater(Gr32::TCustomSampler* ASampler) : TMorphologicalSampler(ASampler) { }
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TDilater() { }
	
};


class PASCALIMPLEMENTATION TEroder : public TMorphologicalSampler
{
	typedef TMorphologicalSampler inherited;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
	
public:
	__fastcall virtual TEroder(Gr32::TCustomSampler* ASampler);
public:
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TEroder() { }
	
};


class PASCALIMPLEMENTATION TExpander : public TKernelSampler
{
	typedef TKernelSampler inherited;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
public:
	/* TKernelSampler.Create */ inline __fastcall virtual TExpander(Gr32::TCustomSampler* ASampler) : TKernelSampler(ASampler) { }
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TExpander() { }
	
};


class PASCALIMPLEMENTATION TContracter : public TExpander
{
	typedef TExpander inherited;
	
private:
	Gr32::TColor32 FMaxWeight;
	
protected:
	virtual void __fastcall UpdateBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color, int Weight);
	
public:
	virtual void __fastcall PrepareSampling();
	virtual Gr32::TColor32 __fastcall GetSampleInt(int X, int Y);
	virtual Gr32::TColor32 __fastcall GetSampleFixed(Gr32::TFixed X, Gr32::TFixed Y);
public:
	/* TKernelSampler.Create */ inline __fastcall virtual TContracter(Gr32::TCustomSampler* ASampler) : TExpander(ASampler) { }
	/* TKernelSampler.Destroy */ inline __fastcall virtual ~TContracter() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAX_KERNEL_WIDTH = System::Int8(0x10);
extern DELPHI_PACKAGE Gr32_containers::TClassList* KernelList;
extern DELPHI_PACKAGE Gr32_containers::TClassList* ResamplerList;
extern DELPHI_PACKAGE TBufferEntry EMPTY_ENTRY;
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall (*BlockAverage)(unsigned Dlx, unsigned Dly, Gr32::PColor32 RowSrc, unsigned OffSrc);
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall (*Interpolator)(unsigned WX_256, unsigned WY_256, Gr32::PColor32 C11, Gr32::PColor32 C21);
extern DELPHI_PACKAGE System::ResourceString _SDstNil;
#define Gr32_resamplers_SDstNil System::LoadResourceString(&Gr32_resamplers::_SDstNil)
extern DELPHI_PACKAGE System::ResourceString _SSrcNil;
#define Gr32_resamplers_SSrcNil System::LoadResourceString(&Gr32_resamplers::_SSrcNil)
extern DELPHI_PACKAGE System::ResourceString _SSrcInvalid;
#define Gr32_resamplers_SSrcInvalid System::LoadResourceString(&Gr32_resamplers::_SSrcInvalid)
extern DELPHI_PACKAGE System::ResourceString _SSamplerNil;
#define Gr32_resamplers_SSamplerNil System::LoadResourceString(&Gr32_resamplers::_SSamplerNil)
extern DELPHI_PACKAGE void __fastcall Convolve(Gr32::TCustomBitmap32* Src, Gr32::TCustomBitmap32* Dst, Gr32_ordinalmaps::TIntegerMap* Kernel, int CenterX, int CenterY);
extern DELPHI_PACKAGE void __fastcall Dilate(Gr32::TCustomBitmap32* Src, Gr32::TCustomBitmap32* Dst, Gr32_ordinalmaps::TIntegerMap* Kernel, int CenterX, int CenterY);
extern DELPHI_PACKAGE void __fastcall Erode(Gr32::TCustomBitmap32* Src, Gr32::TCustomBitmap32* Dst, Gr32_ordinalmaps::TIntegerMap* Kernel, int CenterX, int CenterY);
extern DELPHI_PACKAGE void __fastcall Expand(Gr32::TCustomBitmap32* Src, Gr32::TCustomBitmap32* Dst, Gr32_ordinalmaps::TIntegerMap* Kernel, int CenterX, int CenterY);
extern DELPHI_PACKAGE void __fastcall Contract(Gr32::TCustomBitmap32* Src, Gr32::TCustomBitmap32* Dst, Gr32_ordinalmaps::TIntegerMap* Kernel, int CenterX, int CenterY);
extern DELPHI_PACKAGE void __fastcall IncBuffer(TBufferEntry &Buffer, Gr32::TColor32 Color);
extern DELPHI_PACKAGE void __fastcall MultiplyBuffer(TBufferEntry &Buffer, int W);
extern DELPHI_PACKAGE void __fastcall ShrBuffer(TBufferEntry &Buffer, int Shift);
extern DELPHI_PACKAGE Gr32::TColor32 __fastcall BufferToColor32(const TBufferEntry &Buffer, int Shift);
extern DELPHI_PACKAGE void __fastcall BlockTransfer(Gr32::TCustomBitmap32* Dst, int DstX, int DstY, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack = 0x0);
extern DELPHI_PACKAGE void __fastcall BlockTransferX(Gr32::TCustomBitmap32* Dst, Gr32::TFixed DstX, Gr32::TFixed DstY, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack = 0x0);
extern DELPHI_PACKAGE void __fastcall BlendTransfer(Gr32::TCustomBitmap32* Dst, int DstX, int DstY, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* SrcF, const Gr32::TRect &SrcRectF, Gr32::TCustomBitmap32* SrcB, const Gr32::TRect &SrcRectB, Gr32_blend::TBlendReg BlendCallback)/* overload */;
extern DELPHI_PACKAGE void __fastcall BlendTransfer(Gr32::TCustomBitmap32* Dst, int DstX, int DstY, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* SrcF, const Gr32::TRect &SrcRectF, Gr32::TCustomBitmap32* SrcB, const Gr32::TRect &SrcRectB, Gr32_blend::TBlendRegEx BlendCallback, int MasterAlpha)/* overload */;
extern DELPHI_PACKAGE void __fastcall StretchTransfer(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const Gr32::TRect &DstClip, Gr32::TCustomBitmap32* Src, const Gr32::TRect &SrcRect, Gr32::TCustomResampler* Resampler, Gr32::TDrawMode CombineOp, Gr32::TPixelCombineEvent CombineCallBack = 0x0);
extern DELPHI_PACKAGE TFixedSamplePattern __fastcall CreateJitteredPattern(int TileWidth, int TileHeight, int SamplesX, int SamplesY);
extern DELPHI_PACKAGE void __fastcall RegisterResampler(Gr32::TCustomResamplerClass ResamplerClass);
extern DELPHI_PACKAGE void __fastcall RegisterKernel(TCustomKernelClass KernelClass);
}	/* namespace Gr32_resamplers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_RESAMPLERS)
using namespace Gr32_resamplers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_resamplersHPP
