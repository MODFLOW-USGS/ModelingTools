// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Rasterizers.pas' rev: 36.00 (Windows)

#ifndef GR32_RasterizersHPP
#define GR32_RasterizersHPP

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
#include <System.Classes.hpp>
#include <GR32.hpp>
#include <GR32_Blend.hpp>
#include <GR32_OrdinalMaps.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_rasterizers
{
//-- forward type declarations -----------------------------------------------
struct TCombineInfo;
class DELPHICLASS TRasterizer;
class DELPHICLASS TRegularRasterizer;
class DELPHICLASS TSwizzlingRasterizer;
class DELPHICLASS TProgressiveRasterizer;
class DELPHICLASS TTesseralRasterizer;
class DELPHICLASS TContourRasterizer;
class DELPHICLASS TMultithreadedRegularRasterizer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TAssignColor)(Gr32::TColor32 &Dst, Gr32::TColor32 Src);

typedef TCombineInfo *PCombineInfo;

struct DECLSPEC_DRECORD TCombineInfo
{
public:
	int SrcAlpha;
	Gr32::TDrawMode DrawMode;
	Gr32::TCombineMode CombineMode;
	Gr32::TPixelCombineEvent CombineCallBack;
	Gr32::TColor32 TransparentColor;
};


class PASCALIMPLEMENTATION TRasterizer : public Gr32::TThreadPersistent
{
	typedef Gr32::TThreadPersistent inherited;
	
private:
	Gr32::TCustomSampler* FSampler;
	int FSrcAlpha;
	Gr32_blend::TBlendMemEx FBlendMemEx;
	Gr32::TPixelCombineEvent FCombineCallBack;
	TAssignColor FAssignColor;
	Gr32::TColor32 FTransparentColor;
	void __fastcall SetSampler(Gr32::TCustomSampler* const Value);
	void __fastcall SetCombineInfo(const TCombineInfo &CombineInfo);
	void __fastcall AssignColorOpaque(Gr32::TColor32 &Dst, Gr32::TColor32 Src);
	void __fastcall AssignColorBlend(Gr32::TColor32 &Dst, Gr32::TColor32 Src);
	void __fastcall AssignColorCustom(Gr32::TColor32 &Dst, Gr32::TColor32 Src);
	void __fastcall AssignColorTransparent(Gr32::TColor32 &Dst, Gr32::TColor32 Src);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dst);
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect) = 0 ;
	__property TAssignColor AssignColor = {read=FAssignColor, write=FAssignColor};
	
public:
	__fastcall virtual TRasterizer();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Rasterize(Gr32::TCustomBitmap32* Dst)/* overload */;
	void __fastcall Rasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect)/* overload */;
	void __fastcall Rasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, const TCombineInfo &CombineInfo)/* overload */;
	void __fastcall Rasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect, Gr32::TCustomBitmap32* Src)/* overload */;
	
__published:
	__property Gr32::TCustomSampler* Sampler = {read=FSampler, write=SetSampler};
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TRasterizer() { }
	
};


typedef System::TMetaClass* TRasterizerClass;

class PASCALIMPLEMENTATION TRegularRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
private:
	int FUpdateRowCount;
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
	
public:
	__fastcall virtual TRegularRasterizer();
	
__published:
	__property int UpdateRowCount = {read=FUpdateRowCount, write=FUpdateRowCount, nodefault};
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TRegularRasterizer() { }
	
};


class PASCALIMPLEMENTATION TSwizzlingRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
private:
	int FBlockSize;
	void __fastcall SetBlockSize(const int Value);
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
	
public:
	__fastcall virtual TSwizzlingRasterizer();
	
__published:
	__property int BlockSize = {read=FBlockSize, write=SetBlockSize, default=3};
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TSwizzlingRasterizer() { }
	
};


class PASCALIMPLEMENTATION TProgressiveRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
private:
	int FSteps;
	bool FUpdateRows;
	void __fastcall SetSteps(const int Value);
	void __fastcall SetUpdateRows(const bool Value);
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
	
public:
	__fastcall virtual TProgressiveRasterizer();
	
__published:
	__property int Steps = {read=FSteps, write=SetSteps, default=4};
	__property bool UpdateRows = {read=FUpdateRows, write=SetUpdateRows, default=1};
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TProgressiveRasterizer() { }
	
};


class PASCALIMPLEMENTATION TTesseralRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
public:
	/* TRasterizer.Create */ inline __fastcall virtual TTesseralRasterizer() : TRasterizer() { }
	
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TTesseralRasterizer() { }
	
};


class PASCALIMPLEMENTATION TContourRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
public:
	/* TRasterizer.Create */ inline __fastcall virtual TContourRasterizer() : TRasterizer() { }
	
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TContourRasterizer() { }
	
};


class PASCALIMPLEMENTATION TMultithreadedRegularRasterizer : public TRasterizer
{
	typedef TRasterizer inherited;
	
protected:
	virtual void __fastcall DoRasterize(Gr32::TCustomBitmap32* Dst, const Gr32::TRect &DstRect);
public:
	/* TRasterizer.Create */ inline __fastcall virtual TMultithreadedRegularRasterizer() : TRasterizer() { }
	
public:
	/* TThreadPersistent.Destroy */ inline __fastcall virtual ~TMultithreadedRegularRasterizer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TCombineInfo DEFAULT_COMBINE_INFO;
extern DELPHI_PACKAGE TRasterizerClass DefaultRasterizerClass;
extern DELPHI_PACKAGE int NumberOfProcessors;
extern DELPHI_PACKAGE TCombineInfo __fastcall CombineInfo(Gr32::TCustomBitmap32* Bitmap);
}	/* namespace Gr32_rasterizers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_RASTERIZERS)
using namespace Gr32_rasterizers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GR32_RasterizersHPP
