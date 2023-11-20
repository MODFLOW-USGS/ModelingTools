// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Backends_VCL.pas' rev: 35.00 (Windows)

#ifndef Gr32_backends_vclHPP
#define Gr32_backends_vclHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <GR32.hpp>
#include <GR32_Backends.hpp>
#include <GR32_Containers.hpp>
#include <GR32_Image.hpp>
#include <GR32_Backends_Generic.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_backends_vcl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGDIBackend;
class DELPHICLASS TGDIMMFBackend;
class DELPHICLASS TGDIMemoryBackend;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGDIBackend : public Gr32::TCustomBackend
{
	typedef Gr32::TCustomBackend inherited;
	
private:
	void __fastcall FontChangedHandler(System::TObject* Sender);
	void __fastcall CanvasChangedHandler(System::TObject* Sender);
	void __fastcall CanvasChanged();
	void __fastcall FontChanged();
	
protected:
	tagBITMAPINFO FBitmapInfo;
	HBITMAP FBitmapHandle;
	HDC FHDC;
	Vcl::Graphics::TFont* FFont;
	Vcl::Graphics::TCanvas* FCanvas;
	HFONT FFontHandle;
	NativeUInt FMapHandle;
	System::Classes::TNotifyEvent FOnFontChange;
	System::Classes::TNotifyEvent FOnCanvasChange;
	virtual void __fastcall InitializeSurface(int NewWidth, int NewHeight, bool ClearBuffer);
	virtual void __fastcall FinalizeSurface();
	virtual void __fastcall PrepareFileMapping(int NewWidth, int NewHeight);
	
public:
	__fastcall virtual TGDIBackend()/* overload */;
	__fastcall virtual ~TGDIBackend();
	virtual void __fastcall Changed();
	virtual bool __fastcall Empty();
	void __fastcall ImageNeeded();
	void __fastcall CheckPixmap();
	void __fastcall DoPaint(Gr32::TBitmap32* ABuffer, Gr32_containers::TRectList* AInvalidRects, Vcl::Graphics::TCanvas* ACanvas, Gr32_image::TCustomPaintBox32* APaintBox);
	tagBITMAPINFO __fastcall GetBitmapInfo();
	NativeUInt __fastcall GetBitmapHandle();
	__property tagBITMAPINFO BitmapInfo = {read=GetBitmapInfo};
	__property NativeUInt BitmapHandle = {read=GetBitmapHandle, nodefault};
	HDC __fastcall GetHandle();
	void __fastcall Draw(const System::Types::TRect &DstRect, const System::Types::TRect &SrcRect, HDC hSrc)/* overload */;
	void __fastcall DrawTo(HDC hDst, int DstX, int DstY)/* overload */;
	void __fastcall DrawTo(HDC hDst, const System::Types::TRect &DstRect, const System::Types::TRect &SrcRect)/* overload */;
	__property HDC Handle = {read=GetHandle, nodefault};
	void __fastcall Textout(int X, int Y, const System::UnicodeString Text)/* overload */;
	void __fastcall Textout(int X, int Y, const System::Types::TRect &ClipRect, const System::UnicodeString Text)/* overload */;
	void __fastcall Textout(System::Types::TRect &DstRect, const unsigned Flags, const System::UnicodeString Text)/* overload */;
	System::Types::TSize __fastcall TextExtent(const System::UnicodeString Text);
	void __fastcall TextoutW(int X, int Y, const System::WideString Text)/* overload */;
	void __fastcall TextoutW(int X, int Y, const System::Types::TRect &ClipRect, const System::WideString Text)/* overload */;
	void __fastcall TextoutW(System::Types::TRect &DstRect, const unsigned Flags, const System::WideString Text)/* overload */;
	System::Types::TSize __fastcall TextExtentW(const System::WideString Text);
	System::Classes::TNotifyEvent __fastcall GetOnFontChange();
	void __fastcall SetOnFontChange(System::Classes::TNotifyEvent Handler);
	Vcl::Graphics::TFont* __fastcall GetFont();
	void __fastcall SetFont(Vcl::Graphics::TFont* const Font);
	void __fastcall UpdateFont();
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property System::Classes::TNotifyEvent OnFontChange = {read=FOnFontChange, write=FOnFontChange};
	System::Classes::TNotifyEvent __fastcall GetCanvasChange();
	void __fastcall SetCanvasChange(System::Classes::TNotifyEvent Handler);
	Vcl::Graphics::TCanvas* __fastcall GetCanvas();
	void __fastcall DeleteCanvas();
	bool __fastcall CanvasAllocated();
	__property Vcl::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property System::Classes::TNotifyEvent OnCanvasChange = {read=GetCanvasChange, write=SetCanvasChange};
public:
	/* TCustomBackend.Create */ inline __fastcall virtual TGDIBackend(Gr32::TCustomBitmap32* Owner)/* overload */ : Gr32::TCustomBackend(Owner) { }
	
private:
	void *__ICanvasSupport;	// Gr32_backends::ICanvasSupport 
	void *__IFontSupport;	// Gr32_backends::IFontSupport 
	void *__ITextSupport;	// Gr32_backends::ITextSupport 
	void *__IDeviceContextSupport;	// Gr32_backends::IDeviceContextSupport 
	void *__IBitmapContextSupport;	// Gr32_backends::IBitmapContextSupport 
	void *__IPaintSupport;	// Gr32_backends::IPaintSupport 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {5ACFEEC7-0123-4AD8-8AE6-145718438E01}
	operator Gr32_backends::_di_ICanvasSupport()
	{
		Gr32_backends::_di_ICanvasSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::ICanvasSupport*(void) { return (Gr32_backends::ICanvasSupport*)&__ICanvasSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {67C73044-1EFF-4FDE-AEA2-56BFADA50A48}
	operator Gr32_backends::_di_IFontSupport()
	{
		Gr32_backends::_di_IFontSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IFontSupport*(void) { return (Gr32_backends::IFontSupport*)&__IFontSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {225997CC-958A-423E-8B60-9EDE0D3B53B5}
	operator Gr32_backends::_di_ITextSupport()
	{
		Gr32_backends::_di_ITextSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::ITextSupport*(void) { return (Gr32_backends::ITextSupport*)&__ITextSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {DD1109DA-4019-4A5C-A450-3631A73CF288}
	operator Gr32_backends::_di_IDeviceContextSupport()
	{
		Gr32_backends::_di_IDeviceContextSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IDeviceContextSupport*(void) { return (Gr32_backends::IDeviceContextSupport*)&__IDeviceContextSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {DF0F9475-BA13-4C6B-81C3-D138624C4D08}
	operator Gr32_backends::_di_IBitmapContextSupport()
	{
		Gr32_backends::_di_IBitmapContextSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IBitmapContextSupport*(void) { return (Gr32_backends::IBitmapContextSupport*)&__IBitmapContextSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {CE64DBEE-C4A9-4E8E-ABCA-1B1FD6F45924}
	operator Gr32_backends::_di_IPaintSupport()
	{
		Gr32_backends::_di_IPaintSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IPaintSupport*(void) { return (Gr32_backends::IPaintSupport*)&__IPaintSupport; }
	#endif
	
};


class PASCALIMPLEMENTATION TGDIMMFBackend : public TGDIBackend
{
	typedef TGDIBackend inherited;
	
private:
	NativeUInt FMapFileHandle;
	bool FMapIsTemporary;
	System::UnicodeString FMapFileName;
	
protected:
	virtual void __fastcall PrepareFileMapping(int NewWidth, int NewHeight);
	
public:
	__fastcall virtual TGDIMMFBackend(Gr32::TBitmap32* Owner, bool IsTemporary, const System::UnicodeString MapFileName);
	__fastcall virtual ~TGDIMMFBackend();
};


class PASCALIMPLEMENTATION TGDIMemoryBackend : public Gr32_backends_generic::TMemoryBackend
{
	typedef Gr32_backends_generic::TMemoryBackend inherited;
	
private:
	void __fastcall DoPaintRect(Gr32::TBitmap32* ABuffer, const System::Types::TRect &ARect, Vcl::Graphics::TCanvas* ACanvas);
	HDC __fastcall GetHandle();
	
protected:
	tagBITMAPINFO FBitmapInfo;
	virtual void __fastcall InitializeSurface(int NewWidth, int NewHeight, bool ClearBuffer);
	
public:
	__fastcall virtual TGDIMemoryBackend()/* overload */;
	void __fastcall ImageNeeded();
	void __fastcall CheckPixmap();
	void __fastcall DoPaint(Gr32::TBitmap32* ABuffer, Gr32_containers::TRectList* AInvalidRects, Vcl::Graphics::TCanvas* ACanvas, Gr32_image::TCustomPaintBox32* APaintBox);
	void __fastcall Draw(const System::Types::TRect &DstRect, const System::Types::TRect &SrcRect, HDC hSrc)/* overload */;
	void __fastcall DrawTo(HDC hDst, int DstX, int DstY)/* overload */;
	void __fastcall DrawTo(HDC hDst, const System::Types::TRect &DstRect, const System::Types::TRect &SrcRect)/* overload */;
public:
	/* TCustomBackend.Create */ inline __fastcall virtual TGDIMemoryBackend(Gr32::TCustomBitmap32* Owner)/* overload */ : Gr32_backends_generic::TMemoryBackend(Owner) { }
	/* TCustomBackend.Destroy */ inline __fastcall virtual ~TGDIMemoryBackend() { }
	
private:
	void *__IDeviceContextSupport;	// Gr32_backends::IDeviceContextSupport 
	void *__IPaintSupport;	// Gr32_backends::IPaintSupport 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {DD1109DA-4019-4A5C-A450-3631A73CF288}
	operator Gr32_backends::_di_IDeviceContextSupport()
	{
		Gr32_backends::_di_IDeviceContextSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IDeviceContextSupport*(void) { return (Gr32_backends::IDeviceContextSupport*)&__IDeviceContextSupport; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {CE64DBEE-C4A9-4E8E-ABCA-1B1FD6F45924}
	operator Gr32_backends::_di_IPaintSupport()
	{
		Gr32_backends::_di_IPaintSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gr32_backends::IPaintSupport*(void) { return (Gr32_backends::IPaintSupport*)&__IPaintSupport; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gr32_backends_vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_BACKENDS_VCL)
using namespace Gr32_backends_vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_backends_vclHPP
