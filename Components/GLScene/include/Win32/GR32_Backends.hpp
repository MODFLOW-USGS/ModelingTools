// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Backends.pas' rev: 36.00 (Windows)

#ifndef GR32_BackendsHPP
#define GR32_BackendsHPP

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
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>
#include <GR32_Containers.hpp>
#include <GR32_Image.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_backends
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITextSupport;
typedef System::DelphiInterface<ITextSupport> _di_ITextSupport;
__interface DELPHIINTERFACE IFontSupport;
typedef System::DelphiInterface<IFontSupport> _di_IFontSupport;
__interface DELPHIINTERFACE ICanvasSupport;
typedef System::DelphiInterface<ICanvasSupport> _di_ICanvasSupport;
__interface DELPHIINTERFACE IDeviceContextSupport;
typedef System::DelphiInterface<IDeviceContextSupport> _di_IDeviceContextSupport;
__interface DELPHIINTERFACE IBitmapContextSupport;
typedef System::DelphiInterface<IBitmapContextSupport> _di_IBitmapContextSupport;
__interface DELPHIINTERFACE IPaintSupport;
typedef System::DelphiInterface<IPaintSupport> _di_IPaintSupport;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{225997CC-958A-423E-8B60-9EDE0D3B53B5}") ITextSupport  : public System::IInterface 
{
	virtual void __fastcall Textout(int X, int Y, const System::UnicodeString Text) = 0 /* overload */;
	virtual void __fastcall Textout(int X, int Y, const Gr32::TRect &ClipRect, const System::UnicodeString Text) = 0 /* overload */;
	virtual void __fastcall Textout(Gr32::TRect &DstRect, const unsigned Flags, const System::UnicodeString Text) = 0 /* overload */;
	virtual System::Types::TSize __fastcall TextExtent(const System::UnicodeString Text) = 0 ;
	virtual void __fastcall TextoutW(int X, int Y, const System::WideString Text) = 0 /* overload */;
	virtual void __fastcall TextoutW(int X, int Y, const Gr32::TRect &ClipRect, const System::WideString Text) = 0 /* overload */;
	virtual void __fastcall TextoutW(Gr32::TRect &DstRect, const unsigned Flags, const System::WideString Text) = 0 /* overload */;
	virtual System::Types::TSize __fastcall TextExtentW(const System::WideString Text) = 0 ;
};

__interface  INTERFACE_UUID("{67C73044-1EFF-4FDE-AEA2-56BFADA50A48}") IFontSupport  : public System::IInterface 
{
	virtual System::Classes::TNotifyEvent __fastcall GetOnFontChange() = 0 ;
	virtual void __fastcall SetOnFontChange(System::Classes::TNotifyEvent Handler) = 0 ;
	virtual Vcl::Graphics::TFont* __fastcall GetFont() = 0 ;
	virtual void __fastcall SetFont(Vcl::Graphics::TFont* const Font) = 0 ;
	virtual void __fastcall UpdateFont() = 0 ;
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property System::Classes::TNotifyEvent OnFontChange = {read=GetOnFontChange, write=SetOnFontChange};
};

__interface  INTERFACE_UUID("{5ACFEEC7-0123-4AD8-8AE6-145718438E01}") ICanvasSupport  : public System::IInterface 
{
	virtual System::Classes::TNotifyEvent __fastcall GetCanvasChange() = 0 ;
	virtual void __fastcall SetCanvasChange(System::Classes::TNotifyEvent Handler) = 0 ;
	virtual Vcl::Graphics::TCanvas* __fastcall GetCanvas() = 0 ;
	virtual void __fastcall DeleteCanvas() = 0 ;
	virtual bool __fastcall CanvasAllocated() = 0 ;
	__property Vcl::Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property System::Classes::TNotifyEvent OnCanvasChange = {read=GetCanvasChange, write=SetCanvasChange};
};

__interface  INTERFACE_UUID("{DD1109DA-4019-4A5C-A450-3631A73CF288}") IDeviceContextSupport  : public System::IInterface 
{
	virtual HDC __fastcall GetHandle() = 0 ;
	virtual void __fastcall Draw(const Gr32::TRect &DstRect, const Gr32::TRect &SrcRect, HDC hSrc) = 0 ;
	virtual void __fastcall DrawTo(HDC hDst, int DstX, int DstY) = 0 /* overload */;
	virtual void __fastcall DrawTo(HDC hDst, const Gr32::TRect &DstRect, const Gr32::TRect &SrcRect) = 0 /* overload */;
	__property HDC Handle = {read=GetHandle};
};

__interface  INTERFACE_UUID("{DF0F9475-BA13-4C6B-81C3-D138624C4D08}") IBitmapContextSupport  : public System::IInterface 
{
	virtual Winapi::Windows::TBitmapInfo __fastcall GetBitmapInfo() = 0 ;
	virtual Winapi::Windows::THandle __fastcall GetBitmapHandle() = 0 ;
	__property Winapi::Windows::TBitmapInfo BitmapInfo = {read=GetBitmapInfo};
	__property Winapi::Windows::THandle BitmapHandle = {read=GetBitmapHandle};
};

__interface  INTERFACE_UUID("{CE64DBEE-C4A9-4E8E-ABCA-1B1FD6F45924}") IPaintSupport  : public System::IInterface 
{
	virtual void __fastcall ImageNeeded() = 0 ;
	virtual void __fastcall CheckPixmap() = 0 ;
	virtual void __fastcall DoPaint(Gr32::TBitmap32* ABuffer, Gr32_containers::TRectList* AInvalidRects, Vcl::Graphics::TCanvas* ACanvas, Gr32_image::TCustomPaintBox32* APaintBox) = 0 ;
};

enum DECLSPEC_DENUM TRequireOperatorMode : unsigned char { romAnd, romOr };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _RCStrCannotAllocateDIBHandle;
#define Gr32_backends_RCStrCannotAllocateDIBHandle System::LoadResourceString(&Gr32_backends::_RCStrCannotAllocateDIBHandle)
extern DELPHI_PACKAGE System::ResourceString _RCStrCannotCreateCompatibleDC;
#define Gr32_backends_RCStrCannotCreateCompatibleDC System::LoadResourceString(&Gr32_backends::_RCStrCannotCreateCompatibleDC)
extern DELPHI_PACKAGE System::ResourceString _RCStrCannotSelectAnObjectIntoDC;
#define Gr32_backends_RCStrCannotSelectAnObjectIntoDC System::LoadResourceString(&Gr32_backends::_RCStrCannotSelectAnObjectIntoDC)
extern DELPHI_PACKAGE void __fastcall RequireBackendSupport(Gr32::TCustomBitmap32* TargetBitmap, GUID *RequiredInterfaces, const System::NativeInt RequiredInterfaces_High, TRequireOperatorMode Mode, bool UseOptimizedDestructiveSwitchMethod, /* out */ Gr32::TCustomBackend* &ReleasedBackend);
extern DELPHI_PACKAGE void __fastcall RestoreBackend(Gr32::TCustomBitmap32* TargetBitmap, Gr32::TCustomBackend* const SavedBackend);
}	/* namespace Gr32_backends */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_BACKENDS)
using namespace Gr32_backends;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GR32_BackendsHPP
