// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCrossPlatform.pas' rev: 35.00 (Windows)

#ifndef GlcrossplatformHPP
#define GlcrossplatformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <Vcl.Consts.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcrossplatform
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLComponent;
//-- type declarations -------------------------------------------------------
typedef System::Word THalfFloat;

typedef THalfFloat *PHalfFloat;

typedef void __fastcall (__closure *TGLMouseEvent)(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);

typedef System::Sysutils::EOSError EGLOSError;

class PASCALIMPLEMENTATION TGLComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TGLComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLComponent() { }
	
};


typedef System::UnicodeString __fastcall (*TProjectTargetNameFunc)(void);

//-- var, const, procedure ---------------------------------------------------
static const System::Word FONT_CHARS_COUNT = System::Word(0x7e8);
extern DELPHI_PACKAGE bool IsDesignTime;
extern DELPHI_PACKAGE TProjectTargetNameFunc vProjectTargetName;
extern DELPHI_PACKAGE bool __fastcall IsSubComponent(System::Classes::TComponent* const AComponent);
extern DELPHI_PACKAGE void __fastcall MakeSubComponent(System::Classes::TComponent* const AComponent, const bool Value);
extern DELPHI_PACKAGE bool __fastcall AnsiStartsText(const System::UnicodeString ASubText, const System::UnicodeString AText);
extern DELPHI_PACKAGE int __fastcall GLOKMessageBox(const System::UnicodeString Text, const System::UnicodeString Caption);
extern DELPHI_PACKAGE void __fastcall GLLoadBitmapFromInstance(int Instance, Vcl::Graphics::TBitmap* ABitmap, const System::UnicodeString AName);
extern DELPHI_PACKAGE void __fastcall ShowHTMLUrl(const System::UnicodeString Url);
extern DELPHI_PACKAGE System::Types::TRect __fastcall GetGLRect(const int aLeft, const int aTop, const int aRight, const int aBottom);
extern DELPHI_PACKAGE void __fastcall InflateGLRect(System::Types::TRect &aRect, int dx, int dy);
extern DELPHI_PACKAGE void __fastcall IntersectGLRect(System::Types::TRect &aRect, const System::Types::TRect &rect2);
extern DELPHI_PACKAGE void __fastcall RaiseLastOSError(void);
extern DELPHI_PACKAGE int __fastcall GetDeviceLogicalPixelsX(HDC device);
extern DELPHI_PACKAGE int __fastcall GetCurrentColorDepth(void);
extern DELPHI_PACKAGE int __fastcall PixelFormatToColorBits(Vcl::Graphics::TPixelFormat aPixelFormat);
extern DELPHI_PACKAGE void __fastcall FixPathDelimiter(System::UnicodeString &S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RelativePath(const System::UnicodeString S);
extern DELPHI_PACKAGE void __fastcall QueryPerformanceCounter(/* out */ __int64 &val);
extern DELPHI_PACKAGE bool __fastcall QueryPerformanceFrequency(/* out */ __int64 &val);
extern DELPHI_PACKAGE __int64 __fastcall StartPrecisionTimer(void);
extern DELPHI_PACKAGE double __fastcall PrecisionTimerLap(const __int64 precisionTimer);
extern DELPHI_PACKAGE double __fastcall StopPrecisionTimer(const __int64 precisionTimer);
extern DELPHI_PACKAGE double __fastcall AppTime(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FindUnitName(System::TObject* anObject)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall FindUnitName(System::TClass aClass)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetExeDirectory(void);
extern DELPHI_PACKAGE float __fastcall HalfToFloat(THalfFloat Half);
extern DELPHI_PACKAGE THalfFloat __fastcall FloatToHalf(float Float);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetValueFromStringsIndex(System::Classes::TStrings* const AStrings, const int AIndex);
extern DELPHI_PACKAGE bool __fastcall IsDirectoryWriteable(const System::UnicodeString AName);
extern DELPHI_PACKAGE System::WideChar __fastcall CharToWideChar(const char AChar);
}	/* namespace Glcrossplatform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCROSSPLATFORM)
using namespace Glcrossplatform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcrossplatformHPP
