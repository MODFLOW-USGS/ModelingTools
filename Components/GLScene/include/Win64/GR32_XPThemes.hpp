// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_XPThemes.pas' rev: 36.00 (Windows)

#ifndef Gr32_xpthemesHPP
#define Gr32_xpthemesHPP

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
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_xpthemes
{
//-- forward type declarations -----------------------------------------------
struct _MARGINS;
//-- type declarations -------------------------------------------------------
typedef Winapi::Windows::THandle HIMAGELIST;

typedef Winapi::Windows::THandle HTHEME;

struct DECLSPEC_DRECORD _MARGINS
{
public:
	int cxLeftWidth;
	int cxRightWidth;
	int cyTopHeight;
	int cyBottomHeight;
};


typedef _MARGINS MARGINS;

typedef _MARGINS *PMARGINS;

typedef _MARGINS TMargins;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool USE_THEMES;
extern DELPHI_PACKAGE Winapi::Windows::THandle SCROLLBAR_THEME;
extern DELPHI_PACKAGE Winapi::Windows::THandle GLOBALS_THEME;
static const System::Int8 THEMEMGR_VERSION = System::Int8(0x1);
static const System::Word WM_THEMECHANGED = System::Word(0x31a);
static const System::Int8 SBP_ARROWBTN = System::Int8(0x1);
static const System::Int8 SBP_THUMBBTNHORZ = System::Int8(0x2);
static const System::Int8 SBP_THUMBBTNVERT = System::Int8(0x3);
static const System::Int8 SBP_LOWERTRACKHORZ = System::Int8(0x4);
static const System::Int8 SBP_UPPERTRACKHORZ = System::Int8(0x5);
static const System::Int8 SBP_LOWERTRACKVERT = System::Int8(0x6);
static const System::Int8 SBP_UPPERTRACKVERT = System::Int8(0x7);
static const System::Int8 SBP_GRIPPERHORZ = System::Int8(0x8);
static const System::Int8 SBP_GRIPPERVERT = System::Int8(0x9);
static const System::Int8 SBP_SIZEBOX = System::Int8(0xa);
static const System::Int8 ABS_UPNORMAL = System::Int8(0x1);
static const System::Int8 ABS_UPHOT = System::Int8(0x2);
static const System::Int8 ABS_UPPRESSED = System::Int8(0x3);
static const System::Int8 ABS_UPDISABLED = System::Int8(0x4);
static const System::Int8 ABS_DOWNNORMAL = System::Int8(0x5);
static const System::Int8 ABS_DOWNHOT = System::Int8(0x6);
static const System::Int8 ABS_DOWNPRESSED = System::Int8(0x7);
static const System::Int8 ABS_DOWNDISABLED = System::Int8(0x8);
static const System::Int8 ABS_LEFTNORMAL = System::Int8(0x9);
static const System::Int8 ABS_LEFTHOT = System::Int8(0xa);
static const System::Int8 ABS_LEFTPRESSED = System::Int8(0xb);
static const System::Int8 ABS_LEFTDISABLED = System::Int8(0xc);
static const System::Int8 ABS_RIGHTNORMAL = System::Int8(0xd);
static const System::Int8 ABS_RIGHTHOT = System::Int8(0xe);
static const System::Int8 ABS_RIGHTPRESSED = System::Int8(0xf);
static const System::Int8 ABS_RIGHTDISABLED = System::Int8(0x10);
static const System::Int8 SCRBS_NORMAL = System::Int8(0x1);
static const System::Int8 SCRBS_HOT = System::Int8(0x2);
static const System::Int8 SCRBS_PRESSED = System::Int8(0x3);
static const System::Int8 SCRBS_DISABLED = System::Int8(0x4);
static const System::Int8 SZB_RIGHTALIGN = System::Int8(0x1);
static const System::Int8 SZB_LEFTALIGN = System::Int8(0x2);
extern DELPHI_PACKAGE unsigned __int64 __stdcall (*OpenThemeData)(HWND hwnd, System::WideChar * pszClassList);
extern DELPHI_PACKAGE HRESULT __stdcall (*CloseThemeData)(HTHEME hTheme);
extern DELPHI_PACKAGE HRESULT __stdcall (*DrawThemeBackground)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &Rect, System::Types::PRect pClipRect);
extern DELPHI_PACKAGE HRESULT __stdcall (*DrawThemeEdge)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const System::Types::TRect &pDestRect, unsigned uEdge, unsigned uFlags, System::Types::PRect pContentRect);
extern DELPHI_PACKAGE HRESULT __stdcall (*GetThemeColor)(HTHEME hTheme, int iPartId, int iStateId, int iPropId, unsigned &pColor);
extern DELPHI_PACKAGE HRESULT __stdcall (*GetThemeMetric)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, int iPropId, int &piVal);
extern DELPHI_PACKAGE HRESULT __stdcall (*GetThemeMargins)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, int iPropId, System::Types::PRect prc, MARGINS &pMargins);
extern DELPHI_PACKAGE HRESULT __stdcall (*SetWindowTheme)(HWND hwnd, System::WideChar * pszSubAppName, System::WideChar * pszSubIdList);
extern DELPHI_PACKAGE System::LongBool __stdcall (*IsThemeActive)(void);
extern DELPHI_PACKAGE System::LongBool __stdcall (*IsAppThemed)(void);
extern DELPHI_PACKAGE HRESULT __stdcall (*EnableTheming)(System::LongBool fEnable);
}	/* namespace Gr32_xpthemes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_XPTHEMES)
using namespace Gr32_xpthemes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_xpthemesHPP
