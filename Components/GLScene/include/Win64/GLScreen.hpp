// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glscreen.pas' rev: 36.00 (Windows)

#ifndef GlscreenHPP
#define GlscreenHPP

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
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Vcl.Forms.hpp>
#include <Glvectorgeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glscreen
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDisplayOptions;
struct TVideoMode;
//-- type declarations -------------------------------------------------------
typedef System::Byte TResolution;

enum DECLSPEC_DENUM TWindowAttribute : unsigned char { woDesktop, woStayOnTop, woTransparent };

typedef System::Set<TWindowAttribute, TWindowAttribute::woDesktop, TWindowAttribute::woTransparent> TWindowAttributes;

enum DECLSPEC_DENUM TWindowFitting : unsigned char { wfDefault, wfFitWindowToScreen, wfFitScreenToWindow };

class PASCALIMPLEMENTATION TGLDisplayOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FFullScreen;
	TResolution FScreenResolution;
	TWindowAttributes FWindowAttributes;
	TWindowFitting FWindowFitting;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool FullScreen = {read=FFullScreen, write=FFullScreen, default=0};
	__property TResolution ScreenResolution = {read=FScreenResolution, write=FScreenResolution, default=0};
	__property TWindowAttributes WindowAttributes = {read=FWindowAttributes, write=FWindowAttributes, default=0};
	__property TWindowFitting WindowFitting = {read=FWindowFitting, write=FWindowFitting, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDisplayOptions() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLDisplayOptions() : System::Classes::TPersistent() { }
	
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TVideoMode
{
public:
	System::Word Width;
	System::Word Height;
	System::Byte ColorDepth;
	System::Byte MaxFrequency;
	System::UnicodeString Description;
};
#pragma pack(pop)


typedef TVideoMode *PVideoMode;

typedef System::DynamicArray<TVideoMode> Glscreen__2;

//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Byte MaxVideoModes = System::Byte(0xc8);
static _DELPHI_CONST System::Int8 lcl_release = System::Int8(0x0);
extern DELPHI_PACKAGE int vNumberVideoModes;
extern DELPHI_PACKAGE int vCurrentVideoMode;
extern DELPHI_PACKAGE Glscreen__2 vVideoModes;
extern DELPHI_PACKAGE TResolution __fastcall GetIndexFromResolution(int XRes, int YRes, int BPP);
extern DELPHI_PACKAGE void __fastcall ReadVideoModes(void);
extern DELPHI_PACKAGE bool __fastcall SetFullscreenMode(TResolution modeIndex, int displayFrequency = 0x0);
extern DELPHI_PACKAGE void __fastcall ReadScreenImage(HDC Dest, int DestLeft, int DestTop, const Glvectorgeometry::TRectangle &SrcRect);
extern DELPHI_PACKAGE void __fastcall RestoreDefaultMode(void);
extern DELPHI_PACKAGE void __fastcall GLShowCursor(bool AShow);
extern DELPHI_PACKAGE void __fastcall GLSetCursorPos(int AScreenX, int AScreenY);
extern DELPHI_PACKAGE void __fastcall GLGetCursorPos(Winapi::Windows::TPoint &point);
extern DELPHI_PACKAGE int __fastcall GLGetScreenWidth(void);
extern DELPHI_PACKAGE int __fastcall GLGetScreenHeight(void);
}	/* namespace Glscreen */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCREEN)
using namespace Glscreen;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscreenHPP
