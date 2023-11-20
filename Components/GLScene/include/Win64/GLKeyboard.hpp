// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLKeyboard.pas' rev: 34.00 (Windows)

#ifndef GlkeyboardHPP
#define GlkeyboardHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glkeyboard
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef int TVirtualKeyCode;

//-- var, const, procedure ---------------------------------------------------
static const System::Byte VK_MOUSEWHEELUP = System::Byte(0x86);
static const System::Byte VK_MOUSEWHEELDOWN = System::Byte(0x87);
extern DELPHI_PACKAGE int vLastWheelDelta;
extern DELPHI_PACKAGE bool __fastcall IsKeyDown(System::WideChar c)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsKeyDown(int vk)/* overload */;
extern DELPHI_PACKAGE int __fastcall KeyPressed(int minVkCode = 0x0);
extern DELPHI_PACKAGE System::UnicodeString __fastcall VirtualKeyCodeToKeyName(int vk);
extern DELPHI_PACKAGE int __fastcall KeyNameToVirtualKeyCode(const System::UnicodeString keyName);
extern DELPHI_PACKAGE int __fastcall CharToVirtualKeyCode(System::WideChar c);
extern DELPHI_PACKAGE void __fastcall KeyboardNotifyWheelMoved(int wheelDelta);
}	/* namespace Glkeyboard */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLKEYBOARD)
using namespace Glkeyboard;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlkeyboardHPP
