// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPlugInIntf.pas' rev: 36.00 (Windows)

#ifndef GLPlugInIntfHPP
#define GLPlugInIntfHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glpluginintf
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TPIServiceType : unsigned char { stRaw, stObject, stBitmap, stTexture, stImport, stExport };

typedef System::Set<TPIServiceType, TPIServiceType::stRaw, TPIServiceType::stExport> TPIServices;

typedef void __stdcall (*TEnumCallBack)(char * Name);

typedef void __stdcall (*TEnumResourceNames)(TPIServiceType Service, TEnumCallBack Callback);

typedef TPIServices __stdcall (*TGetServices)(void);

typedef char * __stdcall (*TGetVendor)(void);

typedef char * __stdcall (*TGetDescription)(void);

typedef char * __stdcall (*TGetVersion)(void);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glpluginintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPLUGININTF)
using namespace Glpluginintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLPlugInIntfHPP
