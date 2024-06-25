// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Backends_Generic.pas' rev: 36.00 (Windows)

#ifndef Gr32_backends_genericHPP
#define Gr32_backends_genericHPP

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
#include <System.Classes.hpp>
#include <GR32.hpp>
#include <GR32_Backends.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_backends_generic
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMemoryBackend;
class DELPHICLASS TMMFBackend;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMemoryBackend : public Gr32::TCustomBackend
{
	typedef Gr32::TCustomBackend inherited;
	
protected:
	virtual void __fastcall InitializeSurface(int NewWidth, int NewHeight, bool ClearBuffer);
	virtual void __fastcall FinalizeSurface();
public:
	/* TCustomBackend.Create */ inline __fastcall virtual TMemoryBackend()/* overload */ : Gr32::TCustomBackend() { }
	/* TCustomBackend.Create */ inline __fastcall virtual TMemoryBackend(Gr32::TCustomBitmap32* Owner)/* overload */ : Gr32::TCustomBackend(Owner) { }
	/* TCustomBackend.Destroy */ inline __fastcall virtual ~TMemoryBackend() { }
	
};


class PASCALIMPLEMENTATION TMMFBackend : public TMemoryBackend
{
	typedef TMemoryBackend inherited;
	
private:
	Winapi::Windows::THandle FMapHandle;
	bool FMapIsTemporary;
	Winapi::Windows::THandle FMapFileHandle;
	System::UnicodeString FMapFileName;
	
protected:
	virtual void __fastcall InitializeSurface(int NewWidth, int NewHeight, bool ClearBuffer);
	virtual void __fastcall FinalizeSurface();
	
public:
	__fastcall virtual TMMFBackend(Gr32::TCustomBitmap32* Owner, bool IsTemporary, const System::UnicodeString MapFileName);
	__fastcall virtual ~TMMFBackend();
	__classmethod void __fastcall InitializeFileMapping(Winapi::Windows::THandle &MapHandle, Winapi::Windows::THandle &MapFileHandle, System::UnicodeString &MapFileName);
	__classmethod void __fastcall DeinitializeFileMapping(Winapi::Windows::THandle MapHandle, Winapi::Windows::THandle MapFileHandle, const System::UnicodeString MapFileName);
	__classmethod void __fastcall CreateFileMapping(Winapi::Windows::THandle &MapHandle, Winapi::Windows::THandle &MapFileHandle, System::UnicodeString &MapFileName, bool IsTemporary, int NewWidth, int NewHeight);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gr32_backends_generic */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_BACKENDS_GENERIC)
using namespace Gr32_backends_generic;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_backends_genericHPP
