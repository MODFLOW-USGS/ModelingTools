// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_System.pas' rev: 35.00 (Windows)

#ifndef Gr32_systemHPP
#define Gr32_systemHPP

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

namespace Gr32_system
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPerfTimer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TPerfTimer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__int64 FFrequency;
	__int64 FPerformanceCountStart;
	__int64 FPerformanceCountStop;
	
public:
	void __fastcall Start();
	System::UnicodeString __fastcall ReadNanoseconds();
	System::UnicodeString __fastcall ReadMilliseconds();
	System::UnicodeString __fastcall ReadSeconds();
	__int64 __fastcall ReadValue();
public:
	/* TObject.Create */ inline __fastcall TPerfTimer() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPerfTimer() { }
	
};


enum DECLSPEC_DENUM TCPUInstructionSet : unsigned char { ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt };

typedef System::Set<TCPUInstructionSet, TCPUInstructionSet::ciMMX, TCPUInstructionSet::ci3DNowExt> TCPUFeatures;

typedef TCPUFeatures *PCPUFeatures;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TPerfTimer* GlobalPerfTimer;
extern DELPHI_PACKAGE unsigned __fastcall GetTickCount(void);
extern DELPHI_PACKAGE unsigned __fastcall GetProcessorCount(void);
extern DELPHI_PACKAGE bool __fastcall HasInstructionSet(const TCPUInstructionSet InstructionSet);
extern DELPHI_PACKAGE TCPUFeatures __fastcall CPUFeatures(void);
}	/* namespace Gr32_system */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_SYSTEM)
using namespace Gr32_system;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_systemHPP
