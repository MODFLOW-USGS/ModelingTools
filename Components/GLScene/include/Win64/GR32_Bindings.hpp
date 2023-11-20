// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Bindings.pas' rev: 34.00 (Windows)

#ifndef Gr32_bindingsHPP
#define Gr32_bindingsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GR32_System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_bindings
{
//-- forward type declarations -----------------------------------------------
struct TFunctionInfo;
struct TFunctionBinding;
class DELPHICLASS TFunctionRegistry;
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString TFunctionName;

typedef int TFunctionID;

typedef TFunctionInfo *PFunctionInfo;

struct DECLSPEC_DRECORD TFunctionInfo
{
public:
	int FunctionID;
	void *Proc;
	Gr32_system::TCPUFeatures CPUFeatures;
	int Flags;
};


typedef int __fastcall (*TFunctionPriority)(PFunctionInfo Info);

typedef TFunctionBinding *PFunctionBinding;

struct DECLSPEC_DRECORD TFunctionBinding
{
public:
	int FunctionID;
	void * *BindVariable;
};


class PASCALIMPLEMENTATION TFunctionRegistry : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TList* FItems;
	System::Classes::TList* FBindings;
	System::UnicodeString FName;
	void __fastcall SetName(const System::UnicodeString Value);
	PFunctionInfo __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, const PFunctionInfo Value);
	
public:
	__fastcall virtual TFunctionRegistry();
	__fastcall virtual ~TFunctionRegistry();
	void __fastcall Clear();
	void __fastcall Add(int FunctionID, void * Proc, Gr32_system::TCPUFeatures CPUFeatures = Gr32_system::TCPUFeatures() , int Flags = 0x0);
	void __fastcall RegisterBinding(int FunctionID, System::PPointer BindVariable);
	void __fastcall RebindAll(TFunctionPriority PriorityCallback = 0x0);
	void __fastcall Rebind(int FunctionID, TFunctionPriority PriorityCallback = 0x0);
	void * __fastcall FindFunction(int FunctionID, TFunctionPriority PriorityCallback = 0x0);
	__property PFunctionInfo Items[int Index] = {read=GetItems, write=SetItems};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TFunctionPriority DefaultPriority;
extern DELPHI_PACKAGE int INVALID_PRIORITY;
extern DELPHI_PACKAGE TFunctionRegistry* __fastcall NewRegistry(const System::UnicodeString Name = System::UnicodeString());
extern DELPHI_PACKAGE int __fastcall DefaultPriorityProc(PFunctionInfo Info);
}	/* namespace Gr32_bindings */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_BINDINGS)
using namespace Gr32_bindings;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_bindingsHPP
