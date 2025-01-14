// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPlugInManager.pas' rev: 36.00 (Windows)

#ifndef GlpluginmanagerHPP
#define GlpluginmanagerHPP

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
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Forms.hpp>
#include <GLPlugInIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glpluginmanager
{
//-- forward type declarations -----------------------------------------------
struct TGLPlugInEntry;
class DELPHICLASS TGLResourceManager;
class DELPHICLASS TGLPlugInList;
struct TResManagerEntry;
class DELPHICLASS TGLPlugInManager;
//-- type declarations -------------------------------------------------------
typedef TGLPlugInEntry *PPlugInEntry;

struct DECLSPEC_DRECORD TGLPlugInEntry
{
public:
	System::Sysutils::TFileName Path;
	Winapi::Windows::HINST Handle;
	int FileSize;
	System::TDateTime FileDate;
	Glpluginintf::TEnumResourceNames EnumResourcenames;
	Glpluginintf::TGetServices GetServices;
	Glpluginintf::TGetVendor GetVendor;
	Glpluginintf::TGetDescription GetDescription;
	Glpluginintf::TGetVersion GetVersion;
};


class PASCALIMPLEMENTATION TGLResourceManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	virtual void __fastcall Notify(TGLPlugInManager* Sender, System::Classes::TOperation Operation, Glpluginintf::TPIServiceType Service, int PlugIn) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TGLResourceManager(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLResourceManager() { }
	
};


class PASCALIMPLEMENTATION TGLPlugInList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	PPlugInEntry operator[](int Index) { return this->Objects[Index]; }
	
private:
	TGLPlugInManager* FOwner;
	PPlugInEntry __fastcall GetPlugInEntry(int Index);
	void __fastcall SetPlugInEntry(int Index, PPlugInEntry AEntry);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadPlugIns(System::Classes::TReader* Reader);
	void __fastcall WritePlugIns(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TGLPlugInList(TGLPlugInManager* AOwner);
	void __fastcall ClearList();
	__property PPlugInEntry Objects[int Index] = {read=GetPlugInEntry, write=SetPlugInEntry/*, default*/};
	__property TGLPlugInManager* Owner = {read=FOwner};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLPlugInList() { }
	
};


typedef TResManagerEntry *PResManagerEntry;

struct DECLSPEC_DRECORD TResManagerEntry
{
public:
	TGLResourceManager* Manager;
	Glpluginintf::TPIServices Services;
};


class PASCALIMPLEMENTATION TGLPlugInManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLPlugInList* FLibraryList;
	System::Classes::TList* FResManagerList;
	
protected:
	void __fastcall DoNotify(System::Classes::TOperation Operation, Glpluginintf::TPIServiceType Service, int PlugIn);
	PResManagerEntry __fastcall FindResManager(TGLResourceManager* AManager);
	int __fastcall GetIndexFromFilename(System::UnicodeString FileName);
	PPlugInEntry __fastcall GetPlugInFromFilename(System::UnicodeString FileName);
	
public:
	__fastcall virtual TGLPlugInManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPlugInManager();
	int __fastcall AddPlugIn(System::Sysutils::TFileName Path);
	void __fastcall EditPlugInList();
	void __fastcall RegisterResourceManager(TGLResourceManager* AManager, Glpluginintf::TPIServices Services);
	void __fastcall RemovePlugIn(int Index);
	void __fastcall UnRegisterRessourceManager(TGLResourceManager* AManager, Glpluginintf::TPIServices Services);
	
__published:
	__property TGLPlugInList* PlugIns = {read=FLibraryList, write=FLibraryList};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glpluginmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPLUGINMANAGER)
using namespace Glpluginmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlpluginmanagerHPP
