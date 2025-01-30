// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSceneFormDesign.pas' rev: 36.00 (Windows)

#ifndef GLSceneFormDesignHPP
#define GLSceneFormDesignHPP

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
#include <Vcl.Forms.hpp>
#include <ToolsAPI.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsceneformdesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBaseSceneFormWizard;
class DELPHICLASS TGLSimpleSceneFormWizard;
class DELPHICLASS TGLExtendedSceneFormWizard;
class DELPHICLASS TGLBaseSceneProjectCreator;
class DELPHICLASS TGLBaseSceneProjectWizard;
class DELPHICLASS TGLSimpleSceneProjectWizard;
class DELPHICLASS TGLSimpleSceneProjectCreator;
class DELPHICLASS TGLExtendedSceneProjectWizard;
class DELPHICLASS TGLExtendedSceneProjectCreator;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneFormWizard : public Toolsapi::TNotifierObject
{
	typedef Toolsapi::TNotifierObject inherited;
	
private:
	System::UnicodeString FUnitIdent;
	System::UnicodeString FClassName;
	System::UnicodeString FFileName;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	Toolsapi::TWizardState __fastcall GetState();
	virtual void __fastcall Execute();
	System::UnicodeString __fastcall GetAuthor();
	System::UnicodeString __fastcall GetComment();
	System::UnicodeString __fastcall GetPage();
	unsigned __fastcall GetGlyph();
	System::UnicodeString __fastcall GetCreatorType();
	bool __fastcall GetExisting();
	System::UnicodeString __fastcall GetFileSystem();
	Toolsapi::_di_IOTAModule __fastcall GetOwner();
	bool __fastcall GetUnnamed();
	System::UnicodeString __fastcall GetAncestorName();
	System::UnicodeString __fastcall GetImplFileName();
	System::UnicodeString __fastcall GetIntfFileName();
	System::UnicodeString __fastcall GetFormName();
	bool __fastcall GetMainForm();
	bool __fastcall GetShowForm();
	bool __fastcall GetShowSource();
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	Toolsapi::_di_IOTAFile __fastcall NewIntfSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	void __fastcall FormCreated(const Toolsapi::_di_IOTAFormEditor FormEditor);
	
public:
	__fastcall TGLBaseSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName);
	System::UnicodeString __fastcall GetDesigner();
	Toolsapi::_di_IOTAGalleryCategory __fastcall GetGalleryCategory();
	System::UnicodeString __fastcall GetPersonality();
public:
	/* TObject.Create */ inline __fastcall TGLBaseSceneFormWizard() : Toolsapi::TNotifierObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneFormWizard() { }
	
private:
	void *__IOTARepositoryWizard80;	// Toolsapi::IOTARepositoryWizard80 
	void *__IOTAModuleCreator;	// Toolsapi::IOTAModuleCreator 
	void *__IOTAFormWizard;	// Toolsapi::IOTAFormWizard 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {D7714D41-BC4A-445E-B695-25A65C2F561E}
	operator Toolsapi::_di_IOTARepositoryWizard80()
	{
		Toolsapi::_di_IOTARepositoryWizard80 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard80*(void) { return (Toolsapi::IOTARepositoryWizard80*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {08FCCD88-3A21-4281-ADC9-62FC034CDD12}
	operator Toolsapi::_di_IOTARepositoryWizard60()
	{
		Toolsapi::_di_IOTARepositoryWizard60 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard60*(void) { return (Toolsapi::IOTARepositoryWizard60*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE1-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTARepositoryWizard()
	{
		Toolsapi::_di_IOTARepositoryWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard*(void) { return (Toolsapi::IOTARepositoryWizard*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9A-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTAModuleCreator()
	{
		Toolsapi::_di_IOTAModuleCreator intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAModuleCreator*(void) { return (Toolsapi::IOTAModuleCreator*)&__IOTAModuleCreator; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9E-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTACreator()
	{
		Toolsapi::_di_IOTACreator intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTACreator*(void) { return (Toolsapi::IOTACreator*)&__IOTAModuleCreator; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {36C8BF35-EFFE-11D1-AB1D-00C04FB16FB3}
	operator Toolsapi::_di_IOTAFormWizard()
	{
		Toolsapi::_di_IOTAFormWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAFormWizard*(void) { return (Toolsapi::IOTAFormWizard*)&__IOTAFormWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE0-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTAWizard()
	{
		Toolsapi::_di_IOTAWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAWizard*(void) { return (Toolsapi::IOTAWizard*)&__IOTARepositoryWizard80; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneFormWizard : public TGLBaseSceneFormWizard
{
	typedef TGLBaseSceneFormWizard inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
public:
	/* TGLBaseSceneFormWizard.CreateAndExecute */ inline __fastcall TGLSimpleSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName) : TGLBaseSceneFormWizard(AUnitIdent, AClassName, AFileName) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLSimpleSceneFormWizard() : TGLBaseSceneFormWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneFormWizard() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneFormWizard : public TGLBaseSceneFormWizard
{
	typedef TGLBaseSceneFormWizard inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
public:
	/* TGLBaseSceneFormWizard.CreateAndExecute */ inline __fastcall TGLExtendedSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName) : TGLBaseSceneFormWizard(AUnitIdent, AClassName, AFileName) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLExtendedSceneFormWizard() : TGLBaseSceneFormWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneFormWizard() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneProjectCreator : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	System::UnicodeString FUnitIdent;
	System::UnicodeString FClassName;
	System::UnicodeString FFileName;
	
public:
	__fastcall TGLBaseSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName);
	System::UnicodeString __fastcall GetCreatorType();
	bool __fastcall GetExisting();
	System::UnicodeString __fastcall GetFileSystem();
	Toolsapi::_di_IOTAModule __fastcall GetOwner();
	bool __fastcall GetUnnamed();
	System::UnicodeString __fastcall GetFileName();
	System::UnicodeString __fastcall GetOptionFileName();
	bool __fastcall GetShowSource();
	void __fastcall NewDefaultModule();
	Toolsapi::_di_IOTAFile __fastcall NewOptionSource(const System::UnicodeString ProjectName);
	void __fastcall NewProjectResource(const Toolsapi::_di_IOTAProject Project);
	Toolsapi::_di_IOTAFile __fastcall NewProjectSource(const System::UnicodeString ProjectName);
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneProjectCreator() { }
	
private:
	void *__IOTAProjectCreator50;	// Toolsapi::IOTAProjectCreator50 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {64312F82-62F3-48E9-BAF6-B03DF450312A}
	operator Toolsapi::_di_IOTAProjectCreator50()
	{
		Toolsapi::_di_IOTAProjectCreator50 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectCreator50*(void) { return (Toolsapi::IOTAProjectCreator50*)&__IOTAProjectCreator50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9D-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTAProjectCreator()
	{
		Toolsapi::_di_IOTAProjectCreator intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectCreator*(void) { return (Toolsapi::IOTAProjectCreator*)&__IOTAProjectCreator50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9E-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTACreator()
	{
		Toolsapi::_di_IOTACreator intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTACreator*(void) { return (Toolsapi::IOTACreator*)&__IOTAProjectCreator50; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneProjectWizard : public Toolsapi::TNotifierObject
{
	typedef Toolsapi::TNotifierObject inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	Toolsapi::TWizardState __fastcall GetState();
	virtual void __fastcall Execute();
	System::UnicodeString __fastcall GetAuthor();
	System::UnicodeString __fastcall GetComment();
	System::UnicodeString __fastcall GetPage();
	unsigned __fastcall GetGlyph();
	System::UnicodeString __fastcall GetDesigner();
	Toolsapi::_di_IOTAGalleryCategory __fastcall GetGalleryCategory();
	System::UnicodeString __fastcall GetPersonality();
public:
	/* TObject.Create */ inline __fastcall TGLBaseSceneProjectWizard() : Toolsapi::TNotifierObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneProjectWizard() { }
	
private:
	void *__IOTAProjectWizard;	// Toolsapi::IOTAProjectWizard 
	void *__IOTARepositoryWizard80;	// Toolsapi::IOTARepositoryWizard80 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {36C8BF36-EFFE-11D1-AB1D-00C04FB16FB3}
	operator Toolsapi::_di_IOTAProjectWizard()
	{
		Toolsapi::_di_IOTAProjectWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectWizard*(void) { return (Toolsapi::IOTAProjectWizard*)&__IOTAProjectWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {D7714D41-BC4A-445E-B695-25A65C2F561E}
	operator Toolsapi::_di_IOTARepositoryWizard80()
	{
		Toolsapi::_di_IOTARepositoryWizard80 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard80*(void) { return (Toolsapi::IOTARepositoryWizard80*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {08FCCD88-3A21-4281-ADC9-62FC034CDD12}
	operator Toolsapi::_di_IOTARepositoryWizard60()
	{
		Toolsapi::_di_IOTARepositoryWizard60 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard60*(void) { return (Toolsapi::IOTARepositoryWizard60*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE1-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTARepositoryWizard()
	{
		Toolsapi::_di_IOTARepositoryWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard*(void) { return (Toolsapi::IOTARepositoryWizard*)&__IOTAProjectWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE0-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTAWizard()
	{
		Toolsapi::_di_IOTAWizard intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAWizard*(void) { return (Toolsapi::IOTAWizard*)&__IOTAProjectWizard; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneProjectWizard : public TGLBaseSceneProjectWizard
{
	typedef TGLBaseSceneProjectWizard inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	virtual void __fastcall Execute();
public:
	/* TObject.Create */ inline __fastcall TGLSimpleSceneProjectWizard() : TGLBaseSceneProjectWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneProjectWizard() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneProjectCreator : public TGLBaseSceneProjectCreator
{
	typedef TGLBaseSceneProjectCreator inherited;
	
public:
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TGLBaseSceneProjectCreator.Create */ inline __fastcall TGLSimpleSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName) : TGLBaseSceneProjectCreator(AClassName, AUnitIdent, AFileName) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneProjectCreator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneProjectWizard : public TGLBaseSceneProjectWizard
{
	typedef TGLBaseSceneProjectWizard inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString();
	virtual System::UnicodeString __fastcall GetName();
	virtual void __fastcall Execute();
public:
	/* TObject.Create */ inline __fastcall TGLExtendedSceneProjectWizard() : TGLBaseSceneProjectWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneProjectWizard() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneProjectCreator : public TGLBaseSceneProjectCreator
{
	typedef TGLBaseSceneProjectCreator inherited;
	
public:
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TGLBaseSceneProjectCreator.Create */ inline __fastcall TGLExtendedSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName) : TGLBaseSceneProjectCreator(AClassName, AUnitIdent, AFileName) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneProjectCreator() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _rBaseProjectLocalizedName;
#define Glsceneformdesign_rBaseProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rBaseProjectLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rBaseProjectLocalizedDescription;
#define Glsceneformdesign_rBaseProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rBaseProjectLocalizedDescription)
extern DELPHI_PACKAGE System::ResourceString _rSimpleProjectLocalizedName;
#define Glsceneformdesign_rSimpleProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rSimpleProjectLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rSimpleProjectLocalizedDescription;
#define Glsceneformdesign_rSimpleProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rSimpleProjectLocalizedDescription)
extern DELPHI_PACKAGE System::ResourceString _rExtendedProjectLocalizedName;
#define Glsceneformdesign_rExtendedProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rExtendedProjectLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rExtendedProjectLocalizedDescription;
#define Glsceneformdesign_rExtendedProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rExtendedProjectLocalizedDescription)
extern DELPHI_PACKAGE System::ResourceString _rBaseFormLocalizedName;
#define Glsceneformdesign_rBaseFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rBaseFormLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rBaseFormLocalizedDescription;
#define Glsceneformdesign_rBaseFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rBaseFormLocalizedDescription)
extern DELPHI_PACKAGE System::ResourceString _rSimpleFormLocalizedName;
#define Glsceneformdesign_rSimpleFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rSimpleFormLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rSimpleFormLocalizedDescription;
#define Glsceneformdesign_rSimpleFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rSimpleFormLocalizedDescription)
extern DELPHI_PACKAGE System::ResourceString _rExtendedFormLocalizedName;
#define Glsceneformdesign_rExtendedFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rExtendedFormLocalizedName)
extern DELPHI_PACKAGE System::ResourceString _rExtendedFormLocalizedDescription;
#define Glsceneformdesign_rExtendedFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rExtendedFormLocalizedDescription)
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Glsceneformdesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENEFORMDESIGN)
using namespace Glsceneformdesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLSceneFormDesignHPP
