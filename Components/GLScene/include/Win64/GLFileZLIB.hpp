// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileZLIB.pas' rev: 34.00 (Windows)

#ifndef GlfilezlibHPP
#define GlfilezlibHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.ZLib.hpp>
#include <GLSArchiveManager.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilezlib
{
//-- forward type declarations -----------------------------------------------
struct TZLibHeader;
struct TFileSection;
class DELPHICLASS TZLibArchive;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TZLibHeader
{
public:
	System::StaticArray<char, 4> Signature;
	int DirOffset;
	int DirLength;
};


struct DECLSPEC_DRECORD TFileSection
{
public:
	System::StaticArray<char, 120> FileName;
	int FilePos;
	int FileLength;
	Glsarchivemanager::TCompressionLevel CbrMode;
};


class PASCALIMPLEMENTATION TZLibArchive : public Glsarchivemanager::TGLBaseArchive
{
	typedef Glsarchivemanager::TGLBaseArchive inherited;
	
private:
	TZLibHeader FHeader;
	System::Classes::TFileStream* FStream;
	int __fastcall GetContentCount();
	void __fastcall MakeContentList();
	
public:
	__property int ContentCount = {read=GetContentCount, nodefault};
	__fastcall virtual ~TZLibArchive();
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName);
	virtual void __fastcall Clear();
	virtual bool __fastcall ContentExists(System::UnicodeString ContentName);
	virtual System::Classes::TStream* __fastcall GetContent(System::Classes::TStream* aStream, int index)/* overload */;
	virtual System::Classes::TStream* __fastcall GetContent(int index)/* overload */;
	virtual System::Classes::TStream* __fastcall GetContent(System::UnicodeString ContentName)/* overload */;
	virtual int __fastcall GetContentSize(int index)/* overload */;
	virtual int __fastcall GetContentSize(System::UnicodeString ContentName)/* overload */;
	virtual void __fastcall AddFromStream(System::UnicodeString ContentName, System::UnicodeString Path, System::Classes::TStream* FS);
	virtual void __fastcall AddFromFile(System::UnicodeString FileName, System::UnicodeString Path);
	virtual void __fastcall RemoveContent(int index)/* overload */;
	virtual void __fastcall RemoveContent(System::UnicodeString ContentName)/* overload */;
	virtual void __fastcall Extract(int index, System::UnicodeString NewName)/* overload */;
	virtual void __fastcall Extract(System::UnicodeString ContentName, System::UnicodeString NewName)/* overload */;
public:
	/* TGLBaseArchive.Create */ inline __fastcall virtual TZLibArchive(System::Classes::TPersistent* AOwner) : Glsarchivemanager::TGLBaseArchive(AOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define SIGN L"ZLIB"
}	/* namespace Glfilezlib */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEZLIB)
using namespace Glfilezlib;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilezlibHPP
