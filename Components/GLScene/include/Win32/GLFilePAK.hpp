// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFilePAK.pas' rev: 35.00 (Windows)

#ifndef GlfilepakHPP
#define GlfilepakHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLSArchiveManager.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilepak
{
//-- forward type declarations -----------------------------------------------
struct TPakHeader;
struct TFileSection;
class DELPHICLASS TPAKArchive;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TPakHeader
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
};


class PASCALIMPLEMENTATION TPAKArchive : public Glsarchivemanager::TGLBaseArchive
{
	typedef Glsarchivemanager::TGLBaseArchive inherited;
	
private:
	TPakHeader FHeader;
	System::Classes::TFileStream* FStream;
	int __fastcall GetContentCount();
	void __fastcall MakeContentList();
	
protected:
	virtual void __fastcall SetCompressionLevel(Glsarchivemanager::TCompressionLevel aValue);
	
public:
	__property int ContentCount = {read=GetContentCount, nodefault};
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName);
	virtual void __fastcall Clear();
	virtual bool __fastcall ContentExists(System::UnicodeString ContentName);
	virtual System::Classes::TStream* __fastcall GetContent(System::Classes::TStream* Stream, int index)/* overload */;
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
	/* TGLBaseArchive.Create */ inline __fastcall virtual TPAKArchive(System::Classes::TPersistent* AOwner) : Glsarchivemanager::TGLBaseArchive(AOwner) { }
	/* TGLBaseArchive.Destroy */ inline __fastcall virtual ~TPAKArchive() { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define SIGN L"PACK"
}	/* namespace Glfilepak */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEPAK)
using namespace Glfilepak;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilepakHPP
