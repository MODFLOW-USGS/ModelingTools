// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileOBJ.pas' rev: 34.00 (Windows)

#ifndef GlfileobjHPP
#define GlfileobjHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorLists.hpp>
#include <GLTexture.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfileobj
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLOBJVectorFile;
class DELPHICLASS EGLOBJFileError;
class DELPHICLASS TGLMTLFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLOBJVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	System::AnsiString FBuffer;
	System::UnicodeString FLine;
	int FLineNo;
	bool FEof;
	int FBufPos;
	
protected:
	void __fastcall ReadLine();
	void __fastcall Error(const System::UnicodeString msg);
	void __fastcall CalcMissingOBJNormals(Glvectorfileobjects::TMeshObject* mesh);
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLOBJVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLOBJVectorFile() { }
	
};


class PASCALIMPLEMENTATION EGLOBJFileError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	int FLineNo;
	
public:
	__property int LineNo = {read=FLineNo, nodefault};
public:
	/* Exception.Create */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLOBJFileError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLOBJFileError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLOBJFileError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLOBJFileError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLOBJFileError() { }
	
};


class PASCALIMPLEMENTATION TGLMTLFile : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	void __fastcall Prepare();
	System::UnicodeString __fastcall MaterialStringProperty(const System::UnicodeString materialName, const System::UnicodeString propertyName);
	Glvectortypes::TVector4f __fastcall MaterialVectorProperty(const System::UnicodeString materialName, const System::UnicodeString propertyName, const Glvectortypes::TVector4f &defaultValue);
public:
	/* TStringList.Create */ inline __fastcall TGLMTLFile()/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TGLMTLFile(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Create */ inline __fastcall TGLMTLFile(System::WideChar QuoteChar, System::WideChar Delimiter)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter) { }
	/* TStringList.Create */ inline __fastcall TGLMTLFile(System::WideChar QuoteChar, System::WideChar Delimiter, System::Classes::TStringsOptions Options)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter, Options) { }
	/* TStringList.Create */ inline __fastcall TGLMTLFile(System::Types::TDuplicates Duplicates, bool Sorted, bool CaseSensitive)/* overload */ : System::Classes::TStringList(Duplicates, Sorted, CaseSensitive) { }
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLMTLFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word BufSize = System::Word(0x2800);
static const System::Int8 LineLen = System::Int8(0x64);
extern DELPHI_PACKAGE bool vGLFileOBJ_SplitMesh;
}	/* namespace Glfileobj */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEOBJ)
using namespace Glfileobj;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileobjHPP
