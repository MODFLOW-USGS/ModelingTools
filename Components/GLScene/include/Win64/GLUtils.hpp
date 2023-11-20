// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLUtils.pas' rev: 35.00 (Windows)

#ifndef GlutilsHPP
#define GlutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <GLStrings.hpp>
#include <GLVectorGeometry.hpp>
#include <GLCrossPlatform.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLUtilsException;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLUtilsException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLUtilsException() { }
	
};


typedef System::StaticArray<System::Byte, 256> TSqrt255Array;

typedef TSqrt255Array *PSqrt255Array;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall WordToIntegerArray(System::Sysutils::PWordArray Source, Glvectorgeometry::PIntegerVector Dest, unsigned Count);
extern DELPHI_PACKAGE int __fastcall RoundUpToPowerOf2(int value);
extern DELPHI_PACKAGE int __fastcall RoundDownToPowerOf2(int value);
extern DELPHI_PACKAGE bool __fastcall IsPowerOf2(int value);
extern DELPHI_PACKAGE System::AnsiString __fastcall ReadCRLFString(System::Classes::TStream* aStream);
extern DELPHI_PACKAGE void __fastcall WriteCRLFString(System::Classes::TStream* aStream, const System::AnsiString aString);
extern DELPHI_PACKAGE bool __fastcall TryStrToFloat(const System::UnicodeString strValue, System::Extended &val);
extern DELPHI_PACKAGE System::Extended __fastcall StrToFloatDef(const System::UnicodeString strValue, System::Extended defValue = 0.000000E+00);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvancedSafe(const System::UnicodeString Str, const System::Uitypes::TColor Default);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvanced(const System::UnicodeString Str);
extern DELPHI_PACKAGE bool __fastcall TryStringToColorAdvanced(const System::UnicodeString Str, System::Uitypes::TColor &OutColor);
extern DELPHI_PACKAGE int __fastcall ParseInteger(System::WideChar * &p);
extern DELPHI_PACKAGE System::Extended __fastcall ParseFloat(System::WideChar * &p);
extern DELPHI_PACKAGE void __fastcall SaveAnsiStringToFile(const System::UnicodeString fileName, const System::AnsiString data);
extern DELPHI_PACKAGE System::AnsiString __fastcall LoadAnsiStringFromFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE void __fastcall SaveStringToFile(const System::UnicodeString fileName, const System::UnicodeString data);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LoadStringFromFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE void __fastcall SaveComponentToFile(System::Classes::TComponent* const Component, const System::UnicodeString FileName, const bool AsText = true);
extern DELPHI_PACKAGE void __fastcall LoadComponentFromFile(System::Classes::TComponent* const Component, const System::UnicodeString FileName, const bool AsText = true);
extern DELPHI_PACKAGE __int64 __fastcall SizeOfFile(const System::UnicodeString fileName);
extern DELPHI_PACKAGE PSqrt255Array __fastcall GetSqrt255Array(void);
extern DELPHI_PACKAGE void __fastcall InformationDlg(const System::UnicodeString msg);
extern DELPHI_PACKAGE bool __fastcall QuestionDlg(const System::UnicodeString msg);
extern DELPHI_PACKAGE System::UnicodeString __fastcall InputDlg(const System::UnicodeString aCaption, const System::UnicodeString aPrompt, const System::UnicodeString aDefault);
extern DELPHI_PACKAGE bool __fastcall SavePictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
extern DELPHI_PACKAGE bool __fastcall OpenPictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
extern DELPHI_PACKAGE void __fastcall SetGLSceneMediaDir(void);
}	/* namespace Glutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLUTILS)
using namespace Glutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlutilsHPP
