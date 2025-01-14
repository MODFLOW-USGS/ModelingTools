// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCrossXML.pas' rev: 36.00 (Windows)

#ifndef GlscrossxmlHPP
#define GlscrossxmlHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Xml.XMLIntf.hpp>
#include <Xml.XMLDoc.hpp>
#include <Xml.xmldom.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glscrossxml
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef Xml::Xmlintf::_di_IXMLDocument GLSXMLDocument;

typedef Xml::Xmlintf::_di_IXMLNode GLSXMLNode;

typedef Xml::Xmldom::_di_IDOMNode GLSDOMNode;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE GLSXMLDocument __fastcall GLSNewXMLDocument(void);
extern DELPHI_PACKAGE void __fastcall ReleaseXMLDocument(GLSXMLDocument &ADoc);
extern DELPHI_PACKAGE void __fastcall WriteXMLFile(GLSXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern DELPHI_PACKAGE void __fastcall ReadXMLFile(GLSXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern DELPHI_PACKAGE void __fastcall WriteXMLFile(GLSXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern DELPHI_PACKAGE void __fastcall ReadXMLFile(GLSXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern DELPHI_PACKAGE bool __fastcall GetXMLAttribute(const GLSXMLNode XMLNode, const System::UnicodeString AttrName, /* out */ System::UnicodeString &Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetXMLAttribute(const GLSXMLNode XMLNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetXMLAttribute(const GLSDOMNode DOMNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern DELPHI_PACKAGE bool __fastcall FindXMLNode(const GLSXMLNode ParentNode, const System::UnicodeString NodeName, /* out */ GLSXMLNode &ChildNode);
extern DELPHI_PACKAGE GLSDOMNode __fastcall CreateDOMNode(const GLSDOMNode ParentNode, const System::UnicodeString NodeName);
extern DELPHI_PACKAGE void __fastcall SetXMLText(const GLSDOMNode DOMNode, const System::UnicodeString AText);
extern DELPHI_PACKAGE bool __fastcall GetXMLText(const GLSXMLNode XMLNode, /* out */ System::UnicodeString &AText);
extern DELPHI_PACKAGE int __fastcall GetXMLAttributeCount(const GLSXMLNode XMLNode);
extern DELPHI_PACKAGE Xml::Xmlintf::_di_IXMLNode __fastcall GetXMLAttribute(const GLSXMLNode XMLNode, int Idx)/* overload */;
}	/* namespace Glscrossxml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCROSSXML)
using namespace Glscrossxml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscrossxmlHPP
