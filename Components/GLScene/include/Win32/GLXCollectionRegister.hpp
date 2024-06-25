// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLXCollectionRegister.pas' rev: 36.00 (Windows)

#ifndef GlxcollectionregisterHPP
#define GlxcollectionregisterHPP

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
#include <GLXCollection.hpp>
#include <DesignEditors.hpp>
#include <DesignIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glxcollectionregister
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TXCollectionProperty;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TXCollectionProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall Edit();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TXCollectionProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TXCollectionProperty() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Glxcollectionregister */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLXCOLLECTIONREGISTER)
using namespace Glxcollectionregister;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlxcollectionregisterHPP
