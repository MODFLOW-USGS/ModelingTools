// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLWindowsFont.pas' rev: 36.00 (Windows)

#ifndef GLWindowsFontHPP
#define GLWindowsFontHPP

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
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <GLScene.hpp>
#include <GLTexture.hpp>
#include <GLBitmapFont.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorLists.hpp>
#include <GLUtils.hpp>
#include <GLVectorGeometry.hpp>
#include <OpenGLTokens.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorTypes.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glwindowsfont
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLWindowsBitmapFont;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLWindowsBitmapFont : public Glbitmapfont::TGLCustomBitmapFont
{
	typedef Glbitmapfont::TGLCustomBitmapFont inherited;
	
private:
	Vcl::Graphics::TFont* FFont;
	void __fastcall SetList(Glvectorlists::TIntegerList* const AList);
	
protected:
	void __fastcall SetFont(Vcl::Graphics::TFont* value);
	virtual void __fastcall LoadWindowsFont();
	bool __fastcall StoreRanges();
	virtual void __fastcall PrepareImage(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	virtual int __fastcall TextureFormat();
	void __fastcall StreamlineRanges();
	
public:
	__fastcall virtual TGLWindowsBitmapFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLWindowsBitmapFont();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	int __fastcall FontTextureWidth();
	int __fastcall FontTextureHeight();
	void __fastcall EnsureString(const System::UnicodeString s)/* overload */;
	void __fastcall EnsureChars(const System::WideChar AStart, const System::WideChar AEnd);
	__property Glyphs;
	
__published:
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property HSpace = {default=1};
	__property VSpace = {default=1};
	__property MagFilter = {default=1};
	__property MinFilter = {default=1};
	__property Ranges = {stored=StoreRanges};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glwindowsfont */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWINDOWSFONT)
using namespace Glwindowsfont;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLWindowsFontHPP
