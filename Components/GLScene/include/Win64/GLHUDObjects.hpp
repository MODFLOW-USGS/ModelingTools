// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHUDObjects.pas' rev: 35.00 (Windows)

#ifndef GlhudobjectsHPP
#define GlhudobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLScene.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLObjects.hpp>
#include <GLBitmapFont.hpp>
#include <GLCrossPlatform.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glhudobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHUDSprite;
class DELPHICLASS TGLHUDText;
class DELPHICLASS TGLAbsoluteHUDText;
class DELPHICLASS TGLResolutionIndependantHUDText;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLHUDSprite : public Globjects::TGLSprite
{
	typedef Globjects::TGLSprite inherited;
	
private:
	int FXTiles;
	int FYTiles;
	bool __fastcall StoreWidth();
	bool __fastcall StoreHeight();
	
protected:
	void __fastcall SetXTiles(const int val);
	void __fastcall SetYTiles(const int val);
	
public:
	__fastcall virtual TGLHUDSprite(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property int XTiles = {read=FXTiles, write=SetXTiles, default=1};
	__property int YTiles = {read=FYTiles, write=SetYTiles, default=1};
	__property Width = {stored=StoreWidth, default=0};
	__property Height = {stored=StoreHeight, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLHUDSprite() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDSprite(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLSprite(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLHUDText : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Glbitmapfont::TGLCustomBitmapFont* FBitmapFont;
	System::UnicodeString FText;
	float FRotation;
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FLayout;
	Glcolor::TGLColor* FModulateColor;
	
protected:
	void __fastcall SetBitmapFont(Glbitmapfont::TGLCustomBitmapFont* const val);
	void __fastcall SetText(const System::UnicodeString val);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlignment(const System::Classes::TAlignment val);
	void __fastcall SetLayout(const Vcl::Stdctrls::TTextLayout val);
	void __fastcall SetModulateColor(Glcolor::TGLColor* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RenderTextAtPosition(const float X, const float Y, const float Z, Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHUDText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHUDText();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property Glbitmapfont::TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=SetBitmapFont};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property Glcolor::TGLColor* ModulateColor = {read=FModulateColor, write=SetModulateColor};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLAbsoluteHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
public:
	/* TGLHUDText.Create */ inline __fastcall virtual TGLAbsoluteHUDText(System::Classes::TComponent* AOwner) : TGLHUDText(AOwner) { }
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLAbsoluteHUDText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAbsoluteHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLResolutionIndependantHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	__fastcall virtual TGLResolutionIndependantHUDText(System::Classes::TComponent* AOwner);
public:
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLResolutionIndependantHUDText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLResolutionIndependantHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glhudobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHUDOBJECTS)
using namespace Glhudobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlhudobjectsHPP
