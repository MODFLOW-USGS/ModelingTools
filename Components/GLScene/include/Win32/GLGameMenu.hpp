// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGameMenu.pas' rev: 35.00 (Windows)

#ifndef GlgamemenuHPP
#define GlgamemenuHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <OpenGL1x.hpp>
#include <GLVectorTypes.hpp>
#include <GLScene.hpp>
#include <GLCoordinates.hpp>
#include <GLMaterial.hpp>
#include <GLBitmapFont.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLCanvas.hpp>
#include <GLContext.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgamemenu
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGameMenu;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLGameMenuScale : unsigned char { gmsNormal, gms1024x768 };

class PASCALIMPLEMENTATION TGLGameMenu : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	System::Classes::TStrings* FItems;
	int FSelected;
	Glbitmapfont::TGLCustomBitmapFont* FFont;
	int FMarginVert;
	int FMarginHorz;
	int FSpacing;
	TGLGameMenuScale FMenuScale;
	Glcolor::TGLColor* FBackColor;
	Glcolor::TGLColor* FInactiveColor;
	Glcolor::TGLColor* FActiveColor;
	Glcolor::TGLColor* FDisabledColor;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FTitleMaterialName;
	int FTitleWidth;
	int FTitleHeight;
	System::Classes::TNotifyEvent FOnSelectedChanged;
	int FBoxTop;
	int FBoxBottom;
	int FBoxLeft;
	int FBoxRight;
	int FMenuTop;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	void __fastcall SetMenuScale(TGLGameMenuScale AValue);
	void __fastcall SetMarginHorz(int AValue);
	void __fastcall SetMarginVert(int AValue);
	void __fastcall SetSpacing(int AValue);
	void __fastcall SetFont(Glbitmapfont::TGLCustomBitmapFont* AValue);
	void __fastcall SetBackColor(Glcolor::TGLColor* AValue);
	void __fastcall SetInactiveColor(Glcolor::TGLColor* AValue);
	void __fastcall SetActiveColor(Glcolor::TGLColor* AValue);
	void __fastcall SetDisabledColor(Glcolor::TGLColor* AValue);
	bool __fastcall GetEnabled(int AIndex);
	void __fastcall SetEnabled(int AIndex, bool AValue);
	void __fastcall SetItems(System::Classes::TStrings* AValue);
	void __fastcall SetSelected(int AValue);
	System::UnicodeString __fastcall GetSelectedText();
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* AValue);
	void __fastcall SetTitleMaterialName(const System::UnicodeString AValue);
	void __fastcall SetTitleWidth(int AValue);
	void __fastcall SetTitleHeight(int AValue);
	void __fastcall ItemsChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLGameMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGameMenu();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool Enabled[int AIndex] = {read=GetEnabled, write=SetEnabled};
	__property System::UnicodeString SelectedText = {read=GetSelectedText};
	void __fastcall SelectNext();
	void __fastcall SelectPrev();
	void __fastcall MouseMenuSelect(const int X, const int Y);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property TGLGameMenuScale MenuScale = {read=FMenuScale, write=SetMenuScale, default=0};
	__property int MarginHorz = {read=FMarginHorz, write=SetMarginHorz, default=16};
	__property int MarginVert = {read=FMarginVert, write=SetMarginVert, default=16};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=16};
	__property Glbitmapfont::TGLCustomBitmapFont* Font = {read=FFont, write=SetFont};
	__property System::UnicodeString TitleMaterialName = {read=FTitleMaterialName, write=SetTitleMaterialName};
	__property int TitleWidth = {read=FTitleWidth, write=SetTitleWidth, default=0};
	__property int TitleHeight = {read=FTitleHeight, write=SetTitleHeight, default=0};
	__property Glcolor::TGLColor* BackColor = {read=FBackColor, write=SetBackColor};
	__property Glcolor::TGLColor* InactiveColor = {read=FInactiveColor, write=SetInactiveColor};
	__property Glcolor::TGLColor* ActiveColor = {read=FActiveColor, write=SetActiveColor};
	__property Glcolor::TGLColor* DisabledColor = {read=FDisabledColor, write=SetDisabledColor};
	__property System::Classes::TStrings* Items = {read=FItems, write=SetItems};
	__property int Selected = {read=FSelected, write=SetSelected, default=-1};
	__property System::Classes::TNotifyEvent OnSelectedChanged = {read=FOnSelectedChanged, write=FOnSelectedChanged};
	__property int BoxTop = {read=FBoxTop, nodefault};
	__property int BoxBottom = {read=FBoxBottom, nodefault};
	__property int BoxLeft = {read=FBoxLeft, nodefault};
	__property int BoxRight = {read=FBoxRight, nodefault};
	__property int MenuTop = {read=FMenuTop, nodefault};
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGameMenu(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Glmaterial::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glgamemenu */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGAMEMENU)
using namespace Glgamemenu;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgamemenuHPP
