// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLColor.pas' rev: 36.00 (Windows)

#ifndef GlcolorHPP
#define GlcolorHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Graphics.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLPersistentClasses.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcolor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLColor;
struct TColorEntry;
class DELPHICLASS TGLColorManager;
//-- type declarations -------------------------------------------------------
typedef Glvectortypes::TVector4f *PColorVector;

typedef Glvectortypes::TVector4f TColorVector;

typedef Glvectortypes::TVector3b *PRGBColor;

typedef Glvectortypes::TVector3b TRGBColor;

class PASCALIMPLEMENTATION TGLColor : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
private:
	TColorVector FColor;
	PColorVector FPDefaultColor;
	void __fastcall SetColorVector(const TColorVector &aColor)/* overload */;
	void __fastcall SetColorComponent(int index, float value);
	float __fastcall GetColorComponent(const int index);
	void __fastcall SetAsWinColor(const System::Uitypes::TColor val);
	System::Uitypes::TColor __fastcall GetAsWinColor();
	void __fastcall SetDirectColorVector(const TColorVector &aColor);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	Glvectorgeometry::TVector __fastcall GetHSVA();
	void __fastcall SetHSVA(const Glvectorgeometry::TVector &hsva);
	
public:
	__fastcall virtual TGLColor(System::Classes::TPersistent* AOwner);
	__fastcall TGLColor(System::Classes::TPersistent* AOwner, const TColorVector &Color, System::Classes::TNotifyEvent changeEvent);
	__fastcall virtual ~TGLColor();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Initialize(const TColorVector &color);
	System::PSingle __fastcall AsAddress();
	void __fastcall RandomColor();
	void __fastcall SetColor(float Red, float Green, float Blue, float Alpha = 1.000000E+00f)/* overload */;
	__property TColorVector Color = {read=FColor, write=SetColorVector};
	__property TColorVector DirectColor = {read=FColor, write=SetDirectColorVector};
	__property System::Uitypes::TColor AsWinColor = {read=GetAsWinColor, write=SetAsWinColor, nodefault};
	__property Glvectorgeometry::TVector hsva = {read=GetHSVA, write=SetHSVA};
	__property TColorVector DefaultColor = {read=FColor};
	
__published:
	__property float Red = {read=GetColorComponent, write=SetColorComponent, stored=false, index=0};
	__property float Green = {read=GetColorComponent, write=SetColorComponent, stored=false, index=1};
	__property float Blue = {read=GetColorComponent, write=SetColorComponent, stored=false, index=2};
	__property float Alpha = {read=GetColorComponent, write=SetColorComponent, stored=false, index=3};
};


typedef TColorEntry *PColorEntry;

struct DECLSPEC_DRECORD TColorEntry
{
public:
	System::UnicodeString Name;
	TColorVector color;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLColorManager : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TGLColorManager();
	void __fastcall AddColor(const System::UnicodeString aName, const TColorVector &aColor);
	void __fastcall EnumColors(System::Classes::TGetStrProc Proc)/* overload */;
	void __fastcall EnumColors(System::Classes::TStrings* AValues)/* overload */;
	TColorVector __fastcall FindColor(const System::UnicodeString aName);
	TColorVector __fastcall GetColor(const System::UnicodeString aName);
	System::UnicodeString __fastcall GetColorName(const TColorVector &aColor);
	void __fastcall RegisterDefaultColors();
	void __fastcall RemoveColor(const System::UnicodeString aName);
public:
	/* TObject.Create */ inline __fastcall TGLColorManager() : System::Classes::TList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Uitypes::TColor clForeground = System::Uitypes::TColor(-1);
static _DELPHI_CONST System::Uitypes::TColor clButton = System::Uitypes::TColor(-2);
static _DELPHI_CONST System::Uitypes::TColor clLight = System::Uitypes::TColor(-3);
static _DELPHI_CONST System::Uitypes::TColor clMidlight = System::Uitypes::TColor(-4);
static _DELPHI_CONST System::Uitypes::TColor clDark = System::Uitypes::TColor(-5);
static _DELPHI_CONST System::Uitypes::TColor clMid = System::Uitypes::TColor(-6);
static _DELPHI_CONST System::Uitypes::TColor clText = System::Uitypes::TColor(-7);
static _DELPHI_CONST System::Uitypes::TColor clBrightText = System::Uitypes::TColor(-8);
static _DELPHI_CONST System::Uitypes::TColor clButtonText = System::Uitypes::TColor(-9);
static _DELPHI_CONST System::Uitypes::TColor clBase = System::Uitypes::TColor(-10);
static _DELPHI_CONST System::Uitypes::TColor clBackground = System::Uitypes::TColor(-11);
static _DELPHI_CONST System::Uitypes::TColor clShadow = System::Uitypes::TColor(-12);
static _DELPHI_CONST System::Uitypes::TColor clHighlight = System::Uitypes::TColor(-13);
static _DELPHI_CONST System::Uitypes::TColor clHighlightedText = System::Uitypes::TColor(-14);
static _DELPHI_CONST System::Int8 cloNormal = System::Int8(0x20);
static _DELPHI_CONST System::Int8 cloDisabled = System::Int8(0x40);
static _DELPHI_CONST System::Int8 cloActive = System::Int8(0x60);
static _DELPHI_CONST System::Uitypes::TColor clNormalForeground = System::Uitypes::TColor(-33);
static _DELPHI_CONST System::Uitypes::TColor clNormalButton = System::Uitypes::TColor(-34);
static _DELPHI_CONST System::Uitypes::TColor clNormalLight = System::Uitypes::TColor(-35);
static _DELPHI_CONST System::Uitypes::TColor clNormalMidlight = System::Uitypes::TColor(-36);
static _DELPHI_CONST System::Uitypes::TColor clNormalDark = System::Uitypes::TColor(-37);
static _DELPHI_CONST System::Uitypes::TColor clNormalMid = System::Uitypes::TColor(-38);
static _DELPHI_CONST System::Uitypes::TColor clNormalText = System::Uitypes::TColor(-39);
static _DELPHI_CONST System::Uitypes::TColor clNormalBrightText = System::Uitypes::TColor(-40);
static _DELPHI_CONST System::Uitypes::TColor clNormalButtonText = System::Uitypes::TColor(-41);
static _DELPHI_CONST System::Uitypes::TColor clNormalBase = System::Uitypes::TColor(-42);
static _DELPHI_CONST System::Uitypes::TColor clNormalBackground = System::Uitypes::TColor(-43);
static _DELPHI_CONST System::Uitypes::TColor clNormalShadow = System::Uitypes::TColor(-44);
static _DELPHI_CONST System::Uitypes::TColor clNormalHighlight = System::Uitypes::TColor(-45);
static _DELPHI_CONST System::Uitypes::TColor clNormalHighlightedText = System::Uitypes::TColor(-46);
static _DELPHI_CONST System::Uitypes::TColor clDisabledForeground = System::Uitypes::TColor(-65);
static _DELPHI_CONST System::Uitypes::TColor clDisabledButton = System::Uitypes::TColor(-66);
static _DELPHI_CONST System::Uitypes::TColor clDisabledLight = System::Uitypes::TColor(-67);
static _DELPHI_CONST System::Uitypes::TColor clDisabledMidlight = System::Uitypes::TColor(-68);
static _DELPHI_CONST System::Uitypes::TColor clDisabledDark = System::Uitypes::TColor(-69);
static _DELPHI_CONST System::Uitypes::TColor clDisabledMid = System::Uitypes::TColor(-70);
static _DELPHI_CONST System::Uitypes::TColor clDisabledText = System::Uitypes::TColor(-71);
static _DELPHI_CONST System::Uitypes::TColor clDisabledBrightText = System::Uitypes::TColor(-72);
static _DELPHI_CONST System::Uitypes::TColor clDisabledButtonText = System::Uitypes::TColor(-73);
static _DELPHI_CONST System::Uitypes::TColor clDisabledBase = System::Uitypes::TColor(-74);
static _DELPHI_CONST System::Uitypes::TColor clDisabledBackground = System::Uitypes::TColor(-75);
static _DELPHI_CONST System::Uitypes::TColor clDisabledShadow = System::Uitypes::TColor(-76);
static _DELPHI_CONST System::Uitypes::TColor clDisabledHighlight = System::Uitypes::TColor(-77);
static _DELPHI_CONST System::Uitypes::TColor clDisabledHighlightedText = System::Uitypes::TColor(-78);
static _DELPHI_CONST System::Uitypes::TColor clActiveForeground = System::Uitypes::TColor(-97);
static _DELPHI_CONST System::Uitypes::TColor clActiveButton = System::Uitypes::TColor(-98);
static _DELPHI_CONST System::Uitypes::TColor clActiveLight = System::Uitypes::TColor(-99);
static _DELPHI_CONST System::Uitypes::TColor clActiveMidlight = System::Uitypes::TColor(-100);
static _DELPHI_CONST System::Uitypes::TColor clActiveDark = System::Uitypes::TColor(-101);
static _DELPHI_CONST System::Uitypes::TColor clActiveMid = System::Uitypes::TColor(-102);
static _DELPHI_CONST System::Uitypes::TColor clActiveText = System::Uitypes::TColor(-103);
static _DELPHI_CONST System::Uitypes::TColor clActiveBrightText = System::Uitypes::TColor(-104);
static _DELPHI_CONST System::Uitypes::TColor clActiveButtonText = System::Uitypes::TColor(-105);
static _DELPHI_CONST System::Uitypes::TColor clActiveBase = System::Uitypes::TColor(-106);
static _DELPHI_CONST System::Uitypes::TColor clActiveBackground = System::Uitypes::TColor(-107);
static _DELPHI_CONST System::Uitypes::TColor clActiveShadow = System::Uitypes::TColor(-108);
static _DELPHI_CONST System::Uitypes::TColor clActiveHighlight = System::Uitypes::TColor(-109);
static _DELPHI_CONST System::Uitypes::TColor clActiveHighlightedText = System::Uitypes::TColor(-110);
static _DELPHI_CONST System::Uitypes::TColor clFirstSpecialColor = System::Uitypes::TColor(-110);
static _DELPHI_CONST int clMask = int(16777215);
static _DELPHI_CONST int clDontMask = int(0);
extern DELPHI_PACKAGE TColorVector clrScrollBar;
extern DELPHI_PACKAGE TColorVector clrBackground;
extern DELPHI_PACKAGE TColorVector clrActiveCaption;
extern DELPHI_PACKAGE TColorVector clrInactiveCaption;
extern DELPHI_PACKAGE TColorVector clrMenu;
extern DELPHI_PACKAGE TColorVector clrWindow;
extern DELPHI_PACKAGE TColorVector clrWindowFrame;
extern DELPHI_PACKAGE TColorVector clrMenuText;
extern DELPHI_PACKAGE TColorVector clrWindowText;
extern DELPHI_PACKAGE TColorVector clrCaptionText;
extern DELPHI_PACKAGE TColorVector clrActiveBorder;
extern DELPHI_PACKAGE TColorVector clrInactiveBorder;
extern DELPHI_PACKAGE TColorVector clrAppWorkSpace;
extern DELPHI_PACKAGE TColorVector clrHighlight;
extern DELPHI_PACKAGE TColorVector clrHighlightText;
extern DELPHI_PACKAGE TColorVector clrBtnFace;
extern DELPHI_PACKAGE TColorVector clrBtnShadow;
extern DELPHI_PACKAGE TColorVector clrGrayText;
extern DELPHI_PACKAGE TColorVector clrBtnText;
extern DELPHI_PACKAGE TColorVector clrInactiveCaptionText;
extern DELPHI_PACKAGE TColorVector clrBtnHighlight;
extern DELPHI_PACKAGE TColorVector clr3DDkShadow;
extern DELPHI_PACKAGE TColorVector clr3DLight;
extern DELPHI_PACKAGE TColorVector clrInfoText;
extern DELPHI_PACKAGE TColorVector clrInfoBk;
extern DELPHI_PACKAGE TColorVector clrTransparent;
extern DELPHI_PACKAGE TColorVector clrBlack;
extern DELPHI_PACKAGE TColorVector clrGray05;
extern DELPHI_PACKAGE TColorVector clrGray10;
extern DELPHI_PACKAGE TColorVector clrGray15;
extern DELPHI_PACKAGE TColorVector clrGray20;
extern DELPHI_PACKAGE TColorVector clrGray25;
extern DELPHI_PACKAGE TColorVector clrGray30;
extern DELPHI_PACKAGE TColorVector clrGray35;
extern DELPHI_PACKAGE TColorVector clrGray40;
extern DELPHI_PACKAGE TColorVector clrGray45;
extern DELPHI_PACKAGE TColorVector clrGray50;
extern DELPHI_PACKAGE TColorVector clrGray55;
extern DELPHI_PACKAGE TColorVector clrGray60;
extern DELPHI_PACKAGE TColorVector clrGray65;
extern DELPHI_PACKAGE TColorVector clrGray70;
extern DELPHI_PACKAGE TColorVector clrGray75;
extern DELPHI_PACKAGE TColorVector clrGray80;
extern DELPHI_PACKAGE TColorVector clrGray85;
extern DELPHI_PACKAGE TColorVector clrGray90;
extern DELPHI_PACKAGE TColorVector clrGray95;
extern DELPHI_PACKAGE TColorVector clrWhite;
extern DELPHI_PACKAGE TColorVector clrDimGray;
extern DELPHI_PACKAGE TColorVector clrGray;
extern DELPHI_PACKAGE TColorVector clrLightGray;
extern DELPHI_PACKAGE TColorVector clrAquamarine;
extern DELPHI_PACKAGE TColorVector clrBlueViolet;
extern DELPHI_PACKAGE TColorVector clrBrown;
extern DELPHI_PACKAGE TColorVector clrCadetBlue;
extern DELPHI_PACKAGE TColorVector clrCoral;
extern DELPHI_PACKAGE TColorVector clrCornflowerBlue;
extern DELPHI_PACKAGE TColorVector clrDarkGreen;
extern DELPHI_PACKAGE TColorVector clrDarkOliveGreen;
extern DELPHI_PACKAGE TColorVector clrDarkOrchid;
extern DELPHI_PACKAGE TColorVector clrDarkSlateBlue;
extern DELPHI_PACKAGE TColorVector clrDarkSlateGray;
extern DELPHI_PACKAGE TColorVector clrDarkSlateGrey;
extern DELPHI_PACKAGE TColorVector clrDarkTurquoise;
extern DELPHI_PACKAGE TColorVector clrFirebrick;
extern DELPHI_PACKAGE TColorVector clrForestGreen;
extern DELPHI_PACKAGE TColorVector clrGold;
extern DELPHI_PACKAGE TColorVector clrGoldenrod;
extern DELPHI_PACKAGE TColorVector clrGreenYellow;
extern DELPHI_PACKAGE TColorVector clrIndian;
extern DELPHI_PACKAGE TColorVector clrKhaki;
extern DELPHI_PACKAGE TColorVector clrLightBlue;
extern DELPHI_PACKAGE TColorVector clrLightSteelBlue;
extern DELPHI_PACKAGE TColorVector clrLimeGreen;
extern DELPHI_PACKAGE TColorVector clrMaroon;
extern DELPHI_PACKAGE TColorVector clrMediumAquamarine;
extern DELPHI_PACKAGE TColorVector clrMediumBlue;
extern DELPHI_PACKAGE TColorVector clrMediumForestGreen;
extern DELPHI_PACKAGE TColorVector clrMediumGoldenrod;
extern DELPHI_PACKAGE TColorVector clrMediumOrchid;
extern DELPHI_PACKAGE TColorVector clrMediumSeaGreen;
extern DELPHI_PACKAGE TColorVector clrMediumSlateBlue;
extern DELPHI_PACKAGE TColorVector clrMediumSpringGreen;
extern DELPHI_PACKAGE TColorVector clrMediumTurquoise;
extern DELPHI_PACKAGE TColorVector clrMediumViolet;
extern DELPHI_PACKAGE TColorVector clrMidnightBlue;
extern DELPHI_PACKAGE TColorVector clrNavy;
extern DELPHI_PACKAGE TColorVector clrNavyBlue;
extern DELPHI_PACKAGE TColorVector clrOrange;
extern DELPHI_PACKAGE TColorVector clrOrangeRed;
extern DELPHI_PACKAGE TColorVector clrOrchid;
extern DELPHI_PACKAGE TColorVector clrPaleGreen;
extern DELPHI_PACKAGE TColorVector clrPink;
extern DELPHI_PACKAGE TColorVector clrPlum;
extern DELPHI_PACKAGE TColorVector clrSalmon;
extern DELPHI_PACKAGE TColorVector clrSeaGreen;
extern DELPHI_PACKAGE TColorVector clrSienna;
extern DELPHI_PACKAGE TColorVector clrSkyBlue;
extern DELPHI_PACKAGE TColorVector clrSlateBlue;
extern DELPHI_PACKAGE TColorVector clrSpringGreen;
extern DELPHI_PACKAGE TColorVector clrSteelBlue;
extern DELPHI_PACKAGE TColorVector clrTan;
extern DELPHI_PACKAGE TColorVector clrThistle;
extern DELPHI_PACKAGE TColorVector clrTurquoise;
extern DELPHI_PACKAGE TColorVector clrViolet;
extern DELPHI_PACKAGE TColorVector clrVioletRed;
extern DELPHI_PACKAGE TColorVector clrWheat;
extern DELPHI_PACKAGE TColorVector clrYellowGreen;
extern DELPHI_PACKAGE TColorVector clrSummerSky;
extern DELPHI_PACKAGE TColorVector clrRichBlue;
extern DELPHI_PACKAGE TColorVector clrBrass;
extern DELPHI_PACKAGE TColorVector clrCopper;
extern DELPHI_PACKAGE TColorVector clrBronze;
extern DELPHI_PACKAGE TColorVector clrBronze2;
extern DELPHI_PACKAGE TColorVector clrSilver;
extern DELPHI_PACKAGE TColorVector clrBrightGold;
extern DELPHI_PACKAGE TColorVector clrOldGold;
extern DELPHI_PACKAGE TColorVector clrFeldspar;
extern DELPHI_PACKAGE TColorVector clrQuartz;
extern DELPHI_PACKAGE TColorVector clrNeonPink;
extern DELPHI_PACKAGE TColorVector clrDarkPurple;
extern DELPHI_PACKAGE TColorVector clrNeonBlue;
extern DELPHI_PACKAGE TColorVector clrCoolCopper;
extern DELPHI_PACKAGE TColorVector clrMandarinOrange;
extern DELPHI_PACKAGE TColorVector clrLightWood;
extern DELPHI_PACKAGE TColorVector clrMediumWood;
extern DELPHI_PACKAGE TColorVector clrDarkWood;
extern DELPHI_PACKAGE TColorVector clrSpicyPink;
extern DELPHI_PACKAGE TColorVector clrSemiSweetChoc;
extern DELPHI_PACKAGE TColorVector clrBakersChoc;
extern DELPHI_PACKAGE TColorVector clrFlesh;
extern DELPHI_PACKAGE TColorVector clrNewTan;
extern DELPHI_PACKAGE TColorVector clrNewMidnightBlue;
extern DELPHI_PACKAGE TColorVector clrVeryDarkBrown;
extern DELPHI_PACKAGE TColorVector clrDarkBrown;
extern DELPHI_PACKAGE TColorVector clrDarkTan;
extern DELPHI_PACKAGE TColorVector clrGreenCopper;
extern DELPHI_PACKAGE TColorVector clrDkGreenCopper;
extern DELPHI_PACKAGE TColorVector clrDustyRose;
extern DELPHI_PACKAGE TColorVector clrHuntersGreen;
extern DELPHI_PACKAGE TColorVector clrScarlet;
extern DELPHI_PACKAGE TColorVector clrMediumPurple;
extern DELPHI_PACKAGE TColorVector clrLightPurple;
extern DELPHI_PACKAGE TColorVector clrVeryLightPurple;
extern DELPHI_PACKAGE TColorVector clrGreen;
extern DELPHI_PACKAGE TColorVector clrOlive;
extern DELPHI_PACKAGE TColorVector clrPurple;
extern DELPHI_PACKAGE TColorVector clrTeal;
extern DELPHI_PACKAGE TColorVector clrRed;
extern DELPHI_PACKAGE TColorVector clrLime;
extern DELPHI_PACKAGE TColorVector clrYellow;
extern DELPHI_PACKAGE TColorVector clrBlue;
extern DELPHI_PACKAGE TColorVector clrFuchsia;
extern DELPHI_PACKAGE TColorVector clrAqua;
#define cDefaultNormalMapScale  (1.250000E-01)
extern DELPHI_PACKAGE bool vUseDefaultColorSets;
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall RGB2Color(const System::Byte r, const System::Byte g, const System::Byte b);
extern DELPHI_PACKAGE TGLColorManager* __fastcall ColorManager(void);
extern DELPHI_PACKAGE TColorVector __fastcall ConvertWinColor(System::Uitypes::TColor aColor, float Alpha = 1.000000E+00f);
extern DELPHI_PACKAGE void __fastcall InitGLSceneColors(void);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const TColorVector &aColor)/* overload */;
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const TColorVector &aColor, float intensity)/* overload */;
extern DELPHI_PACKAGE TColorVector __fastcall ConvertRGBColor(const System::Byte *aColor, const System::NativeInt aColor_High);
extern DELPHI_PACKAGE void __fastcall RegisterColor(const System::UnicodeString aName, const TColorVector &aColor);
extern DELPHI_PACKAGE void __fastcall UnRegisterColor(const System::UnicodeString aName);
}	/* namespace Glcolor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOLOR)
using namespace Glcolor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcolorHPP
