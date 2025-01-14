// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSkydome.pas' rev: 36.00 (Windows)

#ifndef GlskydomeHPP
#define GlskydomeHPP

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
#include <System.UITypes.hpp>
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <OpenGLAdapter.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLContext.hpp>
#include <GLState.hpp>
#include <GLVectorGeometry.hpp>
#include <GLGraphics.hpp>
#include <GLVectorTypes.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glskydome
{
//-- forward type declarations -----------------------------------------------
struct TGLStarRecord;
class DELPHICLASS TGLSkyDomeBand;
class DELPHICLASS TGLSkyDomeBands;
class DELPHICLASS TGLSkyDomeStar;
class DELPHICLASS TGLSkyDomeStars;
class DELPHICLASS TGLSkyDome;
class DELPHICLASS TGLEarthSkyDome;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLStarRecord
{
public:
	System::Word RA;
	short DEC;
	System::Byte BVColorIndex;
	System::Byte VMagnitude;
};
#pragma pack(pop)


typedef TGLStarRecord *PGLStarRecord;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSkyDomeBand : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FStartAngle;
	float FStopAngle;
	Glcolor::TGLColor* FStartColor;
	Glcolor::TGLColor* FStopColor;
	int FSlices;
	int FStacks;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetStartAngle(const float val);
	void __fastcall SetStartColor(Glcolor::TGLColor* const val);
	void __fastcall SetStopAngle(const float val);
	void __fastcall SetStopColor(Glcolor::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChange(System::TObject* sender);
	
public:
	__fastcall virtual TGLSkyDomeBand(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLSkyDomeBand();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property Glcolor::TGLColor* StartColor = {read=FStartColor, write=SetStartColor};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle};
	__property Glcolor::TGLColor* StopColor = {read=FStopColor, write=SetStopColor};
	__property int Slices = {read=FSlices, write=SetSlices, default=12};
	__property int Stacks = {read=FStacks, write=SetStacks, default=1};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSkyDomeBands : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSkyDomeBand* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSkyDomeBand* const val);
	TGLSkyDomeBand* __fastcall GetItems(int index);
	
public:
	__fastcall TGLSkyDomeBands(System::Classes::TComponent* AOwner);
	HIDESBASE TGLSkyDomeBand* __fastcall Add();
	HIDESBASE TGLSkyDomeBand* __fastcall FindItemID(int ID);
	__property TGLSkyDomeBand* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange();
	void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSkyDomeBands() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSkyDomeStar : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FRA;
	float FDec;
	float FMagnitude;
	System::Uitypes::TColor FColor;
	Glvectorgeometry::TAffineVector FCacheCoord;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLSkyDomeStar(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLSkyDomeStar();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RA = {read=FRA, write=FRA};
	__property float Dec = {read=FDec, write=FDec};
	__property float Magnitude = {read=FMagnitude, write=FMagnitude};
	__property System::Uitypes::TColor Color = {read=FColor, write=FColor, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSkyDomeStars : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSkyDomeStar* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSkyDomeStar* const val);
	TGLSkyDomeStar* __fastcall GetItems(int index);
	void __fastcall PrecomputeCartesianCoordinates();
	
public:
	__fastcall TGLSkyDomeStars(System::Classes::TComponent* AOwner);
	HIDESBASE TGLSkyDomeStar* __fastcall Add();
	HIDESBASE TGLSkyDomeStar* __fastcall FindItemID(int ID);
	__property TGLSkyDomeStar* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci, bool twinkle);
	void __fastcall AddRandomStars(const int nb, const System::Uitypes::TColor color, const bool limitToTopDome = false)/* overload */;
	void __fastcall AddRandomStars(const int nb, const Glvectortypes::TVector3b ColorMin, const Glvectortypes::TVector3b ColorMax, const float Magnitude_min, const float Magnitude_max, const bool limitToTopDome = false)/* overload */;
	void __fastcall LoadStarsFile(const System::UnicodeString starsFileName);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSkyDomeStars() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLSkyDomeOption : unsigned char { sdoTwinkle };

typedef System::Set<TGLSkyDomeOption, TGLSkyDomeOption::sdoTwinkle, TGLSkyDomeOption::sdoTwinkle> TGLSkyDomeOptions;

class PASCALIMPLEMENTATION TGLSkyDome : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	TGLSkyDomeOptions FOptions;
	TGLSkyDomeBands* FBands;
	TGLSkyDomeStars* FStars;
	
protected:
	void __fastcall SetBands(TGLSkyDomeBands* const val);
	void __fastcall SetStars(TGLSkyDomeStars* const val);
	void __fastcall SetOptions(const TGLSkyDomeOptions val);
	
public:
	__fastcall virtual TGLSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyDome();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLSkyDomeBands* Bands = {read=FBands, write=SetBands};
	__property TGLSkyDomeStars* Stars = {read=FStars, write=SetStars};
	__property TGLSkyDomeOptions Options = {read=FOptions, write=SetOptions, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyDome(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TEarthSkydomeOption : unsigned char { esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest };

typedef System::Set<TEarthSkydomeOption, TEarthSkydomeOption::esoFadeStarsWithSun, TEarthSkydomeOption::esoDepthTest> TEarthSkydomeOptions;

class PASCALIMPLEMENTATION TGLEarthSkyDome : public TGLSkyDome
{
	typedef TGLSkyDome inherited;
	
private:
	float FSunElevation;
	float FTurbidity;
	Glcolor::TColorVector FCurSunColor;
	Glcolor::TColorVector FCurSkyColor;
	Glcolor::TColorVector FCurHazeColor;
	float FCurHazeTurbid;
	float FCurSunSkyTurbid;
	Glcolor::TGLColor* FSunZenithColor;
	Glcolor::TGLColor* FSunDawnColor;
	Glcolor::TGLColor* FHazeColor;
	Glcolor::TGLColor* FSkyColor;
	Glcolor::TGLColor* FNightColor;
	Glcolor::TGLColor* FDeepColor;
	int FSlices;
	int FStacks;
	TEarthSkydomeOptions FExtendedOptions;
	bool FMorning;
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall SetSunElevation(const float val);
	void __fastcall SetTurbidity(const float val);
	void __fastcall SetSunZenithColor(Glcolor::TGLColor* const val);
	void __fastcall SetSunDawnColor(Glcolor::TGLColor* const val);
	void __fastcall SetHazeColor(Glcolor::TGLColor* const val);
	void __fastcall SetSkyColor(Glcolor::TGLColor* const val);
	void __fastcall SetNightColor(Glcolor::TGLColor* const val);
	void __fastcall SetDeepColor(Glcolor::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChanged(System::TObject* Sender);
	void __fastcall PreCalculate();
	void __fastcall RenderDome();
	Glcolor::TColorVector __fastcall CalculateColor(const float theta, const float cosGamma);
	
public:
	__fastcall virtual TGLEarthSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLEarthSkyDome();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall SetSunAtTime(float HH, float MM);
	
__published:
	__property float SunElevation = {read=FSunElevation, write=SetSunElevation};
	__property float Turbidity = {read=FTurbidity, write=SetTurbidity};
	__property Glcolor::TGLColor* SunZenithColor = {read=FSunZenithColor, write=SetSunZenithColor};
	__property Glcolor::TGLColor* SunDawnColor = {read=FSunDawnColor, write=SetSunDawnColor};
	__property Glcolor::TGLColor* HazeColor = {read=FHazeColor, write=SetHazeColor};
	__property Glcolor::TGLColor* SkyColor = {read=FSkyColor, write=SetSkyColor};
	__property Glcolor::TGLColor* NightColor = {read=FNightColor, write=SetNightColor};
	__property Glcolor::TGLColor* DeepColor = {read=FDeepColor, write=SetDeepColor};
	__property TEarthSkydomeOptions ExtendedOptions = {read=FExtendedOptions, write=FExtendedOptions, nodefault};
	__property int Slices = {read=FSlices, write=SetSlices, default=24};
	__property int Stacks = {read=FStacks, write=SetStacks, default=48};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLEarthSkyDome(Glscene::TGLBaseSceneObject* aParentOwner) : TGLSkyDome(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Glvectorgeometry::TAffineVector __fastcall StarRecordPositionYUp(const TGLStarRecord &starRecord);
extern DELPHI_PACKAGE Glvectorgeometry::TAffineVector __fastcall StarRecordPositionZUp(const TGLStarRecord &starRecord);
extern DELPHI_PACKAGE Glvectorgeometry::TVector __fastcall StarRecordColor(const TGLStarRecord &starRecord, float bias);
}	/* namespace Glskydome */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSKYDOME)
using namespace Glskydome;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlskydomeHPP
