// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLThorFX.pas' rev: 36.00 (Windows)

#ifndef GLThorFXHPP
#define GLThorFXHPP

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
#include <System.Types.hpp>
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLXCollection.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>
#include <GLCadencer.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLManager.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glthorfx
{
//-- forward type declarations -----------------------------------------------
struct TThorpoint;
class DELPHICLASS TGLThorFXManager;
class DELPHICLASS TGLBThorFX;
//-- type declarations -------------------------------------------------------
typedef TThorpoint *PThorpoint;

struct DECLSPEC_DRECORD TThorpoint
{
public:
	Glvectorgeometry::TVector Position;
	float Size;
};


typedef System::StaticArray<TThorpoint, 33554432> TThorpointArray;

typedef TThorpointArray *PThorpointArray;

typedef void __fastcall (__closure *TCalcPointEvent)(System::TObject* Sender, int PointNo, float &x, float &y, float &z);

class PASCALIMPLEMENTATION TGLThorFXManager : public Glbaseclasses::TGLCadenceAbleComponent
{
	typedef Glbaseclasses::TGLCadenceAbleComponent inherited;
	
private:
	System::Classes::TList* FClients;
	PThorpointArray FThorpoints;
	Glcoordinates::TGLCoordinates* FTarget;
	Glcadencer::TGLCadencer* FCadencer;
	int FMaxpoints;
	float FGlowSize;
	float FVibrate;
	float FWildness;
	int NP;
	Glcolor::TGLColor* FInnerColor;
	Glcolor::TGLColor* FOuterColor;
	Glcolor::TGLColor* FCoreColor;
	bool FDisabled;
	bool FCore;
	bool FGlow;
	TCalcPointEvent FOnCalcPoint;
	
protected:
	void __fastcall RegisterClient(TGLBThorFX* aClient);
	void __fastcall DeRegisterClient(TGLBThorFX* aClient);
	void __fastcall DeRegisterAllClients();
	void __fastcall SetTarget(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const val);
	void __fastcall SetMaxpoints(const int val);
	bool __fastcall StoreGlowSize();
	bool __fastcall StoreVibrate();
	void __fastcall SetInnerColor(Glcolor::TGLColor* const val);
	void __fastcall SetOuterColor(Glcolor::TGLColor* const val);
	void __fastcall SetCoreColor(Glcolor::TGLColor* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ThorInit();
	void __fastcall CalcThor();
	void __fastcall CalcFrac(int left, int right, float lh, float rh, int xyz);
	
public:
	__fastcall virtual TGLThorFXManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLThorFXManager();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates* Target = {read=FTarget, write=SetTarget};
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property int Maxpoints = {read=FMaxpoints, write=SetMaxpoints, default=256};
	__property float GlowSize = {read=FGlowSize, write=FGlowSize, stored=StoreGlowSize};
	__property float Vibrate = {read=FVibrate, write=FVibrate, stored=StoreVibrate};
	__property Glcolor::TGLColor* InnerColor = {read=FInnerColor, write=SetInnerColor};
	__property Glcolor::TGLColor* OuterColor = {read=FOuterColor, write=SetOuterColor};
	__property Glcolor::TGLColor* CoreColor = {read=FCoreColor, write=SetCoreColor};
	__property bool Disabled = {read=FDisabled, write=FDisabled, nodefault};
	__property bool Core = {read=FCore, write=FCore, nodefault};
	__property bool Glow = {read=FGlow, write=FGlow, nodefault};
	__property float Wildness = {read=FWildness, write=FWildness};
	__property TCalcPointEvent OnCalcPoint = {read=FOnCalcPoint, write=FOnCalcPoint};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBThorFX : public Glscene::TGLObjectPostEffect
{
	typedef Glscene::TGLObjectPostEffect inherited;
	
private:
	TGLThorFXManager* FManager;
	System::UnicodeString FManagerName;
	Glcoordinates::TGLCoordinates* FTarget;
	
protected:
	void __fastcall SetManager(TGLThorFXManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall SetTarget(Glcoordinates::TGLCoordinates* const val);
	
public:
	__fastcall virtual TGLBThorFX(Glxcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLBThorFX();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Render(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLThorFXManager* Manager = {read=FManager, write=SetManager};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBThorFX* __fastcall GetOrCreateThorFX(Glscene::TGLBaseSceneObject* obj, const System::UnicodeString name = System::UnicodeString());
}	/* namespace Glthorfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTHORFX)
using namespace Glthorfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLThorFXHPP
