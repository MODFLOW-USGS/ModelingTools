// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFireFX.pas' rev: 36.00 (Windows)

#ifndef GlfirefxHPP
#define GlfirefxHPP

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
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLXCollection.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>
#include <GLCadencer.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLManager.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfirefx
{
//-- forward type declarations -----------------------------------------------
struct TFireParticle;
class DELPHICLASS TGLFireFXManager;
class DELPHICLASS TGLBFireFX;
//-- type declarations -------------------------------------------------------
typedef TFireParticle *PFireParticle;

struct DECLSPEC_DRECORD TFireParticle
{
public:
	Glvectorgeometry::TVector Position;
	Glvectorgeometry::TVector Speed;
	float Alpha;
	float TimeToLive;
	float LifeLength;
};


typedef System::StaticArray<TFireParticle, 33554432> TFireParticleArray;

typedef TFireParticleArray *PFireParticleArray;

class PASCALIMPLEMENTATION TGLFireFXManager : public Glbaseclasses::TGLCadenceAbleComponent
{
	typedef Glbaseclasses::TGLCadenceAbleComponent inherited;
	
private:
	System::Classes::TList* FClients;
	PFireParticleArray FFireParticles;
	Glcoordinates::TGLCoordinates* FFireDir;
	Glcoordinates::TGLCoordinates* FInitialDir;
	Glcadencer::TGLCadencer* FCadencer;
	int FMaxParticles;
	int FParticleLife;
	float FParticleSize;
	float FFireDensity;
	float FFireEvaporation;
	float FFireCrown;
	float FParticleInterval;
	float IntervalDelta;
	int NP;
	Glcolor::TGLColor* FInnerColor;
	Glcolor::TGLColor* FOuterColor;
	float FFireBurst;
	float FFireRadius;
	bool FDisabled;
	bool FPaused;
	bool FUseInterval;
	Glscene::TGLBaseSceneObject* FReference;
	bool FNoZWrite;
	
protected:
	void __fastcall RegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterAllClients();
	void __fastcall SetFireDir(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetInitialDir(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const val);
	bool __fastcall StoreParticleSize();
	void __fastcall SetInnerColor(Glcolor::TGLColor* const val);
	void __fastcall SetOuterColor(Glcolor::TGLColor* const val);
	HIDESBASE void __fastcall SetReference(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetMaxParticles(const int val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall CalcFire(double deltaTime, float ParticleInterval, float ParticleLife, float FireAlpha);
	void __fastcall AffParticle3d(const Glcolor::TColorVector &Color2, const Glvectorgeometry::TMatrix &mat);
	
public:
	__fastcall virtual TGLFireFXManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFireFXManager();
	void __fastcall FireInit();
	void __fastcall IsotropicExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, int nbParticles = 0xffffffff);
	void __fastcall RingExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, const Glvectorgeometry::TAffineVector &ringVectorX, const Glvectorgeometry::TAffineVector &ringVectorY, int nbParticles = 0xffffffff);
	__property int ParticleCount = {read=NP, nodefault};
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates* FireDir = {read=FFireDir, write=SetFireDir};
	__property Glcoordinates::TGLCoordinates* InitialDir = {read=FInitialDir, write=SetInitialDir};
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property int MaxParticles = {read=FMaxParticles, write=SetMaxParticles, default=256};
	__property float ParticleSize = {read=FParticleSize, write=FParticleSize, stored=StoreParticleSize};
	__property Glcolor::TGLColor* InnerColor = {read=FInnerColor, write=SetInnerColor};
	__property Glcolor::TGLColor* OuterColor = {read=FOuterColor, write=SetOuterColor};
	__property float FireDensity = {read=FFireDensity, write=FFireDensity};
	__property float FireEvaporation = {read=FFireEvaporation, write=FFireEvaporation};
	__property float FireCrown = {read=FFireCrown, write=FFireCrown};
	__property int ParticleLife = {read=FParticleLife, write=FParticleLife, default=3};
	__property float FireBurst = {read=FFireBurst, write=FFireBurst};
	__property float FireRadius = {read=FFireRadius, write=FFireRadius};
	__property bool Disabled = {read=FDisabled, write=FDisabled, nodefault};
	__property bool Paused = {read=FPaused, write=FPaused, nodefault};
	__property float ParticleInterval = {read=FParticleInterval, write=FParticleInterval};
	__property bool UseInterval = {read=FUseInterval, write=FUseInterval, nodefault};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, default=1};
	__property Glscene::TGLBaseSceneObject* Reference = {read=FReference, write=SetReference};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBFireFX : public Glscene::TGLObjectPostEffect
{
	typedef Glscene::TGLObjectPostEffect inherited;
	
private:
	TGLFireFXManager* FManager;
	System::UnicodeString FManagerName;
	
protected:
	void __fastcall SetManager(TGLFireFXManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLBFireFX(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFireFX();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Render(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLFireFXManager* Manager = {read=FManager, write=SetManager};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Glscene::TGLObjectEffects* effects)/* overload */;
extern DELPHI_PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glfirefx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFIREFX)
using namespace Glfirefx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfirefxHPP
