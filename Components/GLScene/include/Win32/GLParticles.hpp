// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLParticles.pas' rev: 36.00 (Windows)

#ifndef GlparticlesHPP
#define GlparticlesHPP

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
#include <GLScene.hpp>
#include <GLXCollection.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glparticles
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLParticles;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGLParticleEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* particle);

class PASCALIMPLEMENTATION TGLParticles : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Opengltokens::TGLfloat FCubeSize;
	Glcolor::TGLColor* FEdgeColor;
	bool FVisibleAtRunTime;
	System::Classes::TList* particlePool;
	int FParticlePoolSize;
	TGLParticleEvent FOnCreateParticle;
	TGLParticleEvent FOnActivateParticle;
	TGLParticleEvent FOnKillParticle;
	TGLParticleEvent FOnDestroyParticle;
	Glscene::TGLDirectRenderEvent FOnBeforeRenderParticles;
	Glscene::TGLDirectRenderEvent FOnAfterRenderParticles;
	
protected:
	void __fastcall SetCubeSize(const Opengltokens::TGLfloat val);
	void __fastcall SetEdgeColor(Glcolor::TGLColor* const val);
	void __fastcall SetVisibleAtRunTime(const bool val);
	void __fastcall SetParticlePoolSize(int val);
	void __fastcall ClearParticlePool();
	
public:
	__fastcall virtual TGLParticles(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLParticles();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	Glscene::TGLBaseSceneObject* __fastcall CreateParticle();
	void __fastcall KillParticle(Glscene::TGLBaseSceneObject* aParticle);
	void __fastcall KillParticles();
	
__published:
	__property Opengltokens::TGLfloat CubeSize = {read=FCubeSize, write=SetCubeSize};
	__property Glcolor::TGLColor* EdgeColor = {read=FEdgeColor, write=SetEdgeColor};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property int ParticlePoolSize = {read=FParticlePoolSize, write=SetParticlePoolSize, default=0};
	__property TGLParticleEvent OnCreateParticle = {read=FOnCreateParticle, write=FOnCreateParticle};
	__property TGLParticleEvent OnActivateParticle = {read=FOnActivateParticle, write=FOnActivateParticle};
	__property TGLParticleEvent OnKillParticle = {read=FOnKillParticle, write=FOnKillParticle};
	__property TGLParticleEvent OnDestroyParticle = {read=FOnDestroyParticle, write=FOnDestroyParticle};
	__property Glscene::TGLDirectRenderEvent OnBeforeRenderParticles = {read=FOnBeforeRenderParticles, write=FOnBeforeRenderParticles};
	__property Glscene::TGLDirectRenderEvent OnAfterRenderParticles = {read=FOnAfterRenderParticles, write=FOnAfterRenderParticles};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLParticles(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glparticles */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPARTICLES)
using namespace Glparticles;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlparticlesHPP
