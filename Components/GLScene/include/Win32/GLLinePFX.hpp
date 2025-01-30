// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLLinePFX.pas' rev: 36.00 (Windows)

#ifndef GLLinePFXHPP
#define GLLinePFXHPP

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
#include <OpenGLTokens.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLParticleFX.hpp>
#include <GLTexture.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLContext.hpp>
#include <GLVectorTypes.hpp>
#include <GLCadencer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gllinepfx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLineParticle;
class DELPHICLASS TGLLinePFXManager;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLineParticle : public Glparticlefx::TGLParticle
{
	typedef Glparticlefx::TGLParticle inherited;
	
private:
	Glvectorgeometry::TAffineVector FDirection;
	float FLength;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	__property Glvectorgeometry::TAffineVector Direction = {read=FDirection, write=FDirection};
	__property float Length = {read=FLength, write=FLength};
public:
	/* TGLParticle.Create */ inline __fastcall virtual TGLLineParticle() : Glparticlefx::TGLParticle() { }
	/* TGLParticle.Destroy */ inline __fastcall virtual ~TGLLineParticle() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLLineParticle(Glpersistentclasses::TVirtualReader* reader) : Glparticlefx::TGLParticle(reader) { }
	
};


class PASCALIMPLEMENTATION TGLLinePFXManager : public Glparticlefx::TGLLifeColoredPFXManager
{
	typedef Glparticlefx::TGLLifeColoredPFXManager inherited;
	
private:
	Glvectorgeometry::TAffineVector Fvx;
	Glvectorgeometry::TAffineVector Fvy;
	Glvectorgeometry::TAffineVector FNvx;
	Glvectorgeometry::TAffineVector FNvy;
	float FDefaultLength;
	
protected:
	bool __fastcall StoreDefaultLength();
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TGLRenderContextInfo &rci, Glparticlefx::TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLLinePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLLinePFXManager();
	__classmethod virtual Glparticlefx::TGLParticleClass __fastcall ParticlesClass();
	virtual Glparticlefx::TGLParticle* __fastcall CreateParticle();
	
__published:
	__property float DefaultLength = {read=FDefaultLength, write=FDefaultLength, stored=StoreDefaultLength};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gllinepfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLLINEPFX)
using namespace Gllinepfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLLinePFXHPP
