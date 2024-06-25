// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexLensFlare.pas' rev: 36.00 (Windows)

#ifndef GltexlensflareHPP
#define GltexlensflareHPP

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
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLObjects.hpp>
#include <GLTexture.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>
#include <GLState.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltexlensflare
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureLensFlare;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTextureLensFlare : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	int FSize;
	float FCurrSize;
	int FNumSecs;
	bool FAutoZTest;
	double FDeltaTime;
	Gltexture::TGLTexture* FImgSecondaries;
	Gltexture::TGLTexture* FImgRays;
	Gltexture::TGLTexture* FImgRing;
	Gltexture::TGLTexture* FImgGlow;
	int FSeed;
	void __fastcall SetImgGlow(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgRays(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgRing(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgSecondaries(Gltexture::TGLTexture* const Value);
	void __fastcall SetSeed(const int Value);
	
protected:
	void __fastcall SetSize(int aValue);
	void __fastcall SetNumSecs(int aValue);
	void __fastcall SetAutoZTest(bool aValue);
	
public:
	__fastcall virtual TGLTextureLensFlare(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureLensFlare();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property int NumSecs = {read=FNumSecs, write=SetNumSecs, default=8};
	__property bool AutoZTest = {read=FAutoZTest, write=SetAutoZTest, default=1};
	__property Gltexture::TGLTexture* ImgGlow = {read=FImgGlow, write=SetImgGlow};
	__property Gltexture::TGLTexture* ImgRays = {read=FImgRays, write=SetImgRays};
	__property Gltexture::TGLTexture* ImgRing = {read=FImgRing, write=SetImgRing};
	__property Gltexture::TGLTexture* ImgSecondaries = {read=FImgSecondaries, write=SetImgSecondaries};
	__property ObjectsSorting = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTextureLensFlare(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexlensflare */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXLENSFLARE)
using namespace Gltexlensflare;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexlensflareHPP
