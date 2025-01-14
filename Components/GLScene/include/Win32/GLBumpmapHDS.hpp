// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBumpmapHDS.pas' rev: 36.00 (Windows)

#ifndef GlbumpmaphdsHPP
#define GlbumpmaphdsHPP

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
#include <System.SyncObjs.hpp>
#include <OpenGLTokens.hpp>
#include <GLCoordinates.hpp>
#include <GLHeightData.hpp>
#include <GLGraphics.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLUtils.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbumpmaphds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBumpmapHDS;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLBumpmapHDS* Sender, Glheightdata::TGLHeightData* heightData, Glmaterial::TGLLibMaterial* normalMapMaterial);

class PASCALIMPLEMENTATION TGLBumpmapHDS : public Glheightdata::TGLHeightDataSourceFilter
{
	typedef Glheightdata::TGLHeightDataSourceFilter inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* FBumpmapLibrary;
	TNewTilePreparedEvent FOnNewTilePrepared;
	float FBumpScale;
	int FSubSampling;
	int FMaxTextures;
	System::Syncobjs::TCriticalSection* Uno;
	
protected:
	void __fastcall SetBumpmapLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetBumpScale(const float val);
	bool __fastcall StoreBumpScale();
	void __fastcall SetSubSampling(const int val);
	void __fastcall Trim(int MaxTextureCount);
	
public:
	__fastcall virtual TGLBumpmapHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpmapHDS();
	virtual void __fastcall Release(Glheightdata::TGLHeightData* aHeightData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall GenerateNormalMap(Glheightdata::TGLHeightData* heightData, Glgraphics::TGLImage* normalMap, float scale);
	void __fastcall TrimTextureCache(int MaxTextureCount);
	virtual void __fastcall PreparingData(Glheightdata::TGLHeightData* heightData);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* BumpmapLibrary = {read=FBumpmapLibrary, write=SetBumpmapLibrary};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property float BumpScale = {read=FBumpScale, write=SetBumpScale, stored=StoreBumpScale};
	__property int SubSampling = {read=FSubSampling, write=SetSubSampling, default=1};
	__property MaxPoolSize;
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbumpmaphds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBUMPMAPHDS)
using namespace Glbumpmaphds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbumpmaphdsHPP
