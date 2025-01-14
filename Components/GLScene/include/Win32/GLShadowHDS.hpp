// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShadowHDS.pas' rev: 36.00 (Windows)

#ifndef GlshadowhdsHPP
#define GlshadowhdsHPP

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
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorLists.hpp>
#include <GLHeightData.hpp>
#include <GLGraphics.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTexture.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>
#include <GLMaterial.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glshadowhds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShadowHDS;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLShadowHDS* Sender, Glheightdata::TGLHeightData* heightData, Glmaterial::TGLLibMaterial* ShadowMapMaterial);

typedef void __fastcall (__closure *TThreadBmp32)(TGLShadowHDS* Sender, Glheightdata::TGLHeightData* heightData, Glgraphics::TGLBitmap32* bmp32);

class PASCALIMPLEMENTATION TGLShadowHDS : public Glheightdata::TGLHeightDataSourceFilter
{
	typedef Glheightdata::TGLHeightDataSourceFilter inherited;
	
private:
	int FTileSize;
	Glmaterial::TGLMaterialLibrary* FShadowmapLibrary;
	Glcoordinates::TGLCoordinates* FLightVector;
	Glcoordinates::TGLCoordinates* FScale;
	Glvectortypes::TVector3f FScaleVec;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TThreadBmp32 FOnThreadBmp32;
	int FMaxTextures;
	Glvectortypes::TVector3f Step;
	int FScanDistance;
	unsigned FSoftRange;
	float FDiffuse;
	float FAmbient;
	Glheightdata::TGLHeightDataSource* OwnerHDS;
	
protected:
	void __fastcall SetShadowmapLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetScale(Glcoordinates::TGLCoordinates* AValue);
	void __fastcall SetLightVector(Glcoordinates::TGLCoordinates* AValue);
	void __fastcall SetSoftRange(unsigned AValue);
	void __fastcall SetDiffuse(float AValue);
	void __fastcall SetAmbient(float AValue);
	void __fastcall Trim(int MaxTextureCount);
	Glmaterial::TGLLibMaterial* __fastcall FindUnusedMaterial();
	Glvectorgeometry::TAffineVector __fastcall CalcStep();
	Glvectorgeometry::TAffineVector __fastcall CalcScale();
	int __fastcall WrapDist(float Lx, float Ly);
	void __fastcall LocalToWorld(float Lx, float Ly, Glheightdata::TGLHeightData* HD, float &Wx, float &Wy);
	void __fastcall WorldToLocal(float Wx, float Wy, Glheightdata::TGLHeightData* &HD, float &Lx, float &Ly);
	
public:
	bool SkipGenerate;
	__fastcall virtual TGLShadowHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowHDS();
	void __fastcall TrimTextureCache(int MaxTextureCount = 0x0);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BeforePreparingData(Glheightdata::TGLHeightData* heightData);
	virtual void __fastcall PreparingData(Glheightdata::TGLHeightData* heightData);
	virtual void __fastcall AfterPreparingData(Glheightdata::TGLHeightData* heightData);
	void __fastcall GenerateShadowMap(Glheightdata::TGLHeightData* heightData, Glgraphics::TGLBitmap32* ShadowMap, float scale);
	float __fastcall RayCastShadowHeight(Glheightdata::TGLHeightData* HD, float localX, float localY)/* overload */;
	void __fastcall RayCastLine(Glheightdata::TGLHeightData* heightData, float Lx, float Ly, Glgraphics::TGLBitmap32* ShadowMap);
	System::Byte __fastcall Shade(Glheightdata::TGLHeightData* heightData, int x, int y, float ShadowHeight, float TerrainHeight);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* ShadowmapLibrary = {read=FShadowmapLibrary, write=SetShadowmapLibrary};
	__property TThreadBmp32 OnThreadBmp32 = {read=FOnThreadBmp32, write=FOnThreadBmp32};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property Glcoordinates::TGLCoordinates* LightVector = {read=FLightVector, write=SetLightVector};
	__property Glcoordinates::TGLCoordinates* scale = {read=FScale, write=FScale};
	__property int ScanDistance = {read=FScanDistance, write=FScanDistance, nodefault};
	__property unsigned SoftRange = {read=FSoftRange, write=SetSoftRange, nodefault};
	__property float Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property float Ambient = {read=FAmbient, write=SetAmbient};
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glshadowhds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADOWHDS)
using namespace Glshadowhds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadowhdsHPP
