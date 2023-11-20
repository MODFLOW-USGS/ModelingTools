// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTerrainRenderer.pas' rev: 34.00 (Windows)

#ifndef GlterrainrendererHPP
#define GlterrainrendererHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLCoordinates.hpp>
#include <GLHeightData.hpp>
#include <GLMaterial.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLROAMPatch.hpp>
#include <GLVectorLists.hpp>
#include <GLRenderContextInfo.hpp>
#include <XOpenGL.hpp>
#include <GLUtils.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glterrainrenderer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTerrainRenderer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGetTerrainBoundsEvent)(float &l, float &t, float &r, float &b);

typedef void __fastcall (__closure *TPatchPostRenderEvent)(Glrendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* const patches);

typedef void __fastcall (__closure *TGLHeightDataPostRenderEvent)(Glrendercontextinfo::TGLRenderContextInfo &rci, System::Classes::TList* &HeightDatas);

typedef void __fastcall (__closure *TMaxCLODTrianglesReachedEvent)(Glrendercontextinfo::TGLRenderContextInfo &rci);

enum DECLSPEC_DENUM TTerrainHighResStyle : unsigned char { hrsFullGeometry, hrsTesselated };

enum DECLSPEC_DENUM TTerrainOcclusionTesselate : unsigned char { totTesselateAlways, totTesselateIfVisible };

enum DECLSPEC_DENUM TTileManagementFlag : unsigned char { tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing };

typedef System::Set<TTileManagementFlag, TTileManagementFlag::tmClearUsedFlags, TTileManagementFlag::tmWaitForPreparing> TTileManagementFlags;

class PASCALIMPLEMENTATION TGLTerrainRenderer : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	Glheightdata::TGLHeightDataSource* FHeightDataSource;
	int FTileSize;
	float FQualityDistance;
	float FinvTileSize;
	int FLastTriangleCount;
	float FTilesPerTexture;
	int FMaxCLODTriangles;
	int FCLODPrecision;
	Glvectorlists::TAffineVectorList* FBufferVertices;
	Glvectorlists::TTexPointList* FBufferTexPoints;
	Glvectorlists::TIntegerList* FBufferVertexIndices;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	TGetTerrainBoundsEvent FOnGetTerrainBounds;
	TPatchPostRenderEvent FOnPatchPostRender;
	TGLHeightDataPostRenderEvent FOnHeightDataPostRender;
	TMaxCLODTrianglesReachedEvent FOnMaxCLODTrianglesReached;
	TTerrainHighResStyle FQualityStyle;
	int FOcclusionFrameSkip;
	TTerrainOcclusionTesselate FOcclusionTesselate;
	int FContourInterval;
	int FContourWidth;
	
protected:
	System::StaticArray<System::Classes::TList*, 256> FTilesHash;
	void __fastcall MarkAllTilesAsUnused();
	void __fastcall ReleaseAllUnusedTiles();
	void __fastcall MarkHashedTileAsUsed(const Glvectortypes::TVector3f &tilePos);
	Glheightdata::TGLHeightData* __fastcall HashedTile(const Glvectortypes::TVector3f &tilePos, bool canAllocate = true)/* overload */;
	Glheightdata::TGLHeightData* __fastcall HashedTile(const int xLeft, const int yTop, bool canAllocate = true)/* overload */;
	void __fastcall SetHeightDataSource(Glheightdata::TGLHeightDataSource* const val);
	void __fastcall SetTileSize(const int val);
	void __fastcall SetTilesPerTexture(const float val);
	void __fastcall SetCLODPrecision(const int val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Val);
	void __fastcall SetQualityStyle(const TTerrainHighResStyle Val);
	void __fastcall SetOcclusionFrameSkip(int Val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DestroyHandle();
	void __fastcall ReleaseAllTiles();
	void __fastcall OnTileDestroyed(System::TObject* Sender);
	Glroampatch::TGLROAMPatch* __fastcall GetPreparedPatch(const Glvectortypes::TVector3f &TilePos, const Glvectortypes::TVector3f &EyePos, float TexFactor, System::Classes::TList* HDList);
	
public:
	TTileManagementFlags TileManagement;
	__fastcall virtual TGLTerrainRenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTerrainRenderer();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	float __fastcall InterpolatedHeight(const Glvectortypes::TVector4f &p)/* overload */;
	float __fastcall InterpolatedHeight(const Glvectortypes::TVector3f &p)/* overload */;
	__property int LastTriangleCount = {read=FLastTriangleCount, nodefault};
	int __fastcall HashedTileCount();
	
__published:
	__property Glheightdata::TGLHeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property int TileSize = {read=FTileSize, write=SetTileSize, default=16};
	__property float TilesPerTexture = {read=FTilesPerTexture, write=SetTilesPerTexture};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property float QualityDistance = {read=FQualityDistance, write=FQualityDistance};
	__property TTerrainHighResStyle QualityStyle = {read=FQualityStyle, write=SetQualityStyle, default=0};
	__property int MaxCLODTriangles = {read=FMaxCLODTriangles, write=FMaxCLODTriangles, default=65536};
	__property int CLODPrecision = {read=FCLODPrecision, write=SetCLODPrecision, default=100};
	__property int OcclusionFrameSkip = {read=FOcclusionFrameSkip, write=SetOcclusionFrameSkip, default=0};
	__property TTerrainOcclusionTesselate OcclusionTesselate = {read=FOcclusionTesselate, write=FOcclusionTesselate, default=1};
	__property TGetTerrainBoundsEvent OnGetTerrainBounds = {read=FOnGetTerrainBounds, write=FOnGetTerrainBounds};
	__property TPatchPostRenderEvent OnPatchPostRender = {read=FOnPatchPostRender, write=FOnPatchPostRender};
	__property TGLHeightDataPostRenderEvent OnHeightDataPostRender = {read=FOnHeightDataPostRender, write=FOnHeightDataPostRender};
	__property TMaxCLODTrianglesReachedEvent OnMaxCLODTrianglesReached = {read=FOnMaxCLODTrianglesReached, write=FOnMaxCLODTrianglesReached};
	__property int ContourInterval = {read=FContourInterval, write=FContourInterval, default=0};
	__property int ContourWidth = {read=FContourWidth, write=FContourWidth, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTerrainRenderer(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte cTilesHashSize = System::Byte(0xff);
}	/* namespace Glterrainrenderer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTERRAINRENDERER)
using namespace Glterrainrenderer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlterrainrendererHPP
