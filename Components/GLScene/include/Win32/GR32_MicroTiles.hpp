// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_MicroTiles.pas' rev: 36.00 (Windows)

#ifndef Gr32_microtilesHPP
#define Gr32_microtilesHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <GR32.hpp>
#include <GR32_System.hpp>
#include <GR32_Containers.hpp>
#include <GR32_Layers.hpp>
#include <GR32_RepaintOpt.hpp>
#include <GR32_Bindings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_microtiles
{
//-- forward type declarations -----------------------------------------------
struct TMicroTiles;
class DELPHICLASS TMicroTilesMap;
class DELPHICLASS TMicroTilesRepaintOptimizer;
//-- type declarations -------------------------------------------------------
typedef int TMicroTile;

typedef TMicroTile *PMicroTile;

typedef System::StaticArray<TMicroTile, 134217727> TMicroTileArray;

typedef TMicroTileArray *PMicroTileArray;

typedef TMicroTiles *PMicroTiles;

typedef PMicroTiles *PPMicroTiles;

struct DECLSPEC_DRECORD TMicroTiles
{
public:
	Gr32::TRect BoundsRect;
	int Columns;
	int Rows;
	Gr32::TRect BoundsUsedTiles;
	int Count;
	PMicroTileArray Tiles;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TMicroTilesMap : public Gr32_containers::TPointerMap
{
	typedef Gr32_containers::TPointerMap inherited;
	
public:
	PMicroTiles operator[](void * Item) { return this->Data[Item]; }
	
private:
	HIDESBASE PMicroTiles __fastcall GetData(void * Item);
	HIDESBASE void __fastcall SetData(void * Item, const PMicroTiles Data);
	
protected:
	virtual void * __fastcall Delete(int BucketIndex, int ItemIndex);
	
public:
	HIDESBASE PPMicroTiles __fastcall Add(void * Item);
	__property PMicroTiles Data[void * Item] = {read=GetData, write=SetData/*, default*/};
public:
	/* TPointerMap.Destroy */ inline __fastcall virtual ~TMicroTilesMap() { }
	
public:
	/* TObject.Create */ inline __fastcall TMicroTilesMap() : Gr32_containers::TPointerMap() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TMicroTilesRepaintOptimizer : public Gr32_repaintopt::TCustomRepaintOptimizer
{
	typedef Gr32_repaintopt::TCustomRepaintOptimizer inherited;
	
private:
	Gr32::TRect FBufferBounds;
	PMicroTiles FWorkMicroTiles;
	TMicroTiles FTempTiles;
	TMicroTiles FInvalidTiles;
	TMicroTiles FForcedInvalidTiles;
	System::Classes::TList* FInvalidLayers;
	TMicroTilesMap* FOldInvalidTilesMap;
	bool FWorkingTilesValid;
	bool FOldInvalidTilesValid;
	bool FUseInvalidTiles;
	bool FAdaptiveMode;
	Gr32_system::TPerfTimer* FPerfTimer;
	int FPerformanceLevel;
	__int64 FElapsedTimeForLastRepaint;
	__int64 FElapsedTimeForFullSceneRepaint;
	bool FAdaptionFailed;
	bool FTimedCheck;
	int FTimeDelta;
	int FNextCheck;
	__int64 FElapsedTimeOnLastPenalty;
	int FOldInvalidRectsCount;
	void __fastcall DrawLayerToMicroTiles(TMicroTiles &DstTiles, Gr32_layers::TCustomLayer* Layer);
	void __fastcall DrawMeasuringHandler(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	void __fastcall ValidateWorkingTiles();
	void __fastcall UpdateOldInvalidTiles();
	void __fastcall SetAdaptiveMode(const bool Value);
	void __fastcall ResetAdaptiveMode();
	void __fastcall BeginAdaption();
	void __fastcall EndAdaption();
	void __fastcall AddArea(TMicroTiles &Tiles, const Gr32::TRect &Area, const unsigned Info);
	
protected:
	virtual void __fastcall SetEnabled(const bool Value);
	virtual void __fastcall LayerCollectionNotifyHandler(Gr32_layers::TLayerCollection* Sender, Gr32_layers::TLayerListNotification Action, Gr32_layers::TCustomLayer* Layer, int Index);
	
public:
	__fastcall virtual TMicroTilesRepaintOptimizer(Gr32::TBitmap32* Buffer, Gr32_containers::TRectList* InvalidRects);
	__fastcall virtual ~TMicroTilesRepaintOptimizer();
	virtual void __fastcall RegisterLayerCollection(Gr32_layers::TLayerCollection* Layers);
	virtual void __fastcall UnregisterLayerCollection(Gr32_layers::TLayerCollection* Layers);
	virtual void __fastcall Reset();
	virtual bool __fastcall UpdatesAvailable();
	virtual void __fastcall PerformOptimization();
	virtual void __fastcall BeginPaintBuffer();
	virtual void __fastcall EndPaintBuffer();
	virtual void __fastcall AreaUpdateHandler(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	virtual void __fastcall LayerUpdateHandler(System::TObject* Sender, Gr32_layers::TCustomLayer* Layer);
	virtual void __fastcall BufferResizedHandler(const int NewWidth, const int NewHeight);
	__property bool AdaptiveMode = {read=FAdaptiveMode, write=SetAdaptiveMode, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MICROTILE_SHIFT = System::Int8(0x5);
static const System::Int8 MICROTILE_SIZE = System::Int8(0x20);
static const System::Int8 MICROTILE_EMPTY = System::Int8(0x0);
static const System::Word MICROTILE_FULL = System::Word(0x2020);
static const int MicroTileSize = int(0x7ffffff);
extern DELPHI_PACKAGE void __fastcall (*MicroTileUnion)(TMicroTile &DstTile, const TMicroTile SrcTile);
extern DELPHI_PACKAGE TMicroTile __fastcall MakeMicroTile(const int Left, const int Top, const int Right, const int Bottom);
extern DELPHI_PACKAGE int __fastcall MicroTileHeight(const TMicroTile Tile);
extern DELPHI_PACKAGE int __fastcall MicroTileWidth(const TMicroTile Tile);
extern DELPHI_PACKAGE TMicroTiles __fastcall MakeEmptyMicroTiles(void);
extern DELPHI_PACKAGE void __fastcall MicroTilesCreate(TMicroTiles &MicroTiles);
extern DELPHI_PACKAGE void __fastcall MicroTilesDestroy(TMicroTiles &MicroTiles);
extern DELPHI_PACKAGE void __fastcall MicroTilesSetSize(TMicroTiles &MicroTiles, const Gr32::TRect &DstRect);
extern DELPHI_PACKAGE void __fastcall MicroTilesClear(TMicroTiles &MicroTiles, const TMicroTile Value = (TMicroTile)(0x0));
extern DELPHI_PACKAGE void __fastcall MicroTilesClearUsed(TMicroTiles &MicroTiles, const TMicroTile Value = (TMicroTile)(0x0));
extern DELPHI_PACKAGE void __fastcall MicroTilesCopy(TMicroTiles &DstTiles, const TMicroTiles &SrcTiles);
extern DELPHI_PACKAGE void __fastcall MicroTilesAddLine(TMicroTiles &MicroTiles, int X1, int Y1, int X2, int Y2, int LineWidth, bool RoundToWholeTiles = false);
extern DELPHI_PACKAGE void __fastcall MicroTilesAddRect(TMicroTiles &MicroTiles, const Gr32::TRect &Rect, bool RoundToWholeTiles = false);
extern DELPHI_PACKAGE void __fastcall MicroTilesUnion(TMicroTiles &DstTiles, const TMicroTiles &SrcTiles, bool RoundToWholeTiles = false);
extern DELPHI_PACKAGE int __fastcall MicroTilesCalcRects(const TMicroTiles &MicroTiles, Gr32_containers::TRectList* DstRects, bool CountOnly = false, bool RoundToWholeTiles = false)/* overload */;
extern DELPHI_PACKAGE int __fastcall MicroTilesCalcRects(const TMicroTiles &MicroTiles, Gr32_containers::TRectList* DstRects, const Gr32::TRect &Clip, bool CountOnly = false, bool RoundToWholeTiles = false)/* overload */;
extern DELPHI_PACKAGE int __fastcall MicroTilesCountEmptyTiles(const TMicroTiles &MicroTiles);
}	/* namespace Gr32_microtiles */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_MICROTILES)
using namespace Gr32_microtiles;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_microtilesHPP
