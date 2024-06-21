// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHeightTileFile.pas' rev: 36.00 (Windows)

#ifndef GlheighttilefileHPP
#define GlheighttilefileHPP

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
#include <GLApplicationFileIO.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glheighttilefile
{
//-- forward type declarations -----------------------------------------------
struct TGLHeightTileInfo;
struct TGLHeightTile;
struct THTFHeader;
class DELPHICLASS TGLHeightTileFile;
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef System::StaticArray<int, 268435456> TIntegerArray;

typedef TIntegerArray *PIntegerArray;

typedef int *PInteger;

typedef System::StaticArray<short, 536870912> TSmallIntArray;

typedef TSmallIntArray *PSmallIntArray;

typedef short *PSmallInt;

typedef System::StaticArray<System::Int8, 536870912> TShortIntArray;

typedef TShortIntArray *PShortIntArray;

typedef System::Int8 *PShortInt;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLHeightTileInfo
{
public:
	int left;
	int top;
	int width;
	int height;
	short min;
	short max;
	short average;
	__int64 fileOffset;
};
#pragma pack(pop)


typedef TGLHeightTileInfo *PHeightTileInfo;

typedef PHeightTileInfo *PPHeightTileInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLHeightTile
{
	
private:
	typedef System::DynamicArray<short> _TGLHeightTile__1;
	
	
public:
	TGLHeightTileInfo info;
	_TGLHeightTile__1 data;
};
#pragma pack(pop)


typedef TGLHeightTile *PHeightTile;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THTFHeader
{
public:
	System::StaticArray<char, 6> FileVersion;
	__int64 TileIndexOffset;
	int SizeX;
	int SizeY;
	int TileSize;
	short DefaultZ;
};
#pragma pack(pop)


typedef System::DynamicArray<int> _TGLHeightTileFile__3;

class PASCALIMPLEMENTATION TGLHeightTileFile : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLHeightTileInfo> _TGLHeightTileFile__1;
	
	typedef System::DynamicArray<unsigned> _TGLHeightTileFile__2;
	
	typedef System::StaticArray<_TGLHeightTileFile__3, 1024> _TGLHeightTileFile__4;
	
	typedef System::DynamicArray<int> _TGLHeightTileFile__5;
	
	typedef System::StaticArray<System::StaticArray<_TGLHeightTileFile__5, 32>, 32> _TGLHeightTileFile__6;
	
	typedef System::DynamicArray<System::Int8> _TGLHeightTileFile__7;
	
	
private:
	System::Classes::TStream* FFile;
	THTFHeader FHeader;
	_TGLHeightTileFile__1 FTileIndex;
	_TGLHeightTileFile__2 FTileMark;
	unsigned FLastMark;
	_TGLHeightTileFile__4 FHashTable;
	_TGLHeightTileFile__6 FQuadTable;
	bool FCreating;
	TGLHeightTile FHeightTile;
	_TGLHeightTileFile__7 FInBuf;
	
protected:
	PHeightTileInfo __fastcall GetTiles(int index);
	int __fastcall QuadTableX(int x);
	int __fastcall QuadTableY(int y);
	void __fastcall PackTile(int aWidth, int aHeight, PSmallIntArray src);
	void __fastcall UnPackTile(PShortIntArray source);
	__property __int64 TileIndexOffset = {read=FHeader.TileIndexOffset, write=FHeader.TileIndexOffset};
	
public:
	__fastcall TGLHeightTileFile(const System::UnicodeString fileName, int aSizeX, int aSizeY, int aTileSize);
	__fastcall TGLHeightTileFile(const System::UnicodeString fileName);
	__fastcall virtual ~TGLHeightTileFile();
	int __fastcall GetTileIndex(int aLeft, int aTop);
	PHeightTile __fastcall GetTile(int aLeft, int aTop, PPHeightTileInfo pTileInfo = (PPHeightTileInfo)(0x0));
	void __fastcall CompressTile(int aLeft, int aTop, int aWidth, int aHeight, PSmallIntArray aData);
	void __fastcall ExtractRow(int x, int y, int len, PSmallIntArray dest);
	PHeightTileInfo __fastcall XYTileInfo(int anX, int anY);
	short __fastcall XYHeight(int anX, int anY);
	void __fastcall TilesInRect(int aLeft, int aTop, int aRight, int aBottom, System::Classes::TList* destList);
	int __fastcall TileCount();
	__property PHeightTileInfo Tiles[int index] = {read=GetTiles};
	int __fastcall IndexOfTile(PHeightTileInfo aTile);
	int __fastcall TileCompressedSize(int tileIndex);
	__property int SizeX = {read=FHeader.SizeX, nodefault};
	__property int SizeY = {read=FHeader.SizeY, nodefault};
	__property int TileSize = {read=FHeader.TileSize, nodefault};
	__property short DefaultZ = {read=FHeader.DefaultZ, write=FHeader.DefaultZ, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cHTFHashTableSize = System::Word(0x3ff);
static const System::Int8 cHTFQuadTableSize = System::Int8(0x1f);
}	/* namespace Glheighttilefile */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHEIGHTTILEFILE)
using namespace Glheighttilefile;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlheighttilefileHPP
