// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTilePlane.pas' rev: 36.00 (Windows)

#ifndef GltileplaneHPP
#define GltileplaneHPP

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
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLState.hpp>
#include <GLVectorGeometry.hpp>
#include <GLContext.hpp>
#include <GLMaterial.hpp>
#include <GLObjects.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorLists.hpp>
#include <GLRenderContextInfo.hpp>
#include <XOpenGL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltileplane
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTiledAreaRow;
class DELPHICLASS TGLTiledArea;
class DELPHICLASS TGLTilePlane;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTiledAreaRow : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
public:
	int operator[](int col) { return this->Cell[col]; }
	
private:
	int FColMin;
	int FColMax;
	Glvectorlists::TIntegerList* FData;
	
protected:
	void __fastcall SetColMin(const int val);
	void __fastcall SetColMax(const int val);
	int __fastcall GetCell(int col);
	void __fastcall SetCell(int col, int val);
	
public:
	__fastcall virtual TGLTiledAreaRow();
	__fastcall virtual ~TGLTiledAreaRow();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	__property int Cell[int col] = {read=GetCell, write=SetCell/*, default*/};
	__property int ColMin = {read=FColMin, write=SetColMin, nodefault};
	__property int ColMax = {read=FColMax, write=SetColMax, nodefault};
	__property Glvectorlists::TIntegerList* Data = {read=FData};
	void __fastcall Pack();
	bool __fastcall Empty();
	void __fastcall RemapTiles(Glvectorlists::TIntegerList* remapList);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledAreaRow(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTiledArea : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	int FRowMin;
	int FRowMax;
	Glpersistentclasses::TPersistentObjectList* FRows;
	
protected:
	void __fastcall SetRowMin(const int val);
	void __fastcall SetRowMax(const int val);
	int __fastcall GetTile(int col, int row);
	void __fastcall SetTile(int col, int row, int val);
	TGLTiledAreaRow* __fastcall GetRow(int index);
	
public:
	__fastcall virtual TGLTiledArea();
	__fastcall virtual ~TGLTiledArea();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	__property int Tile[int col][int row] = {read=GetTile, write=SetTile/*, default*/};
	__property TGLTiledAreaRow* Row[int index] = {read=GetRow};
	__property int RowMin = {read=FRowMin, write=SetRowMin, nodefault};
	__property int RowMax = {read=FRowMax, write=SetRowMax, nodefault};
	void __fastcall Pack();
	void __fastcall Clear();
	bool __fastcall Empty();
	void __fastcall RemapTiles(Glvectorlists::TIntegerList* remapList);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledArea(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTilePlane : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	bool FNoZWrite;
	TGLTiledArea* FTiles;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FSortByMaterials;
	
protected:
	void __fastcall SetNoZWrite(const bool val);
	void __fastcall SetTiles(TGLTiledArea* const val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetSortByMaterials(const bool val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTilePlane(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTilePlane();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property TGLTiledArea* Tiles = {read=FTiles, write=SetTiles};
	__property bool SortByMaterials = {read=FSortByMaterials, write=SetSortByMaterials, nodefault};
	
__published:
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTilePlane(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltileplane */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTILEPLANE)
using namespace Gltileplane;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltileplaneHPP
