// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexturedHDS.pas' rev: 36.00 (Windows)

#ifndef GltexturedhdsHPP
#define GltexturedhdsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>
#include <GLHeightData.hpp>
#include <GLMaterial.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltexturedhds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTexturedHDS;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTexturedHDS : public Glheightdata::TGLHeightDataSource
{
	typedef Glheightdata::TGLHeightDataSource inherited;
	
private:
	Glheightdata::TStartPreparingDataEvent FOnStartPreparingData;
	Glheightdata::TMarkDirtyEvent FOnMarkDirty;
	Glheightdata::TGLHeightDataSource* FHeightDataSource;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FWholeTilesOnly;
	int FTileSize;
	int FTilesPerTexture;
	
protected:
	void __fastcall SetHeightDataSource(Glheightdata::TGLHeightDataSource* val);
	
public:
	__fastcall virtual TGLTexturedHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexturedHDS();
	virtual void __fastcall StartPreparingData(Glheightdata::TGLHeightData* heightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	
__published:
	__property MaxPoolSize;
	__property Glheightdata::TStartPreparingDataEvent OnStartPreparingData = {read=FOnStartPreparingData, write=FOnStartPreparingData};
	__property Glheightdata::TMarkDirtyEvent OnMarkDirtyEvent = {read=FOnMarkDirty, write=FOnMarkDirty};
	__property Glheightdata::TGLHeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=FMaterialLibrary};
	__property bool WholeTilesOnly = {read=FWholeTilesOnly, write=FWholeTilesOnly, nodefault};
	__property int TileSize = {read=FTileSize, write=FTileSize, nodefault};
	__property int TilesPerTexture = {read=FTilesPerTexture, write=FTilesPerTexture, nodefault};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int XLeft, int YTop, int xRight, int yBottom){ Glheightdata::TGLHeightDataSource::MarkDirty(XLeft, YTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(){ Glheightdata::TGLHeightDataSource::MarkDirty(); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexturedhds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTUREDHDS)
using namespace Gltexturedhds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexturedhdsHPP
