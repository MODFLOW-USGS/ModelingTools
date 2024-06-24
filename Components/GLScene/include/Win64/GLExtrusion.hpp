// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glextrusion.pas' rev: 36.00 (Windows)

#ifndef GlextrusionHPP
#define GlextrusionHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.Math.hpp>
#include <Opengltokens.hpp>
#include <Glcontext.hpp>
#include <Globjects.hpp>
#include <Glscene.hpp>
#include <Glmultipolygon.hpp>
#include <Glcolor.hpp>
#include <Glvectorgeometry.hpp>
#include <Glrendercontextinfo.hpp>
#include <Glnodes.hpp>
#include <Glstate.hpp>
#include <Glvectortypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glextrusion
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLRevolutionSolid;
class DELPHICLASS TGLExtrusionSolid;
class DELPHICLASS TGLPipeNode;
class DELPHICLASS TGLPipeNodes;
class DELPHICLASS TGLPipe;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TExtrusionSolidPart : unsigned char { espOutside, espInside, espStartPolygon, espStopPolygon };

typedef System::Set<TExtrusionSolidPart, TExtrusionSolidPart::espOutside, TExtrusionSolidPart::espStopPolygon> TExtrusionSolidParts;

enum DECLSPEC_DENUM TRevolutionSolidPart : unsigned char { rspOutside, rspInside, rspStartPolygon, rspStopPolygon };

typedef System::Set<TRevolutionSolidPart, TRevolutionSolidPart::rspOutside, TRevolutionSolidPart::rspStopPolygon> TRevolutionSolidParts;

class PASCALIMPLEMENTATION TGLRevolutionSolid : public Globjects::TGLPolygonBase
{
	typedef Globjects::TGLPolygonBase inherited;
	
private:
	int FSlices;
	float FStartAngle;
	float FStopAngle;
	Globjects::TNormalSmoothing FNormals;
	float FYOffsetPerTurn;
	int FTriangleCount;
	Glscene::TNormalDirection FNormalDirection;
	TRevolutionSolidParts FParts;
	Glvectorgeometry::TVector FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetStartAngle(const float val);
	void __fastcall SetStopAngle(const float val);
	bool __fastcall StoreStopAngle();
	void __fastcall SetSlices(const int val);
	void __fastcall SetNormals(const Globjects::TNormalSmoothing val);
	void __fastcall SetYOffsetPerTurn(const float val);
	void __fastcall SetNormalDirection(const Glscene::TNormalDirection val);
	void __fastcall SetParts(const TRevolutionSolidParts val);
	
public:
	__fastcall virtual TGLRevolutionSolid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLRevolutionSolid();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	
__published:
	__property TRevolutionSolidParts Parts = {read=FParts, write=SetParts, default=1};
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle, stored=StoreStopAngle};
	__property float YOffsetPerTurn = {read=FYOffsetPerTurn, write=SetYOffsetPerTurn};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property Globjects::TNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=0};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLRevolutionSolid(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLPolygonBase(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLExtrusionSolid : public Glmultipolygon::TMultiPolygonBase
{
	typedef Glmultipolygon::TMultiPolygonBase inherited;
	
private:
	int FStacks;
	Globjects::TNormalSmoothing FNormals;
	int FTriangleCount;
	Glscene::TNormalDirection FNormalDirection;
	TExtrusionSolidParts FParts;
	Opengltokens::TGLfloat FHeight;
	float FMinSmoothAngle;
	float FMinSmoothAngleCos;
	Glvectorgeometry::TVector FAxisAlignedDimensionsCache;
	void __fastcall SetHeight(const Opengltokens::TGLfloat Value);
	void __fastcall SetMinSmoothAngle(const float Value);
	
protected:
	void __fastcall SetStacks(const int val);
	void __fastcall SetNormals(const Globjects::TNormalSmoothing val);
	void __fastcall SetNormalDirection(const Glscene::TNormalDirection val);
	void __fastcall SetParts(const TExtrusionSolidParts val);
	
public:
	__fastcall virtual TGLExtrusionSolid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLExtrusionSolid();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	
__published:
	__property TExtrusionSolidParts Parts = {read=FParts, write=SetParts, default=1};
	__property Opengltokens::TGLfloat Height = {read=FHeight, write=SetHeight};
	__property int Stacks = {read=FStacks, write=SetStacks, default=1};
	__property Globjects::TNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=0};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
	__property float MinSmoothAngle = {read=FMinSmoothAngle, write=SetMinSmoothAngle};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLExtrusionSolid(Glscene::TGLBaseSceneObject* aParentOwner) : Glmultipolygon::TMultiPolygonBase(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLPipeNode : public Glnodes::TGLNode
{
	typedef Glnodes::TGLNode inherited;
	
private:
	float FRadiusFactor;
	Glcolor::TGLColor* FColor;
	float FTexCoordT;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetRadiusFactor(const float val);
	bool __fastcall StoreRadiusFactor();
	void __fastcall SetColor(Glcolor::TGLColor* const val);
	void __fastcall ColorChanged(System::TObject* sender);
	bool __fastcall StoreTexCoordT();
	
public:
	__fastcall virtual TGLPipeNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLPipeNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RadiusFactor = {read=FRadiusFactor, write=SetRadiusFactor, stored=StoreRadiusFactor};
	__property Glcolor::TGLColor* Color = {read=FColor, write=SetColor};
	__property float TexCoordT = {read=FTexCoordT, write=FTexCoordT, stored=StoreTexCoordT};
};


class PASCALIMPLEMENTATION TGLPipeNodes : public Globjects::TGLLinesNodes
{
	typedef Globjects::TGLLinesNodes inherited;
	
public:
	TGLPipeNode* operator[](int index) { return this->Items[index]; }
	
protected:
	HIDESBASE void __fastcall SetItems(int index, TGLPipeNode* const val);
	HIDESBASE TGLPipeNode* __fastcall GetItems(int index);
	
public:
	__fastcall TGLPipeNodes(System::Classes::TComponent* AOwner);
	HIDESBASE TGLPipeNode* __fastcall Add();
	HIDESBASE TGLPipeNode* __fastcall FindItemID(int ID);
	__property TGLPipeNode* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLPipeNodes() { }
	
};


enum DECLSPEC_DENUM TPipePart : unsigned char { ppOutside, ppInside, ppStartDisk, ppStopDisk };

typedef System::Set<TPipePart, TPipePart::ppOutside, TPipePart::ppStopDisk> TPipeParts;

enum DECLSPEC_DENUM TPipeNodesColorMode : unsigned char { pncmNone, pncmEmission, pncmAmbient, pncmDiffuse, pncmAmbientAndDiffuse };

enum DECLSPEC_DENUM TPipeTexCoordMode : unsigned char { ptcmDefault, ptcmManual };

enum DECLSPEC_DENUM TPipeNormalMode : unsigned char { pnmDefault, pnmAdvanced };

class PASCALIMPLEMENTATION TGLPipe : public Globjects::TGLPolygonBase
{
	typedef Globjects::TGLPolygonBase inherited;
	
private:
	int FSlices;
	TPipeParts FParts;
	int FTriangleCount;
	float FRadius;
	TPipeNodesColorMode FNodesColorMode;
	TPipeTexCoordMode FTextCoordMode;
	float FTextCoordTileS;
	float FTextCoordTileT;
	TPipeNormalMode FNormalMode;
	float FNormalSmoothAngle;
	
protected:
	DYNAMIC void __fastcall CreateNodes();
	void __fastcall SetSlices(const int val);
	void __fastcall SetParts(const TPipeParts val);
	void __fastcall SetRadius(const float val);
	bool __fastcall StoreRadius();
	void __fastcall SetNodesColorMode(const TPipeNodesColorMode val);
	void __fastcall SetTextCoordMode(const TPipeTexCoordMode val);
	void __fastcall SetTextCoordTileS(const float val);
	void __fastcall SetTextCoordTileT(const float val);
	bool __fastcall StoreTextCoordTileS();
	bool __fastcall StoreTextCoordTileT();
	void __fastcall SetNormalMode(const TPipeNormalMode val);
	void __fastcall SetNormalSmoothAngle(const float val);
	
public:
	__fastcall virtual TGLPipe(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPipe();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	
__published:
	__property TPipeParts Parts = {read=FParts, write=SetParts, default=1};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property TPipeNodesColorMode NodesColorMode = {read=FNodesColorMode, write=SetNodesColorMode, default=0};
	__property TPipeTexCoordMode TexCoordMode = {read=FTextCoordMode, write=SetTextCoordMode, default=0};
	__property float TexCoordTileS = {read=FTextCoordTileS, write=SetTextCoordTileS, stored=StoreTextCoordTileS};
	__property float TexCoordTileT = {read=FTextCoordTileT, write=SetTextCoordTileT, stored=StoreTextCoordTileT};
	__property TPipeNormalMode NormalMode = {read=FNormalMode, write=SetNormalMode, default=0};
	__property float NormalSmoothAngle = {read=FNormalSmoothAngle, write=SetNormalSmoothAngle};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPipe(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLPolygonBase(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glextrusion */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLEXTRUSION)
using namespace Glextrusion;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlextrusionHPP
