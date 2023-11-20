// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLObjects.pas' rev: 35.00 (Windows)

#ifndef GlobjectsHPP
#define GlobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLScene.hpp>
#include <OpenGLAdapter.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorLists.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLContext.hpp>
#include <GLSilhouette.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLPersistentClasses.hpp>
#include <GLBaseClasses.hpp>
#include <GLNodes.hpp>
#include <GLCoordinates.hpp>
#include <GLSpline.hpp>
#include <XOpenGL.hpp>
#include <GLState.hpp>

//-- user supplied -----------------------------------------------------------

namespace Globjects
{
//-- forward type declarations -----------------------------------------------
struct TVertexRec;
class DELPHICLASS TGLDummyCube;
class DELPHICLASS TGLPlane;
class DELPHICLASS TGLSprite;
class DELPHICLASS TGLPointParameters;
class DELPHICLASS TGLPoints;
class DELPHICLASS TGLLinesNode;
class DELPHICLASS TGLLinesNodes;
class DELPHICLASS TGLLineBase;
class DELPHICLASS TGLNodedLines;
class DELPHICLASS TGLLines;
class DELPHICLASS TGLCube;
class DELPHICLASS TGLQuadricObject;
class DELPHICLASS TGLSphere;
class DELPHICLASS TGLPolygonBase;
class DELPHICLASS TGLSuperellipsoid;
//-- type declarations -------------------------------------------------------
typedef bool __fastcall (__closure *TGLVisibilityDeterminationEvent)(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);

typedef TVertexRec *PVertexRec;

struct DECLSPEC_DRECORD TVertexRec
{
public:
	Glvectortypes::TVector3f Position;
	Glvectortypes::TVector3f Normal;
	Glvectortypes::TVector3f Binormal;
	Glvectortypes::TVector3f Tangent;
	Glvectortypes::TVector2f TexCoord;
};


class PASCALIMPLEMENTATION TGLDummyCube : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	float FCubeSize;
	Glcolor::TGLColor* FEdgeColor;
	bool FVisibleAtRunTime;
	bool FAmalgamate;
	Glcontext::TGLListHandle* FGroupList;
	TGLVisibilityDeterminationEvent FOnVisibilityDetermination;
	
protected:
	void __fastcall SetCubeSize(const float val);
	void __fastcall SetEdgeColor(Glcolor::TGLColor* const val);
	void __fastcall SetVisibleAtRunTime(const bool val);
	void __fastcall SetAmalgamate(const bool val);
	
public:
	__fastcall virtual TGLDummyCube(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDummyCube();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall StructureChanged();
	virtual Glvectortypes::TVector4f __fastcall BarycenterAbsolutePosition();
	
__published:
	__property float CubeSize = {read=FCubeSize, write=SetCubeSize};
	__property Glcolor::TGLColor* EdgeColor = {read=FEdgeColor, write=SetEdgeColor};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property bool Amalgamate = {read=FAmalgamate, write=SetAmalgamate, default=0};
	__property CamInvarianceMode = {default=0};
	__property TGLVisibilityDeterminationEvent OnVisibilityDetermination = {read=FOnVisibilityDetermination, write=FOnVisibilityDetermination};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDummyCube(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLPlaneStyle : unsigned char { psSingleQuad, psTileTexture };

typedef System::Set<TGLPlaneStyle, TGLPlaneStyle::psSingleQuad, TGLPlaneStyle::psTileTexture> TGLPlaneStyles;

class PASCALIMPLEMENTATION TGLPlane : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<TVertexRec> _TGLPlane__1;
	
	typedef System::DynamicArray<System::DynamicArray<TVertexRec> > _TGLPlane__2;
	
	
private:
	float FXOffset;
	float FYOffset;
	float FXScope;
	float FYScope;
	float FWidth;
	float FHeight;
	unsigned FXTiles;
	unsigned FYTiles;
	TGLPlaneStyles FStyle;
	_TGLPlane__2 FMesh;
	
protected:
	void __fastcall SetHeight(const float aValue);
	void __fastcall SetWidth(const float aValue);
	void __fastcall SetXOffset(const float Value);
	void __fastcall SetXScope(const float Value);
	bool __fastcall StoreXScope();
	void __fastcall SetXTiles(const unsigned Value);
	void __fastcall SetYOffset(const float Value);
	void __fastcall SetYScope(const float Value);
	bool __fastcall StoreYScope();
	void __fastcall SetYTiles(const unsigned Value);
	void __fastcall SetStyle(const TGLPlaneStyles val);
	
public:
	__fastcall virtual TGLPlane(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	System::Types::TRect __fastcall ScreenRect(Glscene::TGLSceneBuffer* aBuffer);
	float __fastcall PointDistance(const Glvectortypes::TVector4f &aPoint);
	
__published:
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Width = {read=FWidth, write=SetWidth};
	__property float XOffset = {read=FXOffset, write=SetXOffset};
	__property float XScope = {read=FXScope, write=SetXScope, stored=StoreXScope};
	__property unsigned XTiles = {read=FXTiles, write=SetXTiles, default=1};
	__property float YOffset = {read=FYOffset, write=SetYOffset};
	__property float YScope = {read=FYScope, write=SetYScope, stored=StoreYScope};
	__property unsigned YTiles = {read=FYTiles, write=SetYTiles, default=1};
	__property TGLPlaneStyles Style = {read=FStyle, write=SetStyle, default=3};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLPlane() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPlane(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLSprite : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	float FWidth;
	float FHeight;
	float FRotation;
	float FAlphaChannel;
	bool FMirrorU;
	bool FMirrorV;
	
protected:
	void __fastcall SetWidth(const float val);
	void __fastcall SetHeight(const float val);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlphaChannel(const float val);
	bool __fastcall StoreAlphaChannel();
	void __fastcall SetMirrorU(const bool val);
	void __fastcall SetMirrorV(const bool val);
	
public:
	__fastcall virtual TGLSprite(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	void __fastcall SetSize(const float Width, const float Height);
	void __fastcall SetSquareSize(const float Size);
	
__published:
	__property float Width = {read=FWidth, write=SetWidth};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property float AlphaChannel = {read=FAlphaChannel, write=SetAlphaChannel, stored=StoreAlphaChannel};
	__property bool MirrorU = {read=FMirrorU, write=SetMirrorU, default=0};
	__property bool MirrorV = {read=FMirrorV, write=SetMirrorV, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSprite() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSprite(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLPointStyle : unsigned char { psSquare, psRound, psSmooth, psSmoothAdditive, psSquareAdditive };

class PASCALIMPLEMENTATION TGLPointParameters : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FEnabled;
	float FMinSize;
	float FMaxSize;
	float FFadeTresholdSize;
	Glcoordinates::TGLCoordinates3* FDistanceAttenuation;
	
protected:
	void __fastcall SetEnabled(const bool val);
	void __fastcall SetMinSize(const float val);
	void __fastcall SetMaxSize(const float val);
	void __fastcall SetFadeTresholdSize(const float val);
	void __fastcall SetDistanceAttenuation(Glcoordinates::TGLCoordinates3* const val);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall virtual TGLPointParameters(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPointParameters();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Apply();
	void __fastcall UnApply();
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property float MinSize = {read=FMinSize, write=SetMinSize, stored=false};
	__property float MaxSize = {read=FMaxSize, write=SetMaxSize, stored=false};
	__property float FadeTresholdSize = {read=FFadeTresholdSize, write=SetFadeTresholdSize, stored=false};
	__property Glcoordinates::TGLCoordinates3* DistanceAttenuation = {read=FDistanceAttenuation, write=SetDistanceAttenuation};
};


class PASCALIMPLEMENTATION TGLPoints : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Glvectorlists::TAffineVectorList* FPositions;
	Glvectorlists::TVectorList* FColors;
	float FSize;
	TGLPointStyle FStyle;
	TGLPointParameters* FPointParameters;
	bool FStatic;
	bool FNoZWrite;
	
protected:
	bool __fastcall StoreSize();
	void __fastcall SetNoZWrite(const bool val);
	void __fastcall SetStatic(const bool val);
	void __fastcall SetSize(const float val);
	void __fastcall SetPositions(Glvectorlists::TAffineVectorList* const val);
	void __fastcall SetColors(Glvectorlists::TVectorList* const val);
	void __fastcall SetStyle(const TGLPointStyle val);
	void __fastcall SetPointParameters(TGLPointParameters* const val);
	
public:
	__fastcall virtual TGLPoints(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPoints();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property Glvectorlists::TAffineVectorList* Positions = {read=FPositions, write=SetPositions};
	__property Glvectorlists::TVectorList* Colors = {read=FColors, write=SetColors};
	
__published:
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property bool Static = {read=FStatic, write=SetStatic, nodefault};
	__property float Size = {read=FSize, write=SetSize, stored=StoreSize};
	__property TGLPointStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property TGLPointParameters* PointParameters = {read=FPointParameters, write=SetPointParameters};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPoints(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLLineNodesAspect : unsigned char { lnaInvisible, lnaAxes, lnaCube };

enum DECLSPEC_DENUM TGLLineSplineMode : unsigned char { lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve, lsmSegments, lsmLoop };

class PASCALIMPLEMENTATION TGLLinesNode : public Glnodes::TGLNode
{
	typedef Glnodes::TGLNode inherited;
	
private:
	Glcolor::TGLColor* FColor;
	
protected:
	void __fastcall SetColor(Glcolor::TGLColor* const val);
	void __fastcall OnColorChange(System::TObject* Sender);
	bool __fastcall StoreColor();
	
public:
	__fastcall virtual TGLLinesNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLLinesNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glcolor::TGLColor* Color = {read=FColor, write=SetColor, stored=StoreColor};
};


class PASCALIMPLEMENTATION TGLLinesNodes : public Glnodes::TGLNodes
{
	typedef Glnodes::TGLNodes inherited;
	
public:
	__fastcall TGLLinesNodes(System::Classes::TComponent* AOwner)/* overload */;
	virtual void __fastcall NotifyChange();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLinesNodes() { }
	
};


class PASCALIMPLEMENTATION TGLLineBase : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Glcolor::TGLColor* FLineColor;
	System::Word FLinePattern;
	float FLineWidth;
	bool FAntiAliased;
	
protected:
	void __fastcall SetLineColor(Glcolor::TGLColor* const Value);
	void __fastcall SetLinePattern(const System::Word Value);
	void __fastcall SetLineWidth(const float val);
	bool __fastcall StoreLineWidth();
	void __fastcall SetAntiAliased(const bool val);
	void __fastcall SetupLineStyle(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLLineBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLineBase();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property bool AntiAliased = {read=FAntiAliased, write=SetAntiAliased, default=0};
	__property Glcolor::TGLColor* LineColor = {read=FLineColor, write=SetLineColor};
	__property System::Word LinePattern = {read=FLinePattern, write=SetLinePattern, default=65535};
	__property float LineWidth = {read=FLineWidth, write=SetLineWidth, stored=StoreLineWidth};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLineBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLNodedLines : public TGLLineBase
{
	typedef TGLLineBase inherited;
	
private:
	TGLLinesNodes* FNodes;
	TGLLineNodesAspect FNodesAspect;
	Glcolor::TGLColor* FNodeColor;
	float FNodeSize;
	Glvectortypes::TVector4f FOldNodeColor;
	
protected:
	void __fastcall SetNodesAspect(const TGLLineNodesAspect Value);
	void __fastcall SetNodeColor(Glcolor::TGLColor* const Value);
	void __fastcall OnNodeColorChanged(System::TObject* Sender);
	void __fastcall SetNodes(TGLLinesNodes* const aNodes);
	void __fastcall SetNodeSize(const float val);
	bool __fastcall StoreNodeSize();
	void __fastcall DrawNode(Glrendercontextinfo::TGLRenderContextInfo &rci, float X, float Y, float Z, Glcolor::TGLColor* Color);
	
public:
	__fastcall virtual TGLNodedLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNodedLines();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	void __fastcall AddNode(Glcoordinates::TGLCoordinates3* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector3f &Value)/* overload */;
	
__published:
	__property Glcolor::TGLColor* NodeColor = {read=FNodeColor, write=SetNodeColor};
	__property TGLLinesNodes* Nodes = {read=FNodes, write=SetNodes};
	__property TGLLineNodesAspect NodesAspect = {read=FNodesAspect, write=SetNodesAspect, default=1};
	__property float NodeSize = {read=FNodeSize, write=SetNodeSize, stored=StoreNodeSize};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLNodedLines(Glscene::TGLBaseSceneObject* aParentOwner) : TGLLineBase(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLLinesOption : unsigned char { loUseNodeColorForLines, loColorLogicXor };

typedef System::Set<TGLLinesOption, TGLLinesOption::loUseNodeColorForLines, TGLLinesOption::loColorLogicXor> TGLLinesOptions;

class PASCALIMPLEMENTATION TGLLines : public TGLNodedLines
{
	typedef TGLNodedLines inherited;
	
private:
	int FDivision;
	TGLLineSplineMode FSplineMode;
	TGLLinesOptions FOptions;
	int FNURBSOrder;
	float FNURBSTolerance;
	Glvectorlists::TSingleList* FNURBSKnots;
	
protected:
	void __fastcall SetSplineMode(const TGLLineSplineMode val);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetOptions(const TGLLinesOptions val);
	void __fastcall SetNURBSOrder(const int val);
	void __fastcall SetNURBSTolerance(const float val);
	
public:
	__fastcall virtual TGLLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLines();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property Glvectorlists::TSingleList* NURBSKnots = {read=FNURBSKnots};
	__property int NURBSOrder = {read=FNURBSOrder, write=SetNURBSOrder, nodefault};
	__property float NURBSTolerance = {read=FNURBSTolerance, write=SetNURBSTolerance};
	
__published:
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property TGLLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
	__property TGLLinesOptions Options = {read=FOptions, write=SetOptions, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLines(Glscene::TGLBaseSceneObject* aParentOwner) : TGLNodedLines(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TCubePart : unsigned char { cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight };

typedef System::Set<TCubePart, TCubePart::cpTop, TCubePart::cpRight> TCubeParts;

class PASCALIMPLEMENTATION TGLCube : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	Glvectortypes::TVector3f FCubeSize;
	TCubeParts FParts;
	Glscene::TNormalDirection FNormalDirection;
	float __fastcall GetCubeWHD(const int Index);
	void __fastcall SetCubeWHD(int Index, float AValue);
	void __fastcall SetParts(TCubeParts aValue);
	void __fastcall SetNormalDirection(Glscene::TNormalDirection aValue);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall virtual TGLCube(System::Classes::TComponent* AOwner);
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	
__published:
	__property float CubeWidth = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=0};
	__property float CubeHeight = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=1};
	__property float CubeDepth = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=2};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
	__property TCubeParts Parts = {read=FParts, write=SetParts, default=63};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCube() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCube(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TNormalSmoothing : unsigned char { nsFlat, nsSmooth, nsNone };

class PASCALIMPLEMENTATION TGLQuadricObject : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TNormalSmoothing FNormals;
	Glscene::TNormalDirection FNormalDirection;
	
protected:
	void __fastcall SetNormals(TNormalSmoothing aValue);
	void __fastcall SetNormalDirection(Glscene::TNormalDirection aValue);
	void __fastcall SetupQuadricParams(Opengltokens::PGLUQuadric quadric);
	void __fastcall SetNormalQuadricOrientation(Opengltokens::PGLUQuadric quadric);
	void __fastcall SetInvertedQuadricOrientation(Opengltokens::PGLUQuadric quadric);
	
public:
	__fastcall virtual TGLQuadricObject(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=1};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLQuadricObject() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLQuadricObject(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


typedef System::Int8 TAngleLimit1;

typedef System::Word TAngleLimit2;

enum DECLSPEC_DENUM TCapType : unsigned char { ctNone, ctCenter, ctFlat };

class PASCALIMPLEMENTATION TGLSphere : public TGLQuadricObject
{
	typedef TGLQuadricObject inherited;
	
private:
	float FRadius;
	int FSlices;
	int FStacks;
	TAngleLimit1 FTop;
	TAngleLimit1 FBottom;
	TAngleLimit2 FStart;
	TAngleLimit2 FStop;
	TCapType FTopCap;
	TCapType FBottomCap;
	void __fastcall SetBottom(TAngleLimit1 aValue);
	void __fastcall SetBottomCap(TCapType aValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetStart(TAngleLimit2 aValue);
	void __fastcall SetStop(TAngleLimit2 aValue);
	void __fastcall SetStacks(int aValue);
	void __fastcall SetTop(TAngleLimit1 aValue);
	void __fastcall SetTopCap(TCapType aValue);
	
public:
	__fastcall virtual TGLSphere(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TAngleLimit1 Bottom = {read=FBottom, write=SetBottom, default=-90};
	__property TCapType BottomCap = {read=FBottomCap, write=SetBottomCap, default=0};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property int Stacks = {read=FStacks, write=SetStacks, default=16};
	__property TAngleLimit2 Start = {read=FStart, write=SetStart, default=0};
	__property TAngleLimit2 Stop = {read=FStop, write=SetStop, default=360};
	__property TAngleLimit1 Top = {read=FTop, write=SetTop, default=90};
	__property TCapType TopCap = {read=FTopCap, write=SetTopCap, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSphere() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSphere(Glscene::TGLBaseSceneObject* aParentOwner) : TGLQuadricObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLPolygonBase : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	int FDivision;
	TGLLineSplineMode FSplineMode;
	
protected:
	Glnodes::TGLNodes* FNodes;
	DYNAMIC void __fastcall CreateNodes();
	void __fastcall SetSplineMode(const TGLLineSplineMode val);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetNodes(Glnodes::TGLNodes* const aNodes);
	
public:
	__fastcall virtual TGLPolygonBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPolygonBase();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall AddNode(Glcoordinates::TGLCoordinates3* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector3f &Value)/* overload */;
	
__published:
	__property Glnodes::TGLNodes* Nodes = {read=FNodes, write=SetNodes};
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property TGLLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPolygonBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLSuperellipsoid : public TGLQuadricObject
{
	typedef TGLQuadricObject inherited;
	
private:
	float FRadius;
	float FVCurve;
	float FHCurve;
	int FSlices;
	int FStacks;
	TAngleLimit1 FTop;
	TAngleLimit1 FBottom;
	TAngleLimit2 FStart;
	TAngleLimit2 FStop;
	TCapType FTopCap;
	TCapType FBottomCap;
	void __fastcall SetBottom(TAngleLimit1 aValue);
	void __fastcall SetBottomCap(TCapType aValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetVCurve(const float aValue);
	void __fastcall SetHCurve(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetStart(TAngleLimit2 aValue);
	void __fastcall SetStop(TAngleLimit2 aValue);
	void __fastcall SetStacks(int aValue);
	void __fastcall SetTop(TAngleLimit1 aValue);
	void __fastcall SetTopCap(TCapType aValue);
	
public:
	__fastcall virtual TGLSuperellipsoid(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual Glvectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Glvectortypes::TVector4f &rayStart, const Glvectortypes::TVector4f &rayVector, Glvectorgeometry::PVector intersectPoint = (Glvectorgeometry::PVector)(0x0), Glvectorgeometry::PVector intersectNormal = (Glvectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TAngleLimit1 Bottom = {read=FBottom, write=SetBottom, default=-90};
	__property TCapType BottomCap = {read=FBottomCap, write=SetBottomCap, default=0};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float VCurve = {read=FVCurve, write=SetVCurve};
	__property float HCurve = {read=FHCurve, write=SetHCurve};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property int Stacks = {read=FStacks, write=SetStacks, default=16};
	__property TAngleLimit2 Start = {read=FStart, write=SetStart, default=0};
	__property TAngleLimit2 Stop = {read=FStop, write=SetStop, default=360};
	__property TAngleLimit1 Top = {read=FTop, write=SetTop, default=90};
	__property TCapType TopCap = {read=FTopCap, write=SetTopCap, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSuperellipsoid() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSuperellipsoid(Glscene::TGLBaseSceneObject* aParentOwner) : TGLQuadricObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE float cDefaultPointSize;
extern DELPHI_PACKAGE char *TangentAttributeName;
extern DELPHI_PACKAGE char *BinormalAttributeName;
extern DELPHI_PACKAGE void __fastcall CubeWireframeBuildList(Glrendercontextinfo::TGLRenderContextInfo &rci, float Size, bool Stipple, const Glvectortypes::TVector4f &Color);
}	/* namespace Globjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLOBJECTS)
using namespace Globjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlobjectsHPP
