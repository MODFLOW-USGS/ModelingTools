// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshLines.pas' rev: 36.00 (Windows)

#ifndef GlmeshlinesHPP
#define GlmeshlinesHPP

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
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLObjects.hpp>
#include <GLTexture.hpp>
#include <GLCoordinates.hpp>
#include <GLContext.hpp>
#include <GLMaterial.hpp>
#include <GLColor.hpp>
#include <GLState.hpp>
#include <GLNodes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLSpline.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmeshlines
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLineNode;
class DELPHICLASS TLineNodes;
class DELPHICLASS TLineItem;
class DELPHICLASS TLineCollection;
class DELPHICLASS TLightmapBounds;
class DELPHICLASS TGLMeshLines;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLineNode : public Glnodes::TGLNode
{
	typedef Glnodes::TGLNode inherited;
	
private:
	void *FData;
	
public:
	__fastcall virtual TLineNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TLineNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property void * Data = {read=FData, write=FData};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLineNodes : public Glnodes::TGLNodes
{
	typedef Glnodes::TGLNodes inherited;
	
public:
	__fastcall TLineNodes(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TLineNodes();
	virtual void __fastcall NotifyChange();
	int __fastcall IndexOf(TLineNode* LineNode);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLineItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	float FBreakAngle;
	int FDivision;
	TLineNodes* FNodes;
	Globjects::TGLLineSplineMode FSplineMode;
	float FTextureLength;
	float FWidth;
	bool FTextureCorrection;
	bool FHide;
	void *FData;
	void __fastcall SetHide(const bool Value);
	void __fastcall SetTextureCorrection(const bool Value);
	void __fastcall SetBreakAngle(const float Value);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetNodes(TLineNodes* const Value);
	void __fastcall SetSplineMode(const Globjects::TGLLineSplineMode Value);
	void __fastcall SetTextureLength(const float Value);
	void __fastcall SetWidth(const float Value);
	
protected:
	virtual void __fastcall DoChanged();
	
public:
	__property void * Data = {read=FData, write=FData};
	
__published:
	__fastcall virtual TLineItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TLineItem();
	__property bool Hide = {read=FHide, write=SetHide, nodefault};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property bool TextureCorrection = {read=FTextureCorrection, write=SetTextureCorrection, nodefault};
	__property float BreakAngle = {read=FBreakAngle, write=SetBreakAngle};
	__property int Division = {read=FDivision, write=SetDivision, nodefault};
	__property TLineNodes* Nodes = {read=FNodes, write=SetNodes};
	__property Globjects::TGLLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, nodefault};
	__property float TextureLength = {read=FTextureLength, write=SetTextureLength};
	__property float Width = {read=FWidth, write=SetWidth};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLineCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TLineItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TLineItem* const Val);
	TLineItem* __fastcall GetItems(int Index);
	
public:
	HIDESBASE TLineItem* __fastcall Add()/* overload */;
	HIDESBASE TLineItem* __fastcall Add(System::UnicodeString Name)/* overload */;
	__property TLineItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TLineCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TLineCollection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TLightmapBounds : public Glcoordinates::TGLCustomCoordinates
{
	typedef Glcoordinates::TGLCustomCoordinates inherited;
	
private:
	Opengltokens::TGLfloat __fastcall GetLeft();
	Opengltokens::TGLfloat __fastcall GetTop();
	Opengltokens::TGLfloat __fastcall GetRight();
	Opengltokens::TGLfloat __fastcall GetBottom();
	Opengltokens::TGLfloat __fastcall GetWidth();
	Opengltokens::TGLfloat __fastcall GetHeight();
	void __fastcall SetLeft(const Opengltokens::TGLfloat value);
	void __fastcall SetTop(const Opengltokens::TGLfloat value);
	void __fastcall SetRight(const Opengltokens::TGLfloat value);
	void __fastcall SetBottom(const Opengltokens::TGLfloat value);
	
__published:
	__property Opengltokens::TGLfloat Left = {read=GetLeft, write=SetLeft, stored=false};
	__property Opengltokens::TGLfloat Top = {read=GetTop, write=SetTop, stored=false};
	__property Opengltokens::TGLfloat Right = {read=GetRight, write=SetRight, stored=false};
	__property Opengltokens::TGLfloat Bottom = {read=GetBottom, write=SetBottom, stored=false};
	__property Opengltokens::TGLfloat Width = {read=GetWidth};
	__property Opengltokens::TGLfloat Height = {read=GetHeight};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TLightmapBounds(System::Classes::TPersistent* AOwner, const Glvectorgeometry::TVector &AValue, const Glcoordinates::TGLCoordinatesStyle AStyle) : Glcoordinates::TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TLightmapBounds() { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TLightmapBounds(System::Classes::TPersistent* AOwner) : Glcoordinates::TGLCustomCoordinates(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLMeshLines : public Glvectorfileobjects::TGLFreeForm
{
	typedef Glvectorfileobjects::TGLFreeForm inherited;
	
private:
	TLineCollection* FLines;
	Glvectorfileobjects::TMeshObject* FMesh;
	TLightmapBounds* FLightmapBounds;
	int FLightmapIndex;
	System::UnicodeString FLightmapMaterialName;
	Glvectorfileobjects::TFGVertexIndexList* FFaceGroup;
	int FIndex;
	bool FNoZWrite;
	bool FShowNodes;
	int FUpdating;
	TLineItem* FSelectedLineItem;
	TLineNode* FSelectedNode;
	TLineNode* FNode1;
	TLineNode* FNode2;
	bool __fastcall GetUpdating();
	bool __fastcall PointNearLine(TLineItem* const LineItem, const float X, const float Z, float Tolerance = 1.000000E+00f);
	bool __fastcall PointNearSegment(TLineNode* const StartNode, TLineNode* const EndNode, const float X, const float Z, float LineWidth, float Tolerance = 1.000000E+00f);
	void __fastcall StitchStrips(Glvectorlists::TIntegerList* idx);
	void __fastcall AddStitchMarker(Glvectorlists::TIntegerList* idx);
	void __fastcall SetShowNodes(const bool Value);
	void __fastcall SetNoZWrite(const bool Value);
	void __fastcall SetLightmapIndex(const int value);
	void __fastcall SetLightmapMaterialName(const System::UnicodeString value);
	void __fastcall SetLightmapBounds(TLightmapBounds* const value);
	void __fastcall DoChanged();
	void __fastcall AddIndex();
	void __fastcall AddVertices(const Glvectorgeometry::TAffineVector &Up, const Glvectorgeometry::TAffineVector &Inner, const Glvectorgeometry::TAffineVector &Outer, float S, float Correction, bool UseDegenerate, TLineItem* LineItem);
	void __fastcall BuildLineItem(TLineItem* LineItem);
	void __fastcall BuildGeometry();
	void __fastcall DrawNode(Glrendercontextinfo::TGLRenderContextInfo &rci, TLineNode* Node, float LineWidth);
	void __fastcall DrawCircle(float Radius);
	TLineNode* __fastcall SelectNode(TLineItem* LineItem, float X, float Z);
	
protected:
	virtual void __fastcall Loaded();
	
public:
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	void __fastcall Clear();
	TLineItem* __fastcall SelectLineItem(const float X, const float Z, float Tolerance = 1.000000E+00f)/* overload */;
	TLineItem* __fastcall SelectLineItem(TLineItem* LineItem)/* overload */;
	TLineItem* __fastcall SelectLineItem(TLineNode* LineNode)/* overload */;
	void __fastcall DeselectLineItem();
	void __fastcall DeselectLineNode();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property TLineItem* SelectedLineItem = {read=FSelectedLineItem};
	__property TLineNode* SelectedNode = {read=FSelectedNode};
	__property TLineNode* Node1 = {read=FNode1};
	__property TLineNode* Node2 = {read=FNode2};
	
__published:
	__fastcall virtual TGLMeshLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMeshLines();
	__property bool Updating = {read=GetUpdating, nodefault};
	__property TLineCollection* Lines = {read=FLines};
	__property Material;
	__property TLightmapBounds* LightmapBounds = {read=FLightmapBounds, write=SetLightmapBounds};
	__property int LightmapIndex = {read=FLightmapIndex, write=SetLightmapIndex, nodefault};
	__property System::UnicodeString LightmapMaterialName = {read=FLightmapMaterialName, write=SetLightmapMaterialName};
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property bool ShowNodes = {read=FShowNodes, write=SetShowNodes, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMeshLines(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLFreeForm(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmeshlines */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHLINES)
using namespace Glmeshlines;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshlinesHPP
