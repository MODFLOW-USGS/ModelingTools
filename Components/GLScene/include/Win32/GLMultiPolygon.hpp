// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiPolygon.pas' rev: 36.00 (Windows)

#ifndef GlmultipolygonHPP
#define GlmultipolygonHPP

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
#include <OpenGLAdapter.hpp>
#include <GLSpline.hpp>
#include <XOpenGL.hpp>
#include <GLContext.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLPersistentClasses.hpp>
#include <GLScene.hpp>
#include <GLObjects.hpp>
#include <GLGeomObjects.hpp>
#include <GLNodes.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmultipolygon
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLContourNodes;
class DELPHICLASS TGLContour;
class DELPHICLASS TGLContours;
class DELPHICLASS TPolygonList;
class DELPHICLASS TMultiPolygonBase;
class DELPHICLASS TGLMultiPolygon;
class DELPHICLASS TVectorPool;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLContourNodes : public Glnodes::TGLNodes
{
	typedef Glnodes::TGLNodes inherited;
	
public:
	virtual void __fastcall NotifyChange();
public:
	/* TGLNodes.Create */ inline __fastcall TGLContourNodes(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass) : Glnodes::TGLNodes(AOwner, AItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLContourNodes() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLContour : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLContourNodes* FNodes;
	int FDivision;
	Globjects::TGLLineSplineMode FSplineMode;
	System::UnicodeString FDescription;
	void __fastcall SetNodes(TGLContourNodes* const Value);
	void __fastcall SetDivision(int Value);
	void __fastcall SetSplineMode(const Globjects::TGLLineSplineMode Value);
	void __fastcall SetDescription(const System::UnicodeString Value);
	
protected:
	void __fastcall CreateNodes();
	void __fastcall NodesChanged(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLContour(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLContour();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
	__property TGLContourNodes* Nodes = {read=FNodes, write=SetNodes};
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property Globjects::TGLLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
};

#pragma pack(pop)

typedef System::TMetaClass* TGLContourClass;

class PASCALIMPLEMENTATION TGLContours : public Glbaseclasses::TGLNotifyCollection
{
	typedef Glbaseclasses::TGLNotifyCollection inherited;
	
public:
	TGLContour* operator[](int index) { return this->Items[index]; }
	
private:
	TGLContour* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TGLContour* const Value);
	
public:
	__fastcall TGLContours(System::Classes::TComponent* AOwner)/* overload */;
	HIDESBASE TGLContour* __fastcall Add();
	HIDESBASE TGLContour* __fastcall FindItemID(int ID);
	__property TGLContour* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall GetExtents(Glvectorgeometry::TAffineVector &min, Glvectorgeometry::TAffineVector &max);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLContours() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TPolygonList : public Glpersistentclasses::TPersistentObjectList
{
	typedef Glpersistentclasses::TPersistentObjectList inherited;
	
private:
	Glvectorlists::TAffineVectorList* FAktList;
	Glvectorlists::TAffineVectorList* __fastcall GetList(int I);
	
public:
	HIDESBASE void __fastcall Add();
	__property Glvectorlists::TAffineVectorList* AktList = {read=FAktList};
	__property Glvectorlists::TAffineVectorList* List[int I] = {read=GetList};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TPolygonList() : Glpersistentclasses::TPersistentObjectList() { }
	/* TPersistentObjectList.Destroy */ inline __fastcall virtual ~TPolygonList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TPolygonList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TMultiPolygonBase : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TGLContours* FContours;
	TPolygonList* FOutline;
	Glvectorgeometry::TAffineVector FContoursNormal;
	Glvectorgeometry::TVector FAxisAlignedDimensionsCache;
	void __fastcall SetContours(TGLContours* const Value);
	TGLContourNodes* __fastcall GetPath(int i);
	void __fastcall SetPath(int i, TGLContourNodes* const value);
	TPolygonList* __fastcall GetOutline();
	void __fastcall SetContoursNormal(const Glvectorgeometry::TAffineVector &Value);
	
protected:
	void __fastcall RenderTesselatedPolygon(bool textured, Glvectorgeometry::PAffineVector normal, bool invertNormals);
	void __fastcall RetrieveOutline(TPolygonList* List);
	virtual void __fastcall ContourChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TMultiPolygonBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiPolygonBase();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AddNode(const int i, Glcoordinates::TGLCoordinates* const coords)/* overload */;
	void __fastcall AddNode(const int i, const Opengltokens::TGLfloat X, const Opengltokens::TGLfloat Y, const Opengltokens::TGLfloat Z)/* overload */;
	void __fastcall AddNode(const int i, const Glvectorgeometry::TVector &value)/* overload */;
	void __fastcall AddNode(const int i, const Glvectorgeometry::TAffineVector &value)/* overload */;
	__property TGLContourNodes* Path[int i] = {read=GetPath, write=SetPath};
	__property TPolygonList* Outline = {read=GetOutline};
	__property Glvectorgeometry::TAffineVector ContoursNormal = {read=FContoursNormal, write=SetContoursNormal};
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	
__published:
	__property TGLContours* Contours = {read=FContours, write=SetContours};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TMultiPolygonBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLMultiPolygon : public TMultiPolygonBase
{
	typedef TMultiPolygonBase inherited;
	
private:
	Glgeomobjects::TPolygonParts FParts;
	
protected:
	void __fastcall SetParts(const Glgeomobjects::TPolygonParts value);
	
public:
	__fastcall virtual TGLMultiPolygon(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property Glgeomobjects::TPolygonParts Parts = {read=FParts, write=SetParts, default=3};
public:
	/* TMultiPolygonBase.Destroy */ inline __fastcall virtual ~TGLMultiPolygon() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMultiPolygon(Glscene::TGLBaseSceneObject* aParentOwner) : TMultiPolygonBase(aParentOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TVectorPool : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	int FEntrySize;
	int FPageSize;
	int FArrSize;
	int FUsedEntries;
	Glvectorgeometry::PByteArray FAktArray;
	void __fastcall CreatePage();
	
public:
	__fastcall TVectorPool(int APageSize, int AEntrySize);
	__fastcall virtual ~TVectorPool();
	void __fastcall GetNewVector(void * &P);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultipolygon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIPOLYGON)
using namespace Glmultipolygon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultipolygonHPP
