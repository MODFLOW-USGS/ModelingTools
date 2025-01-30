// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletClothify.pas' rev: 36.00 (Windows)

#ifndef GLVerletClothifyHPP
#define GLVerletClothifyHPP

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
#include <GLVectorFileObjects.hpp>
#include <GLVerletTypes.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTexture.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLPersistentClasses.hpp>
#include <GLContext.hpp>
#include <GLSpacePartition.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glverletclothify
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TFace;
class DELPHICLASS TFaceList;
class DELPHICLASS TFaceExtractor;
class DELPHICLASS TEdge;
class DELPHICLASS TEdgeList;
class DELPHICLASS TEdgeDetector;
class DELPHICLASS TMeshObjectVerletNode;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFace : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<int, 3> Vertices;
	Glvectorgeometry::TAffineVector Normal;
	Glvectorfileobjects::TMeshObject* MeshObject;
	bool Active;
	void __fastcall UpdateNormal();
	__fastcall TFace(Glvectorfileobjects::TMeshObject* aMeshObject);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFace() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TFace* operator[](int i) { return this->Items[i]; }
	
private:
	TFace* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TFace* const Value);
	
public:
	__property TFace* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TFaceList() { }
	
public:
	/* TObject.Create */ inline __fastcall TFaceList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFaceList* FFaceList;
	Glvectorfileobjects::TGLBaseMesh* FGLBaseMesh;
	Glverlettypes::TVerletNodeList* FNodeList;
	float FWeldDistance;
	int FEdgeDoublesSkipped;
	void __fastcall SetWeldDistance(const float Value);
	
protected:
	virtual void __fastcall ProcessMeshObject(Glvectorfileobjects::TMeshObject* const MeshObject);
	
public:
	void __fastcall ExtractFacesFromVertexIndexList(Glvectorfileobjects::TFGVertexIndexList* const FaceGroup, Glvectorfileobjects::TMeshObject* const MeshObject);
	__property TFaceList* FaceList = {read=FFaceList};
	virtual void __fastcall Clear();
	virtual void __fastcall ProcessMesh();
	__property float WeldDistance = {read=FWeldDistance, write=SetWeldDistance};
	__property int EdgeDoublesSkipped = {read=FEdgeDoublesSkipped, nodefault};
	__property Glvectorfileobjects::TGLBaseMesh* GLBaseMesh = {read=FGLBaseMesh};
	__property Glverlettypes::TVerletNodeList* NodeList = {read=FNodeList};
	virtual TFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Glvectorfileobjects::TMeshObject* const MeshObject);
	__fastcall virtual TFaceExtractor(Glvectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TFaceExtractor();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdge : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FSolid;
	float FLength;
	Glvectorfileobjects::TMeshObject* FMeshObject;
	TEdgeDetector* FOwner;
	
public:
	System::StaticArray<int, 2> Vertices;
	System::StaticArray<TFace*, 2> Faces;
	void __fastcall Contract();
	__property TEdgeDetector* Owner = {read=FOwner};
	__property Glvectorfileobjects::TMeshObject* MeshObject = {read=FMeshObject, write=FMeshObject};
	__property float Length = {read=FLength, write=FLength};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	void __fastcall UpdateEdgeLength();
	__fastcall TEdge(TEdgeDetector* const AOwner, int AVi0, int AVi1, TFace* AFace0, TFace* AFace1, Glvectorfileobjects::TMeshObject* AMeshObject, bool ASolid);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEdge() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdgeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TEdge* operator[](int i) { return this->Items[i]; }
	
private:
	TEdge* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TEdge* const Value);
	
public:
	__property TEdge* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall SortByLength();
	int __fastcall InsertSorted(TEdge* AEdge);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TEdgeList() { }
	
public:
	/* TObject.Create */ inline __fastcall TEdgeList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdgeDetector : public TFaceExtractor
{
	typedef TFaceExtractor inherited;
	
private:
	TEdgeList* FEdgeList;
	int FCurrentNodeOffset;
	bool FNodesAdded;
	void __fastcall BuildOpposingEdges();
	
protected:
	bool FCalcEdgeLength;
	
public:
	__property TEdgeList* EdgeList = {read=FEdgeList};
	virtual void __fastcall Clear();
	virtual void __fastcall ProcessMesh();
	TEdge* __fastcall AddEdge(const int Vi0, const int Vi1, TFace* const Face, Glvectorfileobjects::TMeshObject* const AMeshObject);
	virtual TFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Glvectorfileobjects::TMeshObject* const MeshObject);
	virtual Glverlettypes::TVerletNode* __fastcall AddNode(Glverlettypes::TGLVerletWorld* const VerletWorld, Glvectorfileobjects::TMeshObject* const MeshObject, const int VertexIndex);
	void __fastcall AddNodes(Glverlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall AddEdgesAsSticks(Glverlettypes::TGLVerletWorld* const VerletWorld, const float Slack);
	void __fastcall AddEdgesAsSprings(Glverlettypes::TGLVerletWorld* const VerletWorld, const float Strength, const float Damping, const float Slack);
	void __fastcall AddEdgesAsSolidEdges(Glverlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall AddOuterEdgesAsSolidEdges(Glverlettypes::TGLVerletWorld* const VerletWorld);
	void __fastcall RenderEdges(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property int CurrentNodeOffset = {read=FCurrentNodeOffset, nodefault};
	__property bool NodesAdded = {read=FNodesAdded, nodefault};
	void __fastcall ReplaceVertexIndex(const int ViRemove, const int ViReplaceWith);
	__fastcall virtual TEdgeDetector(Glvectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TEdgeDetector();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshObjectVerletNode : public Glverlettypes::TVerletNode
{
	typedef Glverlettypes::TVerletNode inherited;
	
private:
	Glvectorfileobjects::TMeshObject* MeshObject;
	Glvectorlists::TIntegerList* VertexIndices;
	
public:
	virtual void __fastcall AfterProgress();
	__fastcall virtual TMeshObjectVerletNode(Glverlettypes::TGLVerletWorld* const aOwner);
	__fastcall virtual ~TMeshObjectVerletNode();
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TMeshObjectVerletNode() : Glverlettypes::TVerletNode() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshObjectVerletNode(Glpersistentclasses::TVirtualReader* reader) : Glverlettypes::TVerletNode(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glverletclothify */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETCLOTHIFY)
using namespace Glverletclothify;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLVerletClothifyHPP
