// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSpacePartition.pas' rev: 36.00 (Windows)

#ifndef GlspacepartitionHPP
#define GlspacepartitionHPP

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
#include <System.Math.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLGeometryBB.hpp>
#include <GLPersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glspacepartition
{
//-- forward type declarations -----------------------------------------------
struct TSPCone;
struct TExtendedFrustum;
class DELPHICLASS TSpacePartitionLeaf;
class DELPHICLASS TSpacePartitionLeafList;
class DELPHICLASS TBaseSpacePartition;
class DELPHICLASS TLeavedSpacePartition;
class DELPHICLASS TSectorNode;
class DELPHICLASS TSectoredSpacePartition;
class DELPHICLASS TSPOctreeNode;
class DELPHICLASS TOctreeSpacePartition;
class DELPHICLASS TSPQuadtreeNode;
class DELPHICLASS TQuadtreeSpacePartition;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TSPCone
{
public:
	Glvectorgeometry::TAffineVector Base;
	Glvectorgeometry::TAffineVector Axis;
	float Angle;
	float Length;
};


struct DECLSPEC_DRECORD TExtendedFrustum
{
public:
	Glvectorgeometry::TFrustum Frustum;
	Glgeometrybb::TBSphere BSphere;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpacePartitionLeaf : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	TBaseSpacePartition* FSpacePartition;
	void __fastcall SetSpacePartition(TBaseSpacePartition* const Value);
	
public:
	void *FPartitionTag;
	Glgeometrybb::TAABB FCachedAABB;
	Glgeometrybb::TBSphere FCachedBSphere;
	virtual void __fastcall Changed();
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__property TBaseSpacePartition* SpacePartition = {read=FSpacePartition, write=SetSpacePartition};
	__property void * PartitionTag = {read=FPartitionTag};
	__fastcall TSpacePartitionLeaf(TBaseSpacePartition* SpacePartition);
	__fastcall virtual ~TSpacePartitionLeaf();
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TSpacePartitionLeaf() : Glpersistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSpacePartitionLeaf(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpacePartitionLeafList : public Glpersistentclasses::TPersistentObjectList
{
	typedef Glpersistentclasses::TPersistentObjectList inherited;
	
public:
	TSpacePartitionLeaf* operator[](int I) { return this->Items[I]; }
	
private:
	TSpacePartitionLeaf* __fastcall GetItems(int I);
	void __fastcall SetItems(int I, TSpacePartitionLeaf* const Value);
	
public:
	__property TSpacePartitionLeaf* Items[int I] = {read=GetItems, write=SetItems/*, default*/};
	__fastcall virtual TSpacePartitionLeafList();
public:
	/* TPersistentObjectList.Destroy */ inline __fastcall virtual ~TSpacePartitionLeafList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSpacePartitionLeafList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TCullingMode : unsigned char { CmFineCulling, CmGrossCulling };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseSpacePartition : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	TCullingMode FCullingMode;
	virtual int __fastcall QueryCone(const TSPCone &ACone);
	
protected:
	TSpacePartitionLeafList* FQueryResult;
	int FQueryInterObjectTests;
	virtual void __fastcall FlushQueryResult();
	
public:
	__property TSpacePartitionLeafList* QueryResult = {read=FQueryResult};
	virtual void __fastcall Clear();
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Glgeometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Glgeometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Glvectorgeometry::TAffineVector &Location, const Glvectorgeometry::TAffineVector &Normal);
	virtual int __fastcall QueryFrustum(const Glvectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum);
	__property int QueryInterObjectTests = {read=FQueryInterObjectTests, nodefault};
	virtual void __fastcall ProcessUpdated();
	__property TCullingMode CullingMode = {read=FCullingMode, write=FCullingMode, nodefault};
	__fastcall virtual TBaseSpacePartition();
	__fastcall virtual ~TBaseSpacePartition();
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseSpacePartition(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLeavedSpacePartition : public TBaseSpacePartition
{
	typedef TBaseSpacePartition inherited;
	
private:
	TSpacePartitionLeafList* FLeaves;
	virtual int __fastcall QueryCone(const TSPCone &ACone);
	
public:
	virtual void __fastcall Clear();
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Glgeometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Glgeometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryPlane(const Glvectorgeometry::TAffineVector &FLocation, const Glvectorgeometry::TAffineVector &FNormal);
	__fastcall virtual TLeavedSpacePartition();
	__fastcall virtual ~TLeavedSpacePartition();
	
__published:
	__property TSpacePartitionLeafList* Leaves = {read=FLeaves};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TLeavedSpacePartition(Glpersistentclasses::TVirtualReader* reader) : TBaseSpacePartition(reader) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<TSectorNode*, 8> TSectorNodeArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectorNode : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSpacePartitionLeafList* FLeaves;
	Glgeometrybb::TAABB FAABB;
	TSectoredSpacePartition* FSectoredSpacePartition;
	int FRecursiveLeafCount;
	TSectorNode* FParent;
	int FNodeDepth;
	int FChildCount;
	TSectorNodeArray FChildren;
	Glgeometrybb::TBSphere FBSphere;
	bool __fastcall GetNoChildren();
	void __fastcall SetAABB(const Glgeometrybb::TAABB &Value);
	Glvectorgeometry::TAffineVector __fastcall GetCenter();
	
protected:
	int __fastcall CalcRecursiveLeafCount();
	TSectorNode* __fastcall PlaceLeafInChild(TSpacePartitionLeaf* ALeaf);
	System::UnicodeString __fastcall VerifyRecursiveLeafCount();
	virtual void __fastcall ChildrenChanged();
	
public:
	void __fastcall Clear();
	__property Glgeometrybb::TAABB AABB = {read=FAABB, write=SetAABB};
	__property Glgeometrybb::TBSphere BSphere = {read=FBSphere};
	__property Glvectorgeometry::TAffineVector Center = {read=GetCenter};
	__property bool NoChildren = {read=GetNoChildren, nodefault};
	__property TSectorNodeArray Children = {read=FChildren};
	__property int ChildCount = {read=FChildCount, nodefault};
	virtual TSectorNode* __fastcall GetChildForAABB(const Glgeometrybb::TAABB &AABB);
	__property TSpacePartitionLeafList* Leaves = {read=FLeaves};
	__property TSectoredSpacePartition* SectoredSpacePartition = {read=FSectoredSpacePartition};
	__property TSectorNode* Parent = {read=FParent};
	__property int RecursiveLeafCount = {read=FRecursiveLeafCount, nodefault};
	__property int NodeDepth = {read=FNodeDepth, nodefault};
	virtual bool __fastcall AABBFitsInNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Glgeometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Glgeometrybb::TBSphere &BSphere);
	virtual Glgeometrybb::TSpaceContains __fastcall AABBContainsSector(const Glgeometrybb::TAABB &AABB);
	virtual Glgeometrybb::TSpaceContains __fastcall BSphereContainsSector(const Glgeometrybb::TBSphere &BSphere);
	virtual Glgeometrybb::TSpaceContains __fastcall ContainsBSphere(const Glgeometrybb::TBSphere &ABSphere);
	virtual Glgeometrybb::TSpaceContains __fastcall ContainsAABB(const Glgeometrybb::TAABB &AAABB);
	TSectorNode* __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	bool __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf, bool OwnerByThis);
	void __fastcall QueryAABB(const Glgeometrybb::TAABB &AAABB, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryBSphere(const Glgeometrybb::TBSphere &ABSphere, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryPlane(const Glvectorgeometry::TAffineVector &Location, const Glvectorgeometry::TAffineVector &Normal, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustum(const Glvectorgeometry::TFrustum &Frustum, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum, TSpacePartitionLeafList* const QueryResult);
	void __fastcall AddAllLeavesRecursive(TSpacePartitionLeafList* const QueryResult);
	void __fastcall ExpandNode();
	virtual void __fastcall CreateChildren();
	void __fastcall CollapseNode();
	int __fastcall GetNodeCount();
	__fastcall TSectorNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent);
	__fastcall virtual ~TSectorNode();
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGrowMethod : unsigned char { gmNever, gmBestFit, gmIncreaseToFitAll };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectoredSpacePartition : public TLeavedSpacePartition
{
	typedef TLeavedSpacePartition inherited;
	
private:
	TSectorNode* FRootNode;
	int FLeafThreshold;
	int FMaxTreeDepth;
	float FGrowGravy;
	TGrowMethod FGrowMethod;
	void __fastcall SetLeafThreshold(const int Value);
	void __fastcall SetMaxTreeDepth(const int Value);
	
protected:
	int FQueryNodeTests;
	virtual void __fastcall FlushQueryResult();
	
public:
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Glgeometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Glgeometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Glvectorgeometry::TAffineVector &Location, const Glvectorgeometry::TAffineVector &Normal);
	virtual int __fastcall QueryFrustum(const Glvectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum);
	__property int QueryNodeTests = {read=FQueryNodeTests, nodefault};
	int __fastcall GetNodeCount();
	void __fastcall UpdateStructureSize(float Gravy);
	void __fastcall RebuildTree(const Glgeometrybb::TAABB &NewAABB);
	Glgeometrybb::TAABB __fastcall GetAABB();
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
	virtual void __fastcall Clear();
	__fastcall virtual TSectoredSpacePartition();
	__fastcall virtual ~TSectoredSpacePartition();
	
__published:
	__property TSectorNode* RootNode = {read=FRootNode};
	__property int MaxTreeDepth = {read=FMaxTreeDepth, write=SetMaxTreeDepth, nodefault};
	__property int LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold, nodefault};
	__property TGrowMethod GrowMethod = {read=FGrowMethod, write=FGrowMethod, nodefault};
	__property float GrowGravy = {read=FGrowGravy, write=FGrowGravy};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSectoredSpacePartition(Glpersistentclasses::TVirtualReader* reader) : TLeavedSpacePartition(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSPOctreeNode : public TSectorNode
{
	typedef TSectorNode inherited;
	
public:
	virtual void __fastcall CreateChildren();
	virtual bool __fastcall AABBFitsInNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Glgeometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Glgeometrybb::TBSphere &BSphere);
public:
	/* TSectorNode.Create */ inline __fastcall TSPOctreeNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent) : TSectorNode(ASectoredSpacePartition, AParent) { }
	/* TSectorNode.Destroy */ inline __fastcall virtual ~TSPOctreeNode() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TOctreeSpacePartition : public TSectoredSpacePartition
{
	typedef TSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Glvectorgeometry::TAffineVector &Min, const Glvectorgeometry::TAffineVector &Max);
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
public:
	/* TSectoredSpacePartition.Create */ inline __fastcall virtual TOctreeSpacePartition() : TSectoredSpacePartition() { }
	/* TSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TOctreeSpacePartition() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TOctreeSpacePartition(Glpersistentclasses::TVirtualReader* reader) : TSectoredSpacePartition(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSPQuadtreeNode : public TSPOctreeNode
{
	typedef TSPOctreeNode inherited;
	
protected:
	virtual void __fastcall ChildrenChanged();
	
public:
	virtual void __fastcall CreateChildren();
	virtual bool __fastcall AABBFitsInNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Glgeometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Glgeometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Glgeometrybb::TBSphere &BSphere);
	virtual TSectorNode* __fastcall GetChildForAABB(const Glgeometrybb::TAABB &AABB);
public:
	/* TSectorNode.Create */ inline __fastcall TSPQuadtreeNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent) : TSPOctreeNode(ASectoredSpacePartition, AParent) { }
	/* TSectorNode.Destroy */ inline __fastcall virtual ~TSPQuadtreeNode() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TQuadtreeSpacePartition : public TSectoredSpacePartition
{
	typedef TSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Glvectorgeometry::TAffineVector &Min, const Glvectorgeometry::TAffineVector &Max);
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
public:
	/* TSectoredSpacePartition.Create */ inline __fastcall virtual TQuadtreeSpacePartition() : TSectoredSpacePartition() { }
	/* TSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TQuadtreeSpacePartition() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TQuadtreeSpacePartition(Glpersistentclasses::TVirtualReader* reader) : TSectoredSpacePartition(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 COctree_LEAF_TRHESHOLD = System::Int8(0x1e);
static const System::Int8 COctree_MAX_TREE_DEPTH = System::Int8(0x8);
#define COctree_GROW_GRAVY  (1.000000E-01)
extern DELPHI_PACKAGE Glgeometrybb::TSpaceContains __fastcall ConeContainsBSphere(const TSPCone &Cone, const Glgeometrybb::TBSphere &BSphere);
extern DELPHI_PACKAGE bool __fastcall ExtendedFrustumIntersectsBSphere(const TExtendedFrustum &AExtendedFrustum, const Glgeometrybb::TBSphere &ABSphere);
extern DELPHI_PACKAGE TExtendedFrustum __fastcall ExtendedFrustumMake(const Glvectorgeometry::TFrustum &AFrustum, const float ANearDist, const float AFarDist, const float AFieldOfViewRadians, const Glvectorgeometry::TAffineVector &ACameraPosition, const Glvectorgeometry::TAffineVector &ALookVector);
}	/* namespace Glspacepartition */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSPACEPARTITION)
using namespace Glspacepartition;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlspacepartitionHPP
