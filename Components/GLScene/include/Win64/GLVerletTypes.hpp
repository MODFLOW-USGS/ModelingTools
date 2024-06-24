// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glverlettypes.pas' rev: 36.00 (Windows)

#ifndef GlverlettypesHPP
#define GlverlettypesHPP

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
#include <System.Types.hpp>
#include <Globjects.hpp>
#include <Glscene.hpp>
#include <Glcoordinates.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glspacepartition.hpp>
#include <Glgeometrybb.hpp>
#include <Glvectortypes.hpp>
#include <Glpersistentclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glverlettypes
{
//-- forward type declarations -----------------------------------------------
struct TVerletProgressTimes;
class DELPHICLASS TVerletNode;
class DELPHICLASS TVerletNodeList;
class DELPHICLASS TVerletConstraint;
class DELPHICLASS TGLVerletDualConstraint;
class DELPHICLASS TVerletGroupConstraint;
class DELPHICLASS TGLVerletEdge;
class DELPHICLASS TGLVerletEdgeList;
class DELPHICLASS TGLVerletGlobalConstraint;
class DELPHICLASS TGLVerletGlobalFrictionConstraint;
class DELPHICLASS TGLVerletGlobalFrictionConstraintSP;
class DELPHICLASS TGLVerletGlobalFrictionConstraintSphere;
class DELPHICLASS TGLVerletGlobalFrictionConstraintBox;
class DELPHICLASS TVerletConstraintList;
class DELPHICLASS TGLVerletForce;
class DELPHICLASS TGLVerletDualForce;
class DELPHICLASS TVerletGroupForce;
class DELPHICLASS TGLVerletGlobalForce;
class DELPHICLASS TGLVerletForceList;
class DELPHICLASS TGLVerletWorld;
class DELPHICLASS TVFGravity;
class DELPHICLASS TVFAirResistance;
class DELPHICLASS TVFSpring;
class DELPHICLASS TVCFloor;
class DELPHICLASS TVCHeightField;
class DELPHICLASS TVCStick;
class DELPHICLASS TVCRigidBody;
class DELPHICLASS TVCSlider;
class DELPHICLASS TVCSphere;
class DELPHICLASS TVCCylinder;
class DELPHICLASS TVCCube;
class DELPHICLASS TVCCapsule;
class DELPHICLASS TGLVerletNode;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TVerletProgressTimes
{
public:
	double deltaTime;
	double newTime;
	float sqrDeltaTime;
	float invSqrDeltaTime;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TVerletNode : public Glspacepartition::TSpacePartitionLeaf
{
	typedef Glspacepartition::TSpacePartitionLeaf inherited;
	
private:
	Glvectorgeometry::TAffineVector FForce;
	TGLVerletWorld* FOwner;
	float FWeight;
	float FInvWeight;
	float FRadius;
	bool FNailedDown;
	float FFriction;
	int FChangedOnStep;
	Glvectorgeometry::TAffineVector __fastcall GetSpeed();
	
protected:
	Glvectorgeometry::TAffineVector FLocation;
	Glvectorgeometry::TAffineVector FOldLocation;
	virtual void __fastcall SetLocation(const Glvectorgeometry::TAffineVector &Value);
	void __fastcall SetWeight(const float Value);
	virtual void __fastcall AfterProgress();
	
public:
	__fastcall virtual TVerletNode(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TVerletNode();
	void __fastcall ApplyFriction(const float friction, const float penetrationDepth, const Glvectorgeometry::TAffineVector &surfaceNormal);
	void __fastcall OldApplyFriction(const float friction, const float penetrationDepth);
	virtual void __fastcall Verlet(const TVerletProgressTimes &vpt);
	DYNAMIC void __fastcall Initialize();
	float __fastcall DistanceToNode(TVerletNode* const node);
	Glvectorgeometry::TAffineVector __fastcall GetMovement();
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__property TGLVerletWorld* Owner = {read=FOwner};
	__property Glvectorgeometry::TAffineVector Location = {read=FLocation, write=SetLocation};
	__property Glvectorgeometry::TAffineVector OldLocation = {read=FOldLocation, write=FOldLocation};
	__property float Radius = {read=FRadius, write=FRadius};
	__property Glvectorgeometry::TAffineVector Force = {read=FForce, write=FForce};
	__property bool NailedDown = {read=FNailedDown, write=FNailedDown, nodefault};
	__property float Weight = {read=FWeight, write=SetWeight};
	__property float InvWeight = {read=FInvWeight};
	__property Glvectorgeometry::TAffineVector Speed = {read=GetSpeed};
	__property float friction = {read=FFriction, write=FFriction};
	__property int ChangedOnStep = {read=FChangedOnStep, nodefault};
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TVerletNode() : Glspacepartition::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVerletNode(Glpersistentclasses::TVirtualReader* reader) : Glspacepartition::TSpacePartitionLeaf(reader) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TVerletNodeClass);

class PASCALIMPLEMENTATION TVerletNodeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletNode* operator[](int i) { return this->Items[i]; }
	
private:
	TVerletNode* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletNode* const Value);
	
public:
	__property TVerletNode* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletNodeList() { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletNodeList() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TVerletConstraint : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLVerletWorld* FOwner;
	bool FEnabled;
	int FTag;
	
public:
	__fastcall virtual TVerletConstraint(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TVerletConstraint();
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations) = 0 ;
	virtual void __fastcall RemoveNode(TVerletNode* const aNode) = 0 ;
	virtual void __fastcall BeforeIterations();
	__property TGLVerletWorld* Owner = {read=FOwner};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
};


class PASCALIMPLEMENTATION TGLVerletDualConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TGLVerletDualConstraint(TGLVerletWorld* const aOwner) : TVerletConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TGLVerletDualConstraint() { }
	
};


class PASCALIMPLEMENTATION TVerletGroupConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	TVerletNodeList* FNodes;
	
public:
	__fastcall virtual TVerletGroupConstraint(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TVerletGroupConstraint();
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNodeList* Nodes = {read=FNodes};
};


class PASCALIMPLEMENTATION TGLVerletEdge : public Glspacepartition::TSpacePartitionLeaf
{
	typedef Glspacepartition::TSpacePartitionLeaf inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall UpdateCachedAABBAndBSphere();
	__fastcall TGLVerletEdge(TVerletNode* const aNodeA, TVerletNode* const aNodeB);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TSpacePartitionLeaf.CreateOwned */ inline __fastcall TGLVerletEdge(Glspacepartition::TBaseSpacePartition* SpacePartition) : Glspacepartition::TSpacePartitionLeaf(SpacePartition) { }
	/* TSpacePartitionLeaf.Destroy */ inline __fastcall virtual ~TGLVerletEdge() { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLVerletEdge() : Glspacepartition::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLVerletEdge(Glpersistentclasses::TVirtualReader* reader) : Glspacepartition::TSpacePartitionLeaf(reader) { }
	
};


class PASCALIMPLEMENTATION TGLVerletEdgeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLVerletEdge* operator[](int i) { return this->Items[i]; }
	
private:
	TGLVerletEdge* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TGLVerletEdge* const Value);
	
public:
	__property TGLVerletEdge* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLVerletEdgeList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLVerletEdgeList() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TGLVerletGlobalConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	Glvectorgeometry::TAffineVector FKickbackForce;
	Glvectorgeometry::TAffineVector FKickbackTorque;
	Glvectorgeometry::TAffineVector FLocation;
	virtual void __fastcall SetLocation(const Glvectorgeometry::TAffineVector &Value);
	
public:
	__fastcall virtual TGLVerletGlobalConstraint(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TGLVerletGlobalConstraint();
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	virtual void __fastcall BeforeIterations();
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations) = 0 ;
	virtual void __fastcall SatisfyConstraintForEdge(TGLVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Glvectorgeometry::TAffineVector Location = {read=FLocation, write=SetLocation};
	__property Glvectorgeometry::TAffineVector KickbackForce = {read=FKickbackForce, write=FKickbackForce};
	__property Glvectorgeometry::TAffineVector KickbackTorque = {read=FKickbackTorque, write=FKickbackTorque};
	void __fastcall AddKickbackForceAt(const Glvectorgeometry::TAffineVector &Pos, const Glvectorgeometry::TAffineVector &Force);
	Glvectorgeometry::TAffineVector __fastcall TranslateKickbackTorque(const Glvectorgeometry::TAffineVector &TorqueCenter);
};


class PASCALIMPLEMENTATION TGLVerletGlobalFrictionConstraint : public TGLVerletGlobalConstraint
{
	typedef TGLVerletGlobalConstraint inherited;
	
private:
	float FFrictionRatio;
	
public:
	__fastcall virtual TGLVerletGlobalFrictionConstraint(TGLVerletWorld* const aOwner);
	__property float FrictionRatio = {read=FFrictionRatio, write=FFrictionRatio};
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TGLVerletGlobalFrictionConstraint() { }
	
};


class PASCALIMPLEMENTATION TGLVerletGlobalFrictionConstraintSP : public TGLVerletGlobalFrictionConstraint
{
	typedef TGLVerletGlobalFrictionConstraint inherited;
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	virtual void __fastcall PerformSpaceQuery() = 0 ;
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TGLVerletGlobalFrictionConstraintSP(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraint(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TGLVerletGlobalFrictionConstraintSP() { }
	
};


class PASCALIMPLEMENTATION TGLVerletGlobalFrictionConstraintSphere : public TGLVerletGlobalFrictionConstraintSP
{
	typedef TGLVerletGlobalFrictionConstraintSP inherited;
	
private:
	Glgeometrybb::TBSphere FCachedBSphere;
	virtual void __fastcall SetLocation(const Glvectorgeometry::TAffineVector &Value);
	
public:
	void __fastcall UpdateCachedBSphere();
	virtual void __fastcall PerformSpaceQuery();
	virtual Glgeometrybb::TBSphere __fastcall GetBSphere() = 0 ;
	__property Glgeometrybb::TBSphere CachedBSphere = {read=FCachedBSphere};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TGLVerletGlobalFrictionConstraintSphere(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraintSP(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TGLVerletGlobalFrictionConstraintSphere() { }
	
};


class PASCALIMPLEMENTATION TGLVerletGlobalFrictionConstraintBox : public TGLVerletGlobalFrictionConstraintSP
{
	typedef TGLVerletGlobalFrictionConstraintSP inherited;
	
private:
	Glgeometrybb::TAABB FCachedAABB;
	virtual void __fastcall SetLocation(const Glvectorgeometry::TAffineVector &Value);
	
public:
	void __fastcall UpdateCachedAABB();
	virtual void __fastcall PerformSpaceQuery();
	virtual Glgeometrybb::TAABB __fastcall GetAABB() = 0 ;
	__property Glgeometrybb::TAABB CachedAABB = {read=FCachedAABB};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TGLVerletGlobalFrictionConstraintBox(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraintSP(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TGLVerletGlobalFrictionConstraintBox() { }
	
};


class PASCALIMPLEMENTATION TVerletConstraintList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletConstraint* operator[](int i) { return this->Items[i]; }
	
private:
	TVerletConstraint* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletConstraint* const Value);
	
public:
	__property TVerletConstraint* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletConstraintList() { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletConstraintList() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TGLVerletForce : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLVerletWorld* FOwner;
	
public:
	__fastcall virtual TGLVerletForce(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TGLVerletForce();
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt) = 0 ;
	virtual void __fastcall RemoveNode(TVerletNode* const aNode) = 0 ;
	__property TGLVerletWorld* Owner = {read=FOwner};
};


class PASCALIMPLEMENTATION TGLVerletDualForce : public TGLVerletForce
{
	typedef TGLVerletForce inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TGLVerletForce.Create */ inline __fastcall virtual TGLVerletDualForce(TGLVerletWorld* const aOwner) : TGLVerletForce(aOwner) { }
	/* TGLVerletForce.Destroy */ inline __fastcall virtual ~TGLVerletDualForce() { }
	
};


class PASCALIMPLEMENTATION TVerletGroupForce : public TGLVerletForce
{
	typedef TGLVerletForce inherited;
	
private:
	TVerletNodeList* FNodes;
	
public:
	__fastcall virtual TVerletGroupForce(TGLVerletWorld* const aOwner);
	__fastcall virtual ~TVerletGroupForce();
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNodeList* Nodes = {read=FNodes};
};


class PASCALIMPLEMENTATION TGLVerletGlobalForce : public TGLVerletForce
{
	typedef TGLVerletForce inherited;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode) = 0 ;
public:
	/* TGLVerletForce.Create */ inline __fastcall virtual TGLVerletGlobalForce(TGLVerletWorld* const aOwner) : TGLVerletForce(aOwner) { }
	/* TGLVerletForce.Destroy */ inline __fastcall virtual ~TGLVerletGlobalForce() { }
	
};


class PASCALIMPLEMENTATION TGLVerletForceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLVerletForce* operator[](int i) { return this->Items[i]; }
	
private:
	TGLVerletForce* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TGLVerletForce* const Value);
	
public:
	__property TGLVerletForce* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLVerletForceList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLVerletForceList() : System::Classes::TList() { }
	
};


enum DECLSPEC_DENUM TUpdateSpacePartion : unsigned char { uspEveryIteration, uspEveryFrame, uspNever };

enum DECLSPEC_DENUM TCollisionConstraintTypes : unsigned char { cctEdge, cctNode };

typedef System::Set<TCollisionConstraintTypes, TCollisionConstraintTypes::cctEdge, TCollisionConstraintTypes::cctNode> TCollisionConstraintTypesSet;

class PASCALIMPLEMENTATION TGLVerletWorld : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FIterations;
	TVerletNodeList* FNodes;
	TVerletConstraintList* FConstraints;
	TGLVerletForceList* FForces;
	float FMaxDeltaTime;
	float FSimTime;
	float FDrag;
	float FCurrentDeltaTime;
	float FInvCurrentDeltaTime;
	TGLVerletEdgeList* FSolidEdges;
	Glspacepartition::TBaseSpacePartition* FSpacePartition;
	int FCurrentStepCount;
	TUpdateSpacePartion FUpdateSpacePartion;
	TCollisionConstraintTypesSet FCollisionConstraintTypes;
	TVerletConstraintList* FConstraintsWithBeforeIterations;
	TVerletNodeClass FVerletNodeClass;
	bool FInertia;
	int FInertaPauseSteps;
	
protected:
	virtual void __fastcall AccumulateForces(const TVerletProgressTimes &vpt);
	virtual void __fastcall Verlet(const TVerletProgressTimes &vpt);
	virtual void __fastcall SatisfyConstraints(const TVerletProgressTimes &vpt);
	void __fastcall DoUpdateSpacePartition();
	
public:
	__fastcall virtual TGLVerletWorld();
	__fastcall virtual ~TGLVerletWorld();
	int __fastcall AddNode(TVerletNode* const aNode);
	void __fastcall RemoveNode(TVerletNode* const aNode);
	int __fastcall AddConstraint(TVerletConstraint* const aConstraint);
	void __fastcall RemoveConstraint(TVerletConstraint* const aConstraint);
	int __fastcall AddForce(TGLVerletForce* const aForce);
	void __fastcall RemoveForce(TGLVerletForce* const aForce);
	void __fastcall AddSolidEdge(TVerletNode* const aNodeA, TVerletNode* const aNodeB);
	void __fastcall PauseInertia(const int IterationSteps);
	TVerletNode* __fastcall CreateOwnedNode(const Glvectorgeometry::TAffineVector &Location, const float aRadius = 0.000000E+00f, const float aWeight = 1.000000E+00f);
	TVCStick* __fastcall CreateStick(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const float Slack = 0.000000E+00f);
	TVFSpring* __fastcall CreateSpring(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const float aStrength, const float aDamping, const float aSlack = 0.000000E+00f);
	TVCSlider* __fastcall CreateSlider(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const Glvectorgeometry::TAffineVector &aSlideDirection);
	virtual void __fastcall Initialize();
	void __fastcall CreateOctree(const Glvectorgeometry::TAffineVector &OctreeMin, const Glvectorgeometry::TAffineVector &OctreeMax, const int LeafThreshold, const int MaxTreeDepth);
	virtual int __fastcall Progress(const double deltaTime, const double newTime);
	TVerletNode* __fastcall FirstNode();
	TVerletNode* __fastcall LastNode();
	__property float Drag = {read=FDrag, write=FDrag};
	__property int Iterations = {read=FIterations, write=FIterations, nodefault};
	__property TVerletNodeList* Nodes = {read=FNodes};
	__property TVerletConstraintList* Constraints = {read=FConstraints};
	__property TVerletConstraintList* ConstraintsWithBeforeIterations = {read=FConstraintsWithBeforeIterations};
	__property float SimTime = {read=FSimTime, write=FSimTime};
	__property float MaxDeltaTime = {read=FMaxDeltaTime, write=FMaxDeltaTime};
	__property float CurrentDeltaTime = {read=FCurrentDeltaTime};
	__property TGLVerletEdgeList* SolidEdges = {read=FSolidEdges, write=FSolidEdges};
	__property int CurrentStepCount = {read=FCurrentStepCount, nodefault};
	__property Glspacepartition::TBaseSpacePartition* SpacePartition = {read=FSpacePartition};
	__property TUpdateSpacePartion UpdateSpacePartion = {read=FUpdateSpacePartion, write=FUpdateSpacePartion, nodefault};
	__property TCollisionConstraintTypesSet CollisionConstraintTypes = {read=FCollisionConstraintTypes, write=FCollisionConstraintTypes, nodefault};
	__property TVerletNodeClass VerletNodeClass = {read=FVerletNodeClass, write=FVerletNodeClass};
	__property bool Inertia = {read=FInertia, write=FInertia, nodefault};
};


class PASCALIMPLEMENTATION TVFGravity : public TGLVerletGlobalForce
{
	typedef TGLVerletGlobalForce inherited;
	
private:
	Glvectorgeometry::TAffineVector FGravity;
	
public:
	__fastcall virtual TVFGravity(TGLVerletWorld* const aOwner);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode);
	__property Glvectorgeometry::TAffineVector Gravity = {read=FGravity, write=FGravity};
public:
	/* TGLVerletForce.Destroy */ inline __fastcall virtual ~TVFGravity() { }
	
};


class PASCALIMPLEMENTATION TVFAirResistance : public TGLVerletGlobalForce
{
	typedef TGLVerletGlobalForce inherited;
	
private:
	float FDragCoeff;
	Glvectorgeometry::TAffineVector FWindDirection;
	float FWindMagnitude;
	float FWindChaos;
	void __fastcall SetWindDirection(const Glvectorgeometry::TAffineVector &Value);
	
public:
	__fastcall virtual TVFAirResistance(TGLVerletWorld* const aOwner);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode);
	__property float DragCoeff = {read=FDragCoeff, write=FDragCoeff};
	__property Glvectorgeometry::TAffineVector WindDirection = {read=FWindDirection, write=SetWindDirection};
	__property float WindMagnitude = {read=FWindMagnitude, write=FWindMagnitude};
	__property float WindChaos = {read=FWindChaos, write=FWindChaos};
public:
	/* TGLVerletForce.Destroy */ inline __fastcall virtual ~TVFAirResistance() { }
	
};


class PASCALIMPLEMENTATION TVFSpring : public TGLVerletDualForce
{
	typedef TGLVerletDualForce inherited;
	
private:
	float FRestLength;
	float FStrength;
	float FDamping;
	float FSlack;
	float FForceFactor;
	
protected:
	void __fastcall SetSlack(const float Value);
	
public:
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt);
	void __fastcall SetRestLengthToCurrent();
	__property float Strength = {read=FStrength, write=FStrength};
	__property float Damping = {read=FDamping, write=FDamping};
	__property float Slack = {read=FSlack, write=SetSlack};
public:
	/* TGLVerletForce.Create */ inline __fastcall virtual TVFSpring(TGLVerletWorld* const aOwner) : TGLVerletDualForce(aOwner) { }
	/* TGLVerletForce.Destroy */ inline __fastcall virtual ~TVFSpring() { }
	
};


class PASCALIMPLEMENTATION TVCFloor : public TGLVerletGlobalFrictionConstraintSP
{
	typedef TGLVerletGlobalFrictionConstraintSP inherited;
	
private:
	float FBounceRatio;
	float FFloorLevel;
	Glvectorgeometry::TAffineVector FNormal;
	
protected:
	void __fastcall SetNormal(const Glvectorgeometry::TAffineVector &Value);
	
public:
	__fastcall virtual TVCFloor(TGLVerletWorld* const aOwner);
	virtual void __fastcall PerformSpaceQuery();
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property float BounceRatio = {read=FBounceRatio, write=FBounceRatio};
	__property float FloorLevel = {read=FFloorLevel, write=FFloorLevel};
	__property Glvectorgeometry::TAffineVector Normal = {read=FNormal, write=SetNormal};
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCFloor() { }
	
};


typedef float __fastcall (__closure *TVCHeightFieldOnNeedHeight)(TVCHeightField* hfConstraint, TVerletNode* node);

class PASCALIMPLEMENTATION TVCHeightField : public TVCFloor
{
	typedef TVCFloor inherited;
	
private:
	TVCHeightFieldOnNeedHeight FOnNeedHeight;
	
public:
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property TVCHeightFieldOnNeedHeight OnNeedHeight = {read=FOnNeedHeight, write=FOnNeedHeight};
public:
	/* TVCFloor.Create */ inline __fastcall virtual TVCHeightField(TGLVerletWorld* const aOwner) : TVCFloor(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCHeightField() { }
	
};


class PASCALIMPLEMENTATION TVCStick : public TGLVerletDualConstraint
{
	typedef TGLVerletDualConstraint inherited;
	
private:
	float FSlack;
	float FRestLength;
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	void __fastcall SetRestLengthToCurrent();
	__property float Slack = {read=FSlack, write=FSlack};
	__property float RestLength = {read=FRestLength, write=FRestLength};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TVCStick(TGLVerletWorld* const aOwner) : TGLVerletDualConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TVCStick() { }
	
};


class PASCALIMPLEMENTATION TVCRigidBody : public TVerletGroupConstraint
{
	typedef TVerletGroupConstraint inherited;
	
	
private:
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TVCRigidBody__1;
	
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TVCRigidBody__2;
	
	
private:
	_TVCRigidBody__1 FNodeParams;
	_TVCRigidBody__2 FNodeCoords;
	Glvectorgeometry::TAffineMatrix FNatMatrix;
	Glvectorgeometry::TAffineMatrix FInvNatMatrix;
	
protected:
	void __fastcall ComputeBarycenter(Glvectorgeometry::TAffineVector &barycenter);
	void __fastcall ComputeNaturals(const Glvectorgeometry::TAffineVector &barycenter, Glvectorgeometry::TAffineVector &natX, Glvectorgeometry::TAffineVector &natY, Glvectorgeometry::TAffineVector &natZ);
	
public:
	void __fastcall ComputeRigidityParameters();
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
public:
	/* TVerletGroupConstraint.Create */ inline __fastcall virtual TVCRigidBody(TGLVerletWorld* const aOwner) : TVerletGroupConstraint(aOwner) { }
	/* TVerletGroupConstraint.Destroy */ inline __fastcall virtual ~TVCRigidBody() { }
	
};


class PASCALIMPLEMENTATION TVCSlider : public TGLVerletDualConstraint
{
	typedef TGLVerletDualConstraint inherited;
	
private:
	Glvectorgeometry::TAffineVector FSlideDirection;
	bool FConstrained;
	
protected:
	void __fastcall SetSlideDirection(const Glvectorgeometry::TAffineVector &Value);
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	__property Glvectorgeometry::TAffineVector SlideDirection = {read=FSlideDirection, write=SetSlideDirection};
	__property bool Constrained = {read=FConstrained, write=FConstrained, nodefault};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TVCSlider(TGLVerletWorld* const aOwner) : TGLVerletDualConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TVCSlider() { }
	
};


class PASCALIMPLEMENTATION TVCSphere : public TGLVerletGlobalFrictionConstraintSphere
{
	typedef TGLVerletGlobalFrictionConstraintSphere inherited;
	
private:
	float FRadius;
	
public:
	virtual Glgeometrybb::TBSphere __fastcall GetBSphere();
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TGLVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property float Radius = {read=FRadius, write=FRadius};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCSphere(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraintSphere(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCSphere() { }
	
};


class PASCALIMPLEMENTATION TVCCylinder : public TGLVerletGlobalFrictionConstraint
{
	typedef TGLVerletGlobalFrictionConstraint inherited;
	
private:
	Glvectorgeometry::TAffineVector FAxis;
	float FRadius;
	float FRadius2;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property Glvectorgeometry::TAffineVector Axis = {read=FAxis, write=FAxis};
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCylinder(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraint(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCylinder() { }
	
};


class PASCALIMPLEMENTATION TVCCube : public TGLVerletGlobalFrictionConstraintBox
{
	typedef TGLVerletGlobalFrictionConstraintBox inherited;
	
private:
	Glvectorgeometry::TAffineVector FHalfSides;
	Glvectorgeometry::TAffineVector FSides;
	Glvectorgeometry::TAffineVector FDirection;
	void __fastcall SetSides(const Glvectorgeometry::TAffineVector &Value);
	
public:
	virtual Glgeometrybb::TAABB __fastcall GetAABB();
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TGLVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Glvectorgeometry::TAffineVector Direction = {read=FDirection, write=FDirection};
	__property Glvectorgeometry::TAffineVector Sides = {read=FSides, write=SetSides};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCube(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraintBox(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCube() { }
	
};


class PASCALIMPLEMENTATION TVCCapsule : public TGLVerletGlobalFrictionConstraintSphere
{
	typedef TGLVerletGlobalFrictionConstraintSphere inherited;
	
private:
	Glvectorgeometry::TAffineVector FAxis;
	float FRadius;
	float FRadius2;
	float FLength;
	float FLengthDiv2;
	
protected:
	void __fastcall SetAxis(const Glvectorgeometry::TAffineVector &val);
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	virtual Glgeometrybb::TBSphere __fastcall GetBSphere();
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TGLVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Glvectorgeometry::TAffineVector Axis = {read=FAxis, write=SetAxis};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TGLVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCapsule(TGLVerletWorld* const aOwner) : TGLVerletGlobalFrictionConstraintSphere(aOwner) { }
	
public:
	/* TGLVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCapsule() { }
	
};


class PASCALIMPLEMENTATION TGLVerletNode : public TVerletNode
{
	typedef TVerletNode inherited;
	
private:
	Glvectorgeometry::TAffineVector FRelativePosition;
	Glscene::TGLBaseSceneObject* FGLBaseSceneObject;
	void __fastcall SetGLBaseSceneObject(Glscene::TGLBaseSceneObject* const Value);
	
protected:
	virtual void __fastcall SetLocation(const Glvectorgeometry::TAffineVector &Value);
	
public:
	virtual void __fastcall Verlet(const TVerletProgressTimes &vpt);
	__property Glscene::TGLBaseSceneObject* GLBaseSceneObject = {read=FGLBaseSceneObject, write=SetGLBaseSceneObject};
	__property Glvectorgeometry::TAffineVector RelativePosition = {read=FRelativePosition, write=FRelativePosition};
public:
	/* TVerletNode.CreateOwned */ inline __fastcall virtual TGLVerletNode(TGLVerletWorld* const aOwner) : TVerletNode(aOwner) { }
	/* TVerletNode.Destroy */ inline __fastcall virtual ~TGLVerletNode() { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLVerletNode() : TVerletNode() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLVerletNode(Glpersistentclasses::TVirtualReader* reader) : TVerletNode(reader) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define G_DRAG  (1.000000E-04)
#define cDEFAULT_CONSTRAINT_FRICTION  (6.000000E-01)
extern DELPHI_PACKAGE TVCFloor* __fastcall CreateVCPlaneFromGLPlane(Globjects::TGLPlane* Plane, TGLVerletWorld* VerletWorld, float Offset);
}	/* namespace Glverlettypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETTYPES)
using namespace Glverlettypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlverlettypesHPP
