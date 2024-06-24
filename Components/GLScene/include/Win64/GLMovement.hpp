// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glmovement.pas' rev: 36.00 (Windows)

#ifndef GlmovementHPP
#define GlmovementHPP

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
#include <Opengltokens.hpp>
#include <Glscene.hpp>
#include <Glpersistentclasses.hpp>
#include <Glvectorgeometry.hpp>
#include <Glxcollection.hpp>
#include <Glspline.hpp>
#include <Globjects.hpp>
#include <Glcrossplatform.hpp>
#include <Glstrings.hpp>
#include <Glbaseclasses.hpp>
#include <Glvectortypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmovement
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPathNode;
class DELPHICLASS TGLPathNodes;
class DELPHICLASS TGLMovementPath;
class DELPHICLASS TGLMovementPaths;
class DELPHICLASS TGLMovement;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPathNode : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glvectorgeometry::TVector FPosition;
	Glvectorgeometry::TVector FScale;
	Glvectorgeometry::TVector FRotation;
	Glvectorgeometry::TVector FDirection;
	Glvectorgeometry::TVector FUp;
	float FSpeed;
	void __fastcall SetPositionAsVector(const Glvectorgeometry::TVector &Value);
	void __fastcall SetRotationAsVector(const Glvectorgeometry::TVector &Value);
	void __fastcall SetScaleAsVector(const Glvectorgeometry::TVector &Value);
	Opengltokens::TGLfloat __fastcall GetPositionCoordinate(const int Index);
	void __fastcall SetPositionCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	Opengltokens::TGLfloat __fastcall GetRotationCoordinate(const int Index);
	void __fastcall SetRotationCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	Opengltokens::TGLfloat __fastcall GetScaleCoordinate(const int Index);
	void __fastcall SetScaleCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	void __fastcall SetSpeed(const float Value);
	Opengltokens::TGLfloat __fastcall GetDirectionCoordinate(const int Index);
	void __fastcall SetDirectionCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	Opengltokens::TGLfloat __fastcall GetUpCoordinate(const int Index);
	void __fastcall SetUpCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall virtual TGLPathNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLPathNode();
	Opengltokens::PGLfloat __fastcall PositionAsAddress();
	Opengltokens::PGLfloat __fastcall RotationAsAddress();
	Opengltokens::PGLfloat __fastcall ScaleAsAddress();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall InitializeByObject(Glscene::TGLBaseSceneObject* const Obj);
	bool __fastcall EqualNode(TGLPathNode* const aNode);
	__property Glvectorgeometry::TVector RotationAsVector = {read=FRotation, write=SetRotationAsVector};
	__property Glvectorgeometry::TVector PositionAsVector = {read=FPosition, write=SetPositionAsVector};
	__property Glvectorgeometry::TVector ScaleAsVector = {read=FScale, write=SetScaleAsVector};
	__property Glvectorgeometry::TVector UpAsVector = {read=FUp, write=FUp};
	__property Glvectorgeometry::TVector DirectionAsVector = {read=FDirection, write=FDirection};
	
__published:
	__property Opengltokens::TGLfloat X = {read=GetPositionCoordinate, write=SetPositionCoordinate, index=0};
	__property Opengltokens::TGLfloat Y = {read=GetPositionCoordinate, write=SetPositionCoordinate, index=1};
	__property Opengltokens::TGLfloat Z = {read=GetPositionCoordinate, write=SetPositionCoordinate, index=2};
	__property Opengltokens::TGLfloat PitchAngle = {read=GetRotationCoordinate, write=SetRotationCoordinate, index=0};
	__property Opengltokens::TGLfloat TurnAngle = {read=GetRotationCoordinate, write=SetRotationCoordinate, index=1};
	__property Opengltokens::TGLfloat RollAngle = {read=GetRotationCoordinate, write=SetRotationCoordinate, index=2};
	__property Opengltokens::TGLfloat ScaleX = {read=GetScaleCoordinate, write=SetScaleCoordinate, index=0};
	__property Opengltokens::TGLfloat ScaleY = {read=GetScaleCoordinate, write=SetScaleCoordinate, index=1};
	__property Opengltokens::TGLfloat ScaleZ = {read=GetScaleCoordinate, write=SetScaleCoordinate, index=2};
	__property Opengltokens::TGLfloat DirectionX = {read=GetDirectionCoordinate, write=SetDirectionCoordinate, index=0};
	__property Opengltokens::TGLfloat DirectionY = {read=GetDirectionCoordinate, write=SetDirectionCoordinate, index=1};
	__property Opengltokens::TGLfloat DirectionZ = {read=GetDirectionCoordinate, write=SetDirectionCoordinate, index=2};
	__property Opengltokens::TGLfloat UpX = {read=GetUpCoordinate, write=SetUpCoordinate, index=0};
	__property Opengltokens::TGLfloat UpY = {read=GetUpCoordinate, write=SetUpCoordinate, index=1};
	__property Opengltokens::TGLfloat UpZ = {read=GetUpCoordinate, write=SetUpCoordinate, index=2};
	__property float Speed = {read=FSpeed, write=SetSpeed};
};


enum DECLSPEC_DENUM TGLMovementRotationMode : unsigned char { rmTurnPitchRoll, rmUpDirection };

class PASCALIMPLEMENTATION TGLPathNodes : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLPathNode* operator[](const int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(const int index, TGLPathNode* const val);
	TGLPathNode* __fastcall GetItems(const int index);
	
public:
	__fastcall TGLPathNodes(TGLMovementPath* aOwner);
	TGLMovementPath* __fastcall GetOwnerMovementPath();
	HIDESBASE TGLPathNode* __fastcall Add();
	HIDESBASE TGLPathNode* __fastcall FindItemID(const int ID);
	__property TGLPathNode* Items[const int index] = {read=GetItems, write=SetItems/*, default*/};
	virtual void __fastcall NotifyChange();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLPathNodes() { }
	
};


class PASCALIMPLEMENTATION TGLMovementPath : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Globjects::TGLLines* FPathLine;
	bool FShowPath;
	Globjects::TGLLineSplineMode FPathSplineMode;
	TGLPathNodes* FNodes;
	bool FStartTimeApplied;
	double FStartTime;
	double FInitialTime;
	double FEstimateTime;
	TGLPathNode* FCurrentNode;
	bool FInTravel;
	bool FLooped;
	System::UnicodeString FName;
	TGLMovementRotationMode FRotationMode;
	Glspline::TCubicSpline* MotionSplineControl;
	Glspline::TCubicSpline* RotationSplineControl;
	Glspline::TCubicSpline* ScaleSplineControl;
	Glspline::TCubicSpline* DirectionSplineControl;
	Glspline::TCubicSpline* UpSplineControl;
	System::Classes::TNotifyEvent FOnTravelStart;
	System::Classes::TNotifyEvent FOnTravelStop;
	int FCurrentNodeIndex;
	int __fastcall GetNodeCount();
	void __fastcall SetStartTime(const double Value);
	void __fastcall SetCurrentNodeIndex(const int Value);
	void __fastcall SetShowPath(const bool Value);
	void __fastcall SetPathSplineMode(const Globjects::TGLLineSplineMode Value);
	
protected:
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	bool __fastcall CanTravel();
	TGLMovementPaths* __fastcall GetCollection();
	
public:
	__fastcall virtual TGLMovementPath(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMovementPath();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMovement* __fastcall GetMovement();
	TGLPathNode* __fastcall AddNode()/* overload */;
	TGLPathNode* __fastcall AddNode(TGLPathNode* const Node)/* overload */;
	TGLPathNode* __fastcall AddNodeFromObject(Glscene::TGLBaseSceneObject* const Obj);
	TGLPathNode* __fastcall InsertNodeFromObject(Glscene::TGLBaseSceneObject* const Obj, const int Index);
	TGLPathNode* __fastcall InsertNode(TGLPathNode* const Node, const int Index)/* overload */;
	TGLPathNode* __fastcall InsertNode(const int Index)/* overload */;
	TGLPathNode* __fastcall DeleteNode(const int Index)/* overload */;
	TGLPathNode* __fastcall DeleteNode(TGLPathNode* const Node)/* overload */;
	void __fastcall ClearNodes();
	void __fastcall UpdatePathLine();
	double __fastcall NodeDistance(TGLPathNode* const Node1, TGLPathNode* const Node2);
	void __fastcall CalculateState(const double CurrentTime);
	void __fastcall TravelPath(const bool Start)/* overload */;
	void __fastcall TravelPath(const bool Start, const double aStartTime)/* overload */;
	__property int NodeCount = {read=GetNodeCount, nodefault};
	__property TGLPathNode* CurrentNode = {read=FCurrentNode};
	__property bool InTravel = {read=FInTravel, nodefault};
	int __fastcall PrevNode();
	int __fastcall NextNode();
	__property int CurrentNodeIndex = {read=FCurrentNodeIndex, write=SetCurrentNodeIndex, nodefault};
	__property System::Classes::TNotifyEvent OnTravelStart = {read=FOnTravelStart, write=FOnTravelStart};
	__property System::Classes::TNotifyEvent OnTravelStop = {read=FOnTravelStop, write=FOnTravelStop};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property Globjects::TGLLineSplineMode PathSplineMode = {read=FPathSplineMode, write=SetPathSplineMode, default=0};
	__property TGLMovementRotationMode RotationMode = {read=FRotationMode, write=FRotationMode, default=0};
	__property double StartTime = {read=FStartTime, write=SetStartTime};
	__property double EstimateTime = {read=FEstimateTime};
	__property bool Looped = {read=FLooped, write=FLooped, nodefault};
	__property TGLPathNodes* Nodes = {read=FNodes};
	__property bool ShowPath = {read=FShowPath, write=SetShowPath, nodefault};
};


class PASCALIMPLEMENTATION TGLMovementPaths : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMovementPath* operator[](const int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(const int index, TGLMovementPath* const val);
	TGLMovementPath* __fastcall GetItems(const int index);
	TGLMovement* __fastcall GetMovement();
	
public:
	__fastcall TGLMovementPaths(TGLMovement* aOwner);
	HIDESBASE TGLMovementPath* __fastcall Add();
	HIDESBASE TGLMovementPath* __fastcall FindItemID(const int ID);
	__property TGLMovementPath* Items[const int index] = {read=GetItems, write=SetItems/*, default*/};
	virtual void __fastcall NotifyChange();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMovementPaths() { }
	
};


typedef void __fastcall (__closure *TPathTravelStartEvent)(System::TObject* Sender, TGLMovementPath* Path);

typedef void __fastcall (__closure *TPathTravelStopEvent)(System::TObject* Sender, TGLMovementPath* Path, bool &Looped);

class PASCALIMPLEMENTATION TGLMovement : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLMovementPaths* FPaths;
	bool FAutoStartNextPath;
	int FActivePathIndex;
	System::Classes::TNotifyEvent FOnAllPathTravelledOver;
	TPathTravelStartEvent FOnPathTravelStart;
	TPathTravelStopEvent FOnPathTravelStop;
	int __fastcall GetPathCount();
	void __fastcall SetActivePathIndex(int Value);
	TGLMovementPath* __fastcall GetActivePath();
	void __fastcall SetActivePath(TGLMovementPath* Value);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall PathTravelStart(System::TObject* Sender);
	void __fastcall PathTravelStop(System::TObject* Sender);
	Glscene::TGLBaseSceneObject* __fastcall GetSceneObject();
	
public:
	__fastcall virtual TGLMovement(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLMovement();
	TGLMovementPath* __fastcall AddPath()/* overload */;
	TGLMovementPath* __fastcall AddPath(Glscene::TGLBaseSceneObject* aObject)/* overload */;
	TGLMovementPath* __fastcall AddPath(TGLMovementPath* Path)/* overload */;
	void __fastcall ClearPaths();
	TGLMovementPath* __fastcall DeletePath(TGLMovementPath* Path)/* overload */;
	TGLMovementPath* __fastcall DeletePath(int Index)/* overload */;
	TGLMovementPath* __fastcall DeletePath()/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	void __fastcall StartPathTravel();
	void __fastcall StopPathTravel();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	int __fastcall NextPath();
	int __fastcall PrevPath();
	int __fastcall FirstPath();
	int __fastcall LastPath();
	__property int PathCount = {read=GetPathCount, nodefault};
	__property System::Classes::TNotifyEvent OnAllPathTravelledOver = {read=FOnAllPathTravelledOver, write=FOnAllPathTravelledOver};
	__property TPathTravelStartEvent OnPathTravelStart = {read=FOnPathTravelStart, write=FOnPathTravelStart};
	__property TPathTravelStopEvent OnPathTravelStop = {read=FOnPathTravelStop, write=FOnPathTravelStop};
	
__published:
	__property TGLMovementPaths* Paths = {read=FPaths};
	__property bool AutoStartNextPath = {read=FAutoStartNextPath, write=FAutoStartNextPath, nodefault};
	__property int ActivePathIndex = {read=FActivePathIndex, write=SetActivePathIndex, nodefault};
	__property TGLMovementPath* ActivePath = {read=GetActivePath, write=SetActivePath};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLMovement* __fastcall GetMovement(Glscene::TGLBehaviours* const behaviours)/* overload */;
extern DELPHI_PACKAGE TGLMovement* __fastcall GetMovement(Glscene::TGLBaseSceneObject* const obj)/* overload */;
extern DELPHI_PACKAGE TGLMovement* __fastcall GetOrCreateMovement(Glscene::TGLBehaviours* const behaviours)/* overload */;
extern DELPHI_PACKAGE TGLMovement* __fastcall GetOrCreateMovement(Glscene::TGLBaseSceneObject* const obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall StartAllMovements(Glscene::TGLScene* const Scene, const bool StartCamerasMove, const bool StartObjectsMove);
extern DELPHI_PACKAGE void __fastcall StopAllMovements(Glscene::TGLScene* const Scene, const bool StopCamerasMove, const bool StopObjectsMove);
}	/* namespace Glmovement */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMOVEMENT)
using namespace Glmovement;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmovementHPP
