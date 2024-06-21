// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSmoothNavigator.pas' rev: 36.00 (Windows)

#ifndef GlsmoothnavigatorHPP
#define GlsmoothnavigatorHPP

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
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorTypes.hpp>
#include <GLNavigator.hpp>
#include <GLVectorGeometry.hpp>
#include <GLCoordinates.hpp>
#include <GLScreen.hpp>
#include <GLXCollection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsmoothnavigator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLNavigatorAbstractParameters;
class DELPHICLASS TGLNavigatorSmoothChangeItem;
class DELPHICLASS TGLNavigatorSmoothChangeSingle;
class DELPHICLASS TGLNavigatorSmoothChangeVector;
class DELPHICLASS TGLNavigatorSmoothChangeItems;
class DELPHICLASS TGLNavigatorAdjustDistanceParameters;
class DELPHICLASS TGLNavigatorAdjustDistanceParametersEx;
class DELPHICLASS TGLNavigatorInertiaParameters;
class DELPHICLASS TGLNavigatorGeneralParameters;
class DELPHICLASS TGLNavigatorMoveAroundParameters;
class DELPHICLASS TGLSmoothNavigator;
class DELPHICLASS TGLSmoothUserInterface;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAbstractParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float FInertia;
	float FSpeed;
	float FCutoff;
	bool __fastcall StoreCutoff();
	
protected:
	virtual bool __fastcall StoreInertia();
	virtual bool __fastcall StoreSpeed();
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall virtual TGLNavigatorAbstractParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float Speed = {read=FSpeed, write=FSpeed, stored=StoreSpeed};
	__property float Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAbstractParameters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeItem : public Glxcollection::TXCollectionItem
{
	typedef Glxcollection::TXCollectionItem inherited;
	
private:
	float FInertia;
	float FSpeed;
	bool FEnabled;
	float FSpeedLimit;
	double FCutoff;
	bool __fastcall StoreInertia();
	bool __fastcall StoreSpeed();
	bool __fastcall StoreSpeedLimit();
	bool __fastcall StoreCutoff();
	
protected:
	TGLSmoothNavigator* __fastcall GetNavigator();
	
public:
	virtual bool __fastcall Proceed(double ADeltaTime) = 0 ;
	__fastcall virtual TGLNavigatorSmoothChangeItem(Glxcollection::TXCollection* aOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall ResetTargetValue() = 0 ;
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float Speed = {read=FSpeed, write=FSpeed, stored=StoreSpeed};
	__property float SpeedLimit = {read=FSpeedLimit, write=FSpeedLimit, stored=StoreSpeedLimit};
	__property double Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeItem() { }
	
};


typedef float __fastcall (__closure *TGLNavigatorSmoothChangeSingleGetEvent)(TGLNavigatorSmoothChangeSingle* const ASender);

typedef void __fastcall (__closure *TGLNavigatorSmoothChangeSingleSetEvent)(TGLNavigatorSmoothChangeSingle* const ASender, const float AValue);

class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeSingle : public TGLNavigatorSmoothChangeItem
{
	typedef TGLNavigatorSmoothChangeItem inherited;
	
private:
	float FTargetValue;
	TGLNavigatorSmoothChangeSingleGetEvent FOnGetCurrentValue;
	TGLNavigatorSmoothChangeSingleSetEvent FOnSetCurrentValue;
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual bool __fastcall Proceed(double ADeltaTime);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ResetTargetValue();
	
__published:
	__property float TargetValue = {read=FTargetValue, write=FTargetValue};
	__property TGLNavigatorSmoothChangeSingleGetEvent OnGetCurrentValue = {read=FOnGetCurrentValue, write=FOnGetCurrentValue};
	__property TGLNavigatorSmoothChangeSingleSetEvent OnSetCurrentValue = {read=FOnSetCurrentValue, write=FOnSetCurrentValue};
public:
	/* TGLNavigatorSmoothChangeItem.Create */ inline __fastcall virtual TGLNavigatorSmoothChangeSingle(Glxcollection::TXCollection* aOwner) : TGLNavigatorSmoothChangeItem(aOwner) { }
	
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeSingle() { }
	
};


typedef Glvectorgeometry::TVector __fastcall (__closure *TGLNavigatorSmoothChangeVectorGetEvent)(TGLNavigatorSmoothChangeVector* const ASender);

typedef void __fastcall (__closure *TGLNavigatorSmoothChangeVectorSetEvent)(TGLNavigatorSmoothChangeVector* const ASender, const Glvectorgeometry::TVector &AValue);

class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeVector : public TGLNavigatorSmoothChangeItem
{
	typedef TGLNavigatorSmoothChangeItem inherited;
	
private:
	Glcoordinates::TGLCoordinates* FTargetValue;
	TGLNavigatorSmoothChangeVectorGetEvent FOnGetCurrentValue;
	TGLNavigatorSmoothChangeVectorSetEvent FOnSetCurrentValue;
	void __fastcall SetTargetValue(Glcoordinates::TGLCoordinates* const Value);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual bool __fastcall Proceed(double ADeltaTime);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLNavigatorSmoothChangeVector(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLNavigatorSmoothChangeVector();
	virtual void __fastcall ResetTargetValue();
	
__published:
	__property Glcoordinates::TGLCoordinates* TargetValue = {read=FTargetValue, write=SetTargetValue};
	__property TGLNavigatorSmoothChangeVectorGetEvent OnGetCurrentValue = {read=FOnGetCurrentValue, write=FOnGetCurrentValue};
	__property TGLNavigatorSmoothChangeVectorSetEvent OnSetCurrentValue = {read=FOnSetCurrentValue, write=FOnSetCurrentValue};
};


typedef System::TMetaClass* TGLNavigatorSmoothChangeItemClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeItems : public Glxcollection::TXCollection
{
	typedef Glxcollection::TXCollection inherited;
	
public:
	TGLNavigatorSmoothChangeItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TGLNavigatorSmoothChangeItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLNavigatorSmoothChangeItem* const Value);
	
protected:
	void __fastcall DoProceed(double ADeltaTime);
	
public:
	HIDESBASE TGLNavigatorSmoothChangeItem* __fastcall Add(TGLNavigatorSmoothChangeItemClass AClass);
	virtual bool __fastcall CanAdd(Glxcollection::TXCollectionItemClass AClass);
	__classmethod virtual Glxcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLNavigatorSmoothChangeItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLNavigatorSmoothChangeItems(System::Classes::TPersistent* aOwner) : Glxcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeItems() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAdjustDistanceParameters : public TGLNavigatorAbstractParameters
{
	typedef TGLNavigatorAbstractParameters inherited;
	
private:
	float FOldDistanceRatio;
	float FImpulseSpeed;
	bool __fastcall StoreImpulseSpeed();
	
public:
	__fastcall virtual TGLNavigatorAdjustDistanceParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall AddImpulse(const float Impulse);
	
__published:
	__property float ImpulseSpeed = {read=FImpulseSpeed, write=FImpulseSpeed, stored=StoreImpulseSpeed};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAdjustDistanceParameters() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAdjustDistanceParametersEx : public TGLNavigatorAbstractParameters
{
	typedef TGLNavigatorAbstractParameters inherited;
	
private:
	float FSpeedLimit;
	float FTargetDistance;
	bool __fastcall StoreSpeedLimit();
	bool __fastcall StoreTargetDistance();
	
protected:
	virtual bool __fastcall StoreSpeed();
	virtual bool __fastcall StoreInertia();
	
public:
	__fastcall virtual TGLNavigatorAdjustDistanceParametersEx(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float TargetDistance = {read=FTargetDistance, write=FTargetDistance, stored=StoreTargetDistance};
	__property float SpeedLimit = {read=FSpeedLimit, write=FSpeedLimit, stored=StoreSpeedLimit};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAdjustDistanceParametersEx() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorInertiaParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float OldTurnHorizontalAngle;
	float OldTurnVerticalAngle;
	float OldMoveForwardDistance;
	float OldStrafeHorizontalDistance;
	float OldStrafeVerticalDistance;
	float FTurnInertia;
	float FTurnSpeed;
	float FTurnMaxAngle;
	float FMovementAcceleration;
	float FMovementInertia;
	float FMovementSpeed;
	bool __fastcall StoreTurnMaxAngle();
	bool __fastcall StoreMovementAcceleration();
	bool __fastcall StoreMovementInertia();
	bool __fastcall StoreMovementSpeed();
	bool __fastcall StoreTurnInertia();
	bool __fastcall StoreTurnSpeed();
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall virtual TGLNavigatorInertiaParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float MovementAcceleration = {read=FMovementAcceleration, write=FMovementAcceleration, stored=StoreMovementAcceleration};
	__property float MovementInertia = {read=FMovementInertia, write=FMovementInertia, stored=StoreMovementInertia};
	__property float MovementSpeed = {read=FMovementSpeed, write=FMovementSpeed, stored=StoreMovementSpeed};
	__property float TurnMaxAngle = {read=FTurnMaxAngle, write=FTurnMaxAngle, stored=StoreTurnMaxAngle};
	__property float TurnInertia = {read=FTurnInertia, write=FTurnInertia, stored=StoreTurnInertia};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed, stored=StoreTurnSpeed};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorInertiaParameters() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorGeneralParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float FAutoScaleMin;
	float FAutoScaleMax;
	float FAutoScaleMult;
	bool __fastcall StoreAutoScaleMax();
	bool __fastcall StoreAutoScaleMin();
	bool __fastcall StoreAutoScaleMult();
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall virtual TGLNavigatorGeneralParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float AutoScaleMin = {read=FAutoScaleMin, write=FAutoScaleMin, stored=StoreAutoScaleMin};
	__property float AutoScaleMax = {read=FAutoScaleMax, write=FAutoScaleMax, stored=StoreAutoScaleMax};
	__property float AutoScaleMult = {read=FAutoScaleMult, write=FAutoScaleMult, stored=StoreAutoScaleMult};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorGeneralParameters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLNavigatorMoveAroundParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	Glscene::TGLBaseSceneObject* FTargetObject;
	float FOldPitchInertiaAngle;
	float FOldTurnInertiaAngle;
	float FPitchSpeed;
	float FTurnSpeed;
	float FInertia;
	float FMaxAngle;
	double FCutoff;
	bool __fastcall StoreInertia();
	bool __fastcall StoreMaxAngle();
	bool __fastcall StorePitchSpeed();
	bool __fastcall StoreTurnSpeed();
	void __fastcall SetTargetObject(Glscene::TGLBaseSceneObject* const Value);
	bool __fastcall StoreCutoff();
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall virtual TGLNavigatorMoveAroundParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float MaxAngle = {read=FMaxAngle, write=FMaxAngle, stored=StoreMaxAngle};
	__property float PitchSpeed = {read=FPitchSpeed, write=FPitchSpeed, stored=StorePitchSpeed};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed, stored=StoreTurnSpeed};
	__property Glscene::TGLBaseSceneObject* TargetObject = {read=FTargetObject, write=SetTargetObject};
	__property double Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorMoveAroundParameters() { }
	
};


class PASCALIMPLEMENTATION TGLSmoothNavigator : public Glnavigator::TGLNavigator
{
	typedef Glnavigator::TGLNavigator inherited;
	
private:
	double FMaxExpectedDeltaTime;
	TGLNavigatorInertiaParameters* FInertiaParams;
	TGLNavigatorGeneralParameters* FGeneralParams;
	TGLNavigatorMoveAroundParameters* FMoveAroundParams;
	TGLNavigatorAdjustDistanceParameters* FAdjustDistanceParams;
	TGLNavigatorAdjustDistanceParametersEx* FAdjustDistanceParamsEx;
	TGLNavigatorSmoothChangeItems* FCustomAnimatedItems;
	void __fastcall SetInertiaParams(TGLNavigatorInertiaParameters* const Value);
	bool __fastcall StoreMaxExpectedDeltaTime();
	void __fastcall SetGeneralParams(TGLNavigatorGeneralParameters* const Value);
	void __fastcall SetMoveAroundParams(TGLNavigatorMoveAroundParameters* const Value);
	void __fastcall SetAdjustDistanceParams(TGLNavigatorAdjustDistanceParameters* const Value);
	void __fastcall SetAdjustDistanceParamsEx(TGLNavigatorAdjustDistanceParametersEx* const Value);
	void __fastcall SetCustomAnimatedItems(TGLNavigatorSmoothChangeItems* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSmoothNavigator(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSmoothNavigator();
	virtual void __fastcall SetObject(Glscene::TGLBaseSceneObject* Value);
	HIDESBASE virtual void __fastcall TurnHorizontal(float Angle, double ADeltaTime);
	HIDESBASE virtual void __fastcall TurnVertical(float Angle, double ADeltaTime);
	HIDESBASE virtual void __fastcall FlyForward(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall MoveForward(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall StrafeHorizontal(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall StrafeVertical(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	bool __fastcall MoveAroundTarget(const float PitchDelta, const float TurnDelta, const double ADeltaTime);
	bool __fastcall MoveObjectAround(Glscene::TGLBaseSceneObject* const AObject, float PitchDelta, float TurnDelta, double ADeltaTime);
	bool __fastcall AdjustDistanceToPoint(const Glvectorgeometry::TVector &APoint, const float DistanceRatio, double ADeltaTime);
	bool __fastcall AdjustDistanceToTarget(const float DistanceRatio, const double ADeltaTime);
	bool __fastcall AdjustDistanceToPointEx(const Glvectorgeometry::TVector &APoint, double ADeltaTime);
	bool __fastcall AdjustDistanceToTargetEx(const double ADeltaTime);
	virtual void __fastcall AnimateCustomItems(const double ADeltaTime);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall AutoScaleParameters(const float FPS);
	virtual void __fastcall AutoScaleParametersUp(const float FPS);
	
__published:
	__property double MaxExpectedDeltaTime = {read=FMaxExpectedDeltaTime, write=FMaxExpectedDeltaTime, stored=StoreMaxExpectedDeltaTime};
	__property TGLNavigatorInertiaParameters* InertiaParams = {read=FInertiaParams, write=SetInertiaParams};
	__property TGLNavigatorGeneralParameters* GeneralParams = {read=FGeneralParams, write=SetGeneralParams};
	__property TGLNavigatorMoveAroundParameters* MoveAroundParams = {read=FMoveAroundParams, write=SetMoveAroundParams};
	__property TGLNavigatorAdjustDistanceParameters* AdjustDistanceParams = {read=FAdjustDistanceParams, write=SetAdjustDistanceParams};
	__property TGLNavigatorAdjustDistanceParametersEx* AdjustDistanceParamsEx = {read=FAdjustDistanceParamsEx, write=SetAdjustDistanceParamsEx};
	__property TGLNavigatorSmoothChangeItems* CustomAnimatedItems = {read=FCustomAnimatedItems, write=SetCustomAnimatedItems};
};


class PASCALIMPLEMENTATION TGLSmoothUserInterface : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FAutoUpdateMouse;
	bool FMouseLookActive;
	TGLSmoothNavigator* FSmoothNavigator;
	TGLSmoothNavigator* FSmoothVertNavigator;
	bool FInvertMouse;
	Glcoordinates::TGLCoordinates2* FOriginalMousePos;
	virtual void __fastcall SetSmoothNavigator(TGLSmoothNavigator* const Value);
	virtual void __fastcall SetOriginalMousePos(Glcoordinates::TGLCoordinates2* const Value);
	virtual void __fastcall SetSmoothVertNavigator(TGLSmoothNavigator* const Value);
	virtual void __fastcall SetMouseLookActive(const bool Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSmoothUserInterface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSmoothUserInterface();
	virtual void __fastcall TurnHorizontal(const float Angle, const double ADeltaTime);
	virtual void __fastcall TurnVertical(const float Angle, const double ADeltaTime);
	virtual void __fastcall MouseLookActiveToggle();
	bool __fastcall MouseLook(const double ADeltaTime)/* overload */;
	bool __fastcall MouseLook(const System::Types::TPoint &NewXY, const double ADeltaTime)/* overload */;
	bool __fastcall MouseLook(const int NewX, const int NewY, const double ADeltaTime)/* overload */;
	
__published:
	__property bool AutoUpdateMouse = {read=FAutoUpdateMouse, write=FAutoUpdateMouse, default=1};
	__property bool MouseLookActive = {read=FMouseLookActive, write=SetMouseLookActive, default=0};
	__property TGLSmoothNavigator* SmoothVertNavigator = {read=FSmoothVertNavigator, write=SetSmoothVertNavigator};
	__property TGLSmoothNavigator* SmoothNavigator = {read=FSmoothNavigator, write=SetSmoothNavigator};
	__property bool InvertMouse = {read=FInvertMouse, write=FInvertMouse, default=0};
	__property Glcoordinates::TGLCoordinates2* OriginalMousePos = {read=FOriginalMousePos, write=SetOriginalMousePos};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsmoothnavigator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMOOTHNAVIGATOR)
using namespace Glsmoothnavigator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmoothnavigatorHPP
