﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLNavigator.pas' rev: 35.00 (Windows)

#ifndef GlnavigatorHPP
#define GlnavigatorHPP

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
#include <GLScene.hpp>
#include <GLCrossPlatform.hpp>
#include <GLCoordinates.hpp>
#include <GLScreen.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glnavigator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLNavigator;
class DELPHICLASS TGLUserInterface;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLNavigator : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glscene::TGLBaseSceneObject* FObject;
	Glvectortypes::TVector4f FVirtualRight;
	Glcoordinates::TGLCoordinates3* FVirtualUp;
	bool FUseVirtualUp;
	bool FAutoUpdateObject;
	float FMaxAngle;
	float FMinAngle;
	float FCurrentVAngle;
	float FCurrentHAngle;
	bool FAngleLock;
	bool FMoveUpWhenMovingForward;
	bool FInvertHorizontalSteeringWhenUpsideDown;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetObject(Glscene::TGLBaseSceneObject* NewObject);
	void __fastcall SetUseVirtualUp(bool UseIt);
	void __fastcall SetVirtualUp(Glcoordinates::TGLCoordinates3* Up);
	Glvectortypes::TVector4f __fastcall CalcRight();
	
public:
	__fastcall virtual TGLNavigator(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNavigator();
	void __fastcall TurnHorizontal(float Angle);
	void __fastcall TurnVertical(float Angle);
	void __fastcall MoveForward(float Distance);
	void __fastcall StrafeHorizontal(float Distance);
	void __fastcall StrafeVertical(float Distance);
	void __fastcall Straighten();
	void __fastcall FlyForward(float Distance);
	void __fastcall LoadState(System::Classes::TStream* Stream);
	void __fastcall SaveState(System::Classes::TStream* Stream);
	__property float CurrentVAngle = {read=FCurrentVAngle};
	__property float CurrentHAngle = {read=FCurrentHAngle};
	
__published:
	__property bool MoveUpWhenMovingForward = {read=FMoveUpWhenMovingForward, write=FMoveUpWhenMovingForward, default=0};
	__property bool InvertHorizontalSteeringWhenUpsideDown = {read=FInvertHorizontalSteeringWhenUpsideDown, write=FInvertHorizontalSteeringWhenUpsideDown, default=0};
	__property Glcoordinates::TGLCoordinates3* VirtualUp = {read=FVirtualUp, write=SetVirtualUp};
	__property Glscene::TGLBaseSceneObject* MovingObject = {read=FObject, write=SetObject};
	__property bool UseVirtualUp = {read=FUseVirtualUp, write=SetUseVirtualUp, default=0};
	__property bool AutoUpdateObject = {read=FAutoUpdateObject, write=FAutoUpdateObject, default=0};
	__property float MaxAngle = {read=FMaxAngle, write=FMaxAngle};
	__property float MinAngle = {read=FMinAngle, write=FMinAngle};
	__property bool AngleLock = {read=FAngleLock, write=FAngleLock, default=0};
};


class PASCALIMPLEMENTATION TGLUserInterface : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Types::TPoint FPrevPoint;
	int midScreenX;
	int midScreenY;
	bool FMouseActive;
	float FMouseSpeed;
	TGLNavigator* FGLNavigator;
	TGLNavigator* FGLVertNavigator;
	bool FInvertMouse;
	void __fastcall MouseInitialize();
	void __fastcall SetMouseLookActive(const bool val);
	void __fastcall setNavigator(TGLNavigator* val);
	void __fastcall setVertNavigator(TGLNavigator* val);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	
public:
	__fastcall virtual TGLUserInterface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLUserInterface();
	void __fastcall MouseUpdate();
	bool __fastcall MouseLook();
	void __fastcall MouseLookActiveToggle();
	void __fastcall MouseLookActivate();
	void __fastcall MouseLookDeactivate();
	bool __fastcall IsMouseLookOn();
	void __fastcall TurnHorizontal(double Angle);
	void __fastcall TurnVertical(double Angle);
	__property bool MouseLookActive = {read=FMouseActive, write=SetMouseLookActive, nodefault};
	
__published:
	__property bool InvertMouse = {read=FInvertMouse, write=FInvertMouse, default=0};
	__property float MouseSpeed = {read=FMouseSpeed, write=FMouseSpeed};
	__property TGLNavigator* GLNavigator = {read=FGLNavigator, write=setNavigator};
	__property TGLNavigator* GLVertNavigator = {read=FGLVertNavigator, write=setVertNavigator};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glnavigator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLNAVIGATOR)
using namespace Glnavigator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlnavigatorHPP