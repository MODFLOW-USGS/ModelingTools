{*******************************************************************************}
{                                                                               }
{      Newton Game Dynamics Delphi-Headertranslation                            }
{       Current SDK version 2.26 (Beta)                                         }
{                                                                               }
{      Copyright (c) 09,2010 Dmitriy "Executor" Bespalov                        }
{                            Stuart "Stucuk" Carey                              }
{                            Sascha Willems                                     }
{                                                                               }
{      Initial Author : Dmitriy "Executor" Bespalov                             }
{                                                                               }
{*******************************************************************************}
{                                                                               }
{ License :                                                                     }
{                                                                               }
{  The contents of this file are used with permission, subject to               }
{  the Mozilla Public License Version 1.1 (the "License"); you may              }
{  not use this file except in compliance with the License. You may             }
{  obtain a copy of the License at                                              }
{  http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                               }
{  Software distributed under the License is distributed on an                  }
{  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{  implied. See the License for the specific language governing                 }
{  rights and limitations under the License.                                    }
{                                                                               }
{*******************************************************************************}
{                                                                               }
{  See "Readme_NewtonImport.txt" for more information and detailed history      }
{                                                                               }
{*******************************************************************************}

unit NewtonImport_JointLibrary;

{$I PascalDefines.inc}

// Note: Declare the following in Projects->Options->Conditionals not in this unit! - Stucuk
//{$DEFINE NEWTON_DOUBLE_PRECISION} // This is needed when you want to use double precision

interface

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF UNIX}
  Types,
  Libc,
  Xlib,
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll,
{$ENDIF}
  System.Classes,
  NewtonImport;

const
{$IFDEF WIN32}
   JointLibraryDLL = 'dJointLibrary.dll';
{$ENDIF}
{$IFDEF WIN64}
   JointLibraryDLL = 'dJointLibrary64.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN} // MacOS X
  //JointLibraryDLL = 'libnewton.dylib';
{$ELSE}
  //JointLibraryDLL = 'libnewton.so';
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  //JointLibraryDLL = 'libnewton';
{$ENDIF}

type
// *****************************************************************************************************************************
//
//  JointLibrary Callbacks
//
// *****************************************************************************************************************************

NewtonUserJointDestructorCallback = procedure( const me : NewtonJoint ); cdecl;
PNewtonUserJointDestructorCallback = ^NewtonUserJointDestructorCallback;

NewtonUserJointSubmitConstraintCallback = procedure( const me : NewtonJoint; timestep : dFloat; threadIndex : integer ); cdecl;
PNewtonUserJointSubmitConstraintCallback = ^NewtonUserJointSubmitConstraintCallback;

BlankJointGetInfo = procedure( const me : NewtonJoint; info : NewtonJointRecord ); cdecl;
PBlankJointGetInfo = ^BlankJointGetInfo;

DGRaycastVehicleTireTransformCallback = procedure( car : NewtonJoint ); cdecl;
PDGRaycastVehicleTireTransformCallback = ^DGRaycastVehicleTireTransformCallback;

// *****************************************************************************************************************************
//
// JointLibrary functions
//
// *****************************************************************************************************************************

// generic joint functions
procedure CustomDestroyJoint( const joint : NewtonJoint ); cdecl; external{$IFDEF __GPC__}name 'CustomDestroyJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetNewtonJoint( const joint : NewtonJoint ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CustomGetNewtonJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetJointID( const joint : NewtonJoint ) : integer; cdecl; external{$IFDEF __GPC__}name 'CustomGetJointID'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetJointID( const joint : NewtonJoint; rttI : integer ); cdecl; external{$IFDEF __GPC__}name 'CustomSetJointID'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBody0( const joint : NewtonJoint ) : NewtonBody; cdecl; external{$IFDEF __GPC__}name 'CustomGetBody0'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBody1( const joint : NewtonJoint ) : NewtonBody; cdecl; external{$IFDEF __GPC__}name 'CustomGetBody1'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBodiesCollisionState( const joint : NewtonJoint ) : integer; cdecl; external{$IFDEF __GPC__}name 'CustomGetBodiesCollisionState'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetBodiesCollisionState( const joint : NewtonJoint; state : integer ); cdecl; external{$IFDEF __GPC__}name 'CustomSetBodiesCollisionState'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetUserData( const joint : NewtonJoint ) : Pointer; cdecl; external{$IFDEF __GPC__}name 'CustomGetUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetUserData( const joint : NewtonJoint; userData : Pointer ); cdecl; external{$IFDEF __GPC__}name 'CustomSetUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetDestructorCallback( const joint : NewtonJoint; callback : NewtonUserJointDestructorCallback ); cdecl; external{$IFDEF __GPC__}name 'CustomSetDestructorCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetSubmitContraintCallback( const joint : NewtonJoint; callback : NewtonUserJointSubmitConstraintCallback ); cdecl; external{$IFDEF __GPC__}name 'CustomSetSubmitContraintCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// this is a plain blank joint that can be used by advanced users who want to make their own joints
// but that can only use languages that can only interface with C code.
// we recommend using the CPP library to make the joints and then add a C interface, but this join is here for completion
function  CustomCreateBlankJoint( maxDof : integer; const body0 : NewtonBody; const body1 : NewtonBody; info : BlankJointGetInfo) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CustomCreateBlankJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Kinematic control joint
function CreateCustomKinematicController( const targetBody : NewtonBody; attachmentPointInGlobalSpace : PdFloat ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomKinematicController'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetPickMode( const pick : NewtonJoint; mode : integer); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetPickMode'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetMaxLinearFriction( const pick : NewtonJoint; accel : dFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetMaxLinearFriction'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetMaxAngularFriction( const pick : NewtonJoint; alpha : dFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetMaxAngularFriction'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetPosit( const pick : NewtonJoint; posit : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetPosit'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetRotation( const pick : NewtonJoint; rotation : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetRotation'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetMatrix( const pick : NewtonJoint; matrix : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerGetTargetMatrix( const pick : NewtonJoint; matrix : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerGetTargetMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Generic 6 degree of Freedom Joint
function  CreateCustomJoint6DOF( const pinsAndPivotChildFrame : PdFloat; const pinsAndPivotParentFrame : PdFloat; const child : NewtonBody; const parent : NewtonBody ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomJoint6DOF'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetLinearLimits( customJoint6DOF : NewtonJoint; const minLinearLimits : PdFloat; const maxLinearLimits : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetLinearLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetAngularLimits( customJoint6DOF : NewtonJoint; const minAngularLimits : PdFloat; const maxAngularLimits : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetAngularLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_GetLinearLimits( customJoint6DOF : NewtonJoint; minLinearLimits, maxLinearLimits : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_GetLinearLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_GetAngularLimits( customJoint6DOF : NewtonJoint; minAngularLimits, maxAngularLimits : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_GetAngularLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetReverseUniversal( customJoint6DOF : NewtonJoint; order : integer ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetReverseUniversal'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom BallAndSocket joint with Limits
function  CreateCustomBallAndSocket( const pinsAndPivotChildFrame : PdFloat; const child : NewtonBody; const parent : NewtonBody) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomBallAndSocket'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure BallAndSocketSetConeAngle( ballJoint : NewtonJoint; angle : dFloat ); cdecl; external{$IFDEF __GPC__}name 'BallAndSocketSetConeAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure BallAndSocketSetTwistAngle( ballJoint : NewtonJoint; minAngle, maxAngle : dFloat ); cdecl; external{$IFDEF __GPC__}name 'BallAndSocketSetTwistAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom Hinge joint with Limits
function  CreateCustomHinge( const pinsAndPivotChildFrame : PdFloat; const child : NewtonBody; const parent : NewtonBody ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomHinge'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure HingeEnableLimits( hingeJoint : NewtonJoint; state : integer ); cdecl; external{$IFDEF __GPC__}name 'HingeEnableLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure HingeSetLimits( hingeJoint : NewtonJoint; minAngle, maxAngle : dFloat ); cdecl; external{$IFDEF __GPC__}name 'HingeSetLimis'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Function added - Sw
function  HingeGetJointAngle (const hingeJoint : NewtonJoint) : dFloat; cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Procedure added - Sw
procedure HingeGetPinAxis (const hingeJoint : NewtonJoint; Pin : PdFloat); cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Function added - Sw
function  HingeCalculateJointOmega (const hingeJoint : NewtonJoint) : dFloat; cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom Slider joint with Limits
function  CreateCustomSlider( const pinsAndPivotChildFrame : PdFloat; const child : NewtonBody; const parent : NewtonBody ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomSlider'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure SliderEnableLimits( sliderJoint : NewtonJoint; state : integer ); cdecl; external{$IFDEF __GPC__}name 'SliderEnableLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure SliderSetLimits( sliderJoint : NewtonJoint; mindist, maxdist : dFloat ); cdecl; external{$IFDEF __GPC__}name 'SliderSetLimis'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// player controller functions
function  CreateCustomPlayerController( const pins : PdFloat; const player : NewtonBody; maxStairStepFactor, cushion : dFloat ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomPlayerController'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerSetVelocity( const playerController : NewtonJoint; forwardSpeed, sideSpeed, heading : dFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerSetVelocity'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerGetVisualMaTrix( const playerController : NewtonJoint; matrix : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetVisualMaTrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomPlayerControllerGetMaxSlope( const playerController : NewtonJoint ) : dFloat; cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetMaxSlope'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerSetMaxSlope( const playerController : NewtonJoint; maxSlopeAngleIndRadian : dFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerSetMaxSlope'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomPlayerControllerGetSensorShape( const playerController : NewtonJoint ) : NewtonCollision; cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetSensorShape'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// k00m (Dave Gravel simple ray cast world vehicle)
function  DGRaycastVehicleCreate( maxTireCount : integer; const cordenateSytemInLocalSpace : PdFloat; carBody : NewtonBody ) : NewtonJoint; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleCreate'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleAddTire( car : NewtonJoint; userData : Pointer; const localPosition : PdFloat; mass, radius, width, friction, suspensionLength, springConst, springDamper : dFloat; castMode : integer ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleAddTire'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleSetTireTransformCallback( car : NewtonJoint; callback : DGRaycastVehicleTireTransformCallback ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleSetTireTransformCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  DGRaycastVehicleGetTiresCount( car : NewtonJoint ) : integer; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTiresCount'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  DGRaycastVehicleGetTiresUserData( car : NewtonJoint; tireIndex : integer ) : Pointer; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTiresUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleGetTireMatrix( car : NewtonJoint; tireIndex : integer; tireMatrix : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTireMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

procedure DGRaycastVehicleInitNormalizeTireLateralForce( car : NewtonJoint; pointsCount : integer; piceSizeStepAxis : PdFloat; normalizedForceValue : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleInitNormalizeTireLateralForce'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleInitNormalizeTireLongitudinalForce( car : NewtonJoint; pointsCount : integer; piceSizeStepAxis : PdFloat; normalizedForceValue : PdFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleInitNormalizeTireLongitudinalForce'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

implementation

end.
