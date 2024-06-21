// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCameraController.pas' rev: 36.00 (Windows)

#ifndef GlcameracontrollerHPP
#define GlcameracontrollerHPP

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
#include <System.Contnrs.hpp>
#include <System.Types.hpp>
#include <GLScene.hpp>
#include <GLCoordinates.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLSmoothNavigator.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcameracontroller
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLCameraController;
class DELPHICLASS TGLCameraJobList;
class DELPHICLASS TGLCameraJob;
class DELPHICLASS TGLMoveToPosJob;
class DELPHICLASS TGLZoomToDistanceJob;
class DELPHICLASS TGLOrbitToPosJob;
class DELPHICLASS TGLSmoothOrbitToPos;
class DELPHICLASS TGLOrbitToPosAdvJob;
class DELPHICLASS TGLSmoothOrbitToPosAdvJob;
class DELPHICLASS TGLCameraController;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLCameraController : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCameraController(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCameraController() { }
	
};


class PASCALIMPLEMENTATION TGLCameraJobList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	TGLCameraJob* operator[](const int AIndex) { return this->Items[AIndex]; }
	
private:
	TGLCameraController* FController;
	TGLCameraJob* __fastcall GetCameraJob(const int AIndex);
	void __fastcall SetCameraJob(const int AIndex, TGLCameraJob* const Value);
	
public:
	__fastcall TGLCameraJobList(TGLCameraController* AController);
	HIDESBASE int __fastcall Add(TGLCameraJob* ACameraJob);
	__property TGLCameraJob* Items[const int AIndex] = {read=GetCameraJob, write=SetCameraJob/*, default*/};
	HIDESBASE TGLCameraJob* __fastcall First();
	HIDESBASE TGLCameraJob* __fastcall Last();
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLCameraJobList() { }
	
};


class PASCALIMPLEMENTATION TGLCameraJob : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLCameraJobList* FJoblist;
	
protected:
	bool FAbort;
	bool FInit;
	bool FRunning;
	double FElapsedTime;
	double FDeltaTime;
	double FStartTime;
	double FProceedTime;
	
public:
	__fastcall virtual TGLCameraJob(TGLCameraJobList* const AJoblist);
	__fastcall virtual ~TGLCameraJob();
	void __fastcall Abort();
	virtual void __fastcall Step() = 0 ;
	virtual void __fastcall Init() = 0 ;
	__property bool Running = {read=FRunning, write=FRunning, nodefault};
	__property double ElapsedTime = {read=FElapsedTime, write=FElapsedTime};
	__property double StartTime = {read=FStartTime, write=FStartTime};
	__property double ProceedTime = {read=FProceedTime, write=FProceedTime};
};


class PASCALIMPLEMENTATION TGLMoveToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Glvectorgeometry::TVector FInitialPos;
	Glvectorgeometry::TVector FFinalPos;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Glvectorgeometry::TVector InitialPos = {read=FInitialPos};
	__property Glvectorgeometry::TVector FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLMoveToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLMoveToPosJob() { }
	
};


class PASCALIMPLEMENTATION TGLZoomToDistanceJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Glvectorgeometry::TVector FInitialPos;
	Glvectorgeometry::TVector FFinalPos;
	
public:
	double Distance;
	double Time;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Glvectorgeometry::TVector InitialPos = {read=FInitialPos};
	__property Glvectorgeometry::TVector FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLZoomToDistanceJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLZoomToDistanceJob() { }
	
};


class PASCALIMPLEMENTATION TGLOrbitToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Glvectorgeometry::TVector FFinalPos;
	Glvectortypes::TVector2f FRotateSpeed;
	Glvectorgeometry::TVector FCameraUpVector;
	Glvectorgeometry::TVector FTargetPosition;
	double FTime;
	
public:
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Glvectortypes::TVector2f RotateSpeed = {read=FRotateSpeed};
	__property Glvectorgeometry::TVector CameraUpVector = {read=FCameraUpVector};
	__property Glvectorgeometry::TVector TargetPosition = {read=FTargetPosition};
	__property Glvectorgeometry::TVector FinalPos = {read=FFinalPos};
	__property double Time = {read=FTime};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosJob() { }
	
};


class PASCALIMPLEMENTATION TGLSmoothOrbitToPos : public TGLOrbitToPosJob
{
	typedef TGLOrbitToPosJob inherited;
	
private:
	float FCutoffAngle;
	bool FNeedToRecalculateZoom;
	Glvectorgeometry::TMatrix FShouldBeMatrix;
	Glsmoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	
public:
	__fastcall virtual TGLSmoothOrbitToPos(TGLCameraJobList* const AJoblist);
	virtual void __fastcall Step();
	__property float CutoffAngle = {read=FCutoffAngle, write=FCutoffAngle};
	__property bool NeedToRecalculateZoom = {read=FNeedToRecalculateZoom, write=FNeedToRecalculateZoom, nodefault};
public:
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPos() { }
	
};


class PASCALIMPLEMENTATION TGLOrbitToPosAdvJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Glvectorgeometry::TVector FInitialPos;
	Glvectorgeometry::TVector FFinalPos;
	Glvectorgeometry::TVector FInitialUp;
	Glvectorgeometry::TVector FInitialDir;
	Glvectorgeometry::TVector FRotAxis;
	double FAngle;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	bool PreferUpAxis;
	virtual void __fastcall Step();
	virtual void __fastcall Init();
	__property Glvectorgeometry::TVector InitialPos = {read=FInitialPos};
	__property Glvectorgeometry::TVector InitialUp = {read=FInitialUp};
	__property Glvectorgeometry::TVector InitialDir = {read=FInitialDir};
	__property Glvectorgeometry::TVector FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosAdvJob() { }
	
};


class PASCALIMPLEMENTATION TGLSmoothOrbitToPosAdvJob : public TGLOrbitToPosAdvJob
{
	typedef TGLOrbitToPosAdvJob inherited;
	
private:
	Glvectorgeometry::TVector FPreviousPosition;
	Glsmoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	bool FRestoreUpVector;
	
public:
	virtual void __fastcall Step();
	virtual void __fastcall Init();
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLSmoothOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLOrbitToPosAdvJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPosAdvJob() { }
	
};


typedef void __fastcall (__closure *TGLCameraJobEvent)(TGLCameraJob* Sender);

class PASCALIMPLEMENTATION TGLCameraController : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLCameraJobList* FCameraJobList;
	Glscene::TGLBaseSceneObject* FCamera;
	Glscene::TGLBaseSceneObject* FCameraTarget;
	TGLCameraJobEvent FOnJobAdded;
	TGLCameraJobEvent FOnJobFinished;
	TGLCameraJobEvent FOnJobStep;
	double FsoSafeDist;
	double FsoTimeToSafePlacement;
	double FsoTimeToOrbit;
	double FsoTimeToZoomBackIn;
	void __fastcall CheckAssignments(bool Extended);
	void __fastcall SetOnJobAdded(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobFinished(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobStep(const TGLCameraJobEvent Value);
	void __fastcall SetCamera(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetCameraTarget(Glscene::TGLBaseSceneObject* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCameraController(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCameraController();
	TGLMoveToPosJob* __fastcall MoveToPos(double x, double y, double z, double time);
	TGLOrbitToPosJob* __fastcall OrbitToPos(double x, double y, double z, double time);
	TGLSmoothOrbitToPos* __fastcall OrbitToPosSmooth(const Glvectorgeometry::TVector &ATargetPosition, const double ATime, Glsmoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool AFNeedToRecalculateZoom, const Glvectorgeometry::PVector ACameraUpVector = (Glvectorgeometry::PVector)(0x0));
	TGLOrbitToPosAdvJob* __fastcall OrbitToPosAdvanced(double x, double y, double z, double time, bool PreferUpAxis = true);
	TGLSmoothOrbitToPosAdvJob* __fastcall OrbitToPosAdvancedSmooth(const double x, const double y, const double z, const double time, Glsmoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool PreferUpAxis = true);
	TGLZoomToDistanceJob* __fastcall ZoomToDistance(double Distance, double Time);
	void __fastcall SafeOrbitAndZoomToPos(double x, double y, double z);
	void __fastcall StopMovement();
	void __fastcall Step(const double deltaTime, const double newTime);
	__property TGLCameraJobList* CameraJobList = {read=FCameraJobList};
	
__published:
	__property Glscene::TGLBaseSceneObject* Camera = {read=FCamera, write=SetCamera};
	__property Glscene::TGLBaseSceneObject* CameraTarget = {read=FCameraTarget, write=SetCameraTarget};
	__property double soSafeDistance = {read=FsoSafeDist, write=FsoSafeDist};
	__property double soTimeToSafePlacement = {read=FsoTimeToSafePlacement, write=FsoTimeToSafePlacement};
	__property double soTimeToOrbit = {read=FsoTimeToOrbit, write=FsoTimeToOrbit};
	__property double soTimeToZoomBackIn = {read=FsoTimeToZoomBackIn, write=FsoTimeToZoomBackIn};
	__property TGLCameraJobEvent OnJobAdded = {read=FOnJobAdded, write=SetOnJobAdded};
	__property TGLCameraJobEvent OnJobStep = {read=FOnJobStep, write=SetOnJobStep};
	__property TGLCameraJobEvent OnJobFinished = {read=FOnJobFinished, write=SetOnJobFinished};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcameracontroller */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCAMERACONTROLLER)
using namespace Glcameracontroller;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcameracontrollerHPP
