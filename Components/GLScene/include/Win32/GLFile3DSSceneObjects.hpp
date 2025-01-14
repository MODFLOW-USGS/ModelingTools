// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFile3DSSceneObjects.pas' rev: 36.00 (Windows)

#ifndef Glfile3dssceneobjectsHPP
#define Glfile3dssceneobjectsHPP

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
#include <OpenGLAdapter.hpp>
#include <GLVectorGeometry.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLScene.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorTypes.hpp>
#include <GLPersistentClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfile3dssceneobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFile3DSLight;
class DELPHICLASS TGLFile3DSCamera;
class DELPHICLASS TGLFile3DSActor;
class DELPHICLASS TGLFile3DSFreeForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLFile3DSLight : public Glscene::TGLLightSource
{
	typedef Glscene::TGLLightSource inherited;
	
private:
	Glcoordinates::TGLCoordinates* FTargetPos;
	float FHotSpot;
	float FMultipler;
	
public:
	__fastcall virtual TGLFile3DSLight(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	__fastcall virtual ~TGLFile3DSLight();
	
__published:
	__property Glcoordinates::TGLCoordinates* SpotTargetPos = {read=FTargetPos};
	__property float HotSpot = {read=FHotSpot, write=FHotSpot};
	__property float Multipler = {read=FMultipler, write=FMultipler};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSLight(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLLightSource(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSCamera : public Glscene::TGLCamera
{
	typedef Glscene::TGLCamera inherited;
	
private:
	Glcoordinates::TGLCoordinates* FTargetPos;
	System::StaticArray<Opengltokens::PGLUQuadric, 2> FQuadCyl;
	System::StaticArray<Opengltokens::PGLUQuadric, 2> FQuadDisk;
	
public:
	__fastcall virtual TGLFile3DSCamera(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	__fastcall virtual ~TGLFile3DSCamera();
	
__published:
	__property Glcoordinates::TGLCoordinates* CameraTargetPos = {read=FTargetPos};
	__property RollAngle = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSCamera(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCamera(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSActor : public Glvectorfileobjects::TGLActor
{
	typedef Glvectorfileobjects::TGLActor inherited;
	
private:
	void __fastcall ReadMesh(System::Classes::TStream* Stream);
	void __fastcall WriteMesh(System::Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
public:
	/* TGLActor.Create */ inline __fastcall virtual TGLFile3DSActor(System::Classes::TComponent* aOwner) : Glvectorfileobjects::TGLActor(aOwner) { }
	/* TGLActor.Destroy */ inline __fastcall virtual ~TGLFile3DSActor() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSActor(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLActor(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSFreeForm : public Glvectorfileobjects::TGLFreeForm
{
	typedef Glvectorfileobjects::TGLFreeForm inherited;
	
private:
	Glvectorgeometry::TMatrix FTransfMat;
	Glvectorgeometry::TMatrix FScaleMat;
	Glvectorgeometry::TMatrix ParentMatrix;
	Glcoordinates::TGLCoordinates4* FS_Rot3DS;
	Glcoordinates::TGLCoordinates4* FRot3DS;
	Glcoordinates::TGLCoordinates4* FScale3DS;
	void __fastcall ReadMesh(System::Classes::TStream* Stream);
	void __fastcall WriteMesh(System::Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	Glvectorgeometry::TMatrix FRefMat;
	__fastcall virtual TGLFile3DSFreeForm(System::Classes::TComponent* AOWner);
	__fastcall virtual ~TGLFile3DSFreeForm();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual Glvectorgeometry::TVector __fastcall BarycenterAbsolutePosition();
	
__published:
	__property Glcoordinates::TGLCoordinates4* S_Rot3DS = {read=FS_Rot3DS};
	__property Glcoordinates::TGLCoordinates4* Rot3DS = {read=FRot3DS};
	__property Glcoordinates::TGLCoordinates4* Scale3DS = {read=FScale3DS};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSFreeForm(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLFreeForm(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vGLFile3DSSceneObjects_RenderCameraAndLights;
}	/* namespace Glfile3dssceneobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILE3DSSCENEOBJECTS)
using namespace Glfile3dssceneobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfile3dssceneobjectsHPP
