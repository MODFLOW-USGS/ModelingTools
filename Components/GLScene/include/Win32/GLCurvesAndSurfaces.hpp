﻿// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCurvesAndSurfaces.pas' rev: 35.00 (Windows)

#ifndef GlcurvesandsurfacesHPP
#define GlcurvesandsurfacesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcurvesandsurfaces
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBSplineContinuity : unsigned char { bscUniformNonPeriodic, bscUniformPeriodic };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall BezierCurvePoint(float t, int n, Glvectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall BezierSurfacePoint(float s, float t, int m, int n, Glvectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE void __fastcall GenerateBezierCurve(int Steps, Glvectorlists::TAffineVectorList* ControlPoints, Glvectorlists::TAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateBezierSurface(int Steps, int Width, int Height, Glvectorlists::TAffineVectorList* ControlPoints, Glvectorlists::TAffineVectorList* Vertices);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall BSplinePoint(float t, int n, int k, Glvectorgeometry::PFloatVector knots, Glvectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE Glvectortypes::TVector3f __fastcall BSplineSurfacePoint(float s, float t, int m, int n, int k1, int k2, Glvectorgeometry::PFloatVector uknots, Glvectorgeometry::PFloatVector vknots, Glvectorgeometry::PAffineVectorArray cp);
extern DELPHI_PACKAGE void __fastcall GenerateBSpline(int Steps, int Order, Glvectorlists::TSingleList* KnotVector, Glvectorlists::TAffineVectorList* ControlPoints, Glvectorlists::TAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateBSplineSurface(int Steps, int UOrder, int VOrder, int Width, int Height, Glvectorlists::TSingleList* UKnotVector, Glvectorlists::TSingleList* VKnotVector, Glvectorlists::TAffineVectorList* ControlPoints, Glvectorlists::TAffineVectorList* Vertices);
extern DELPHI_PACKAGE void __fastcall GenerateKnotVector(Glvectorlists::TSingleList* KnotVector, int NumberOfPoints, int Order, TBSplineContinuity Continuity);
}	/* namespace Glcurvesandsurfaces */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCURVESANDSURFACES)
using namespace Glcurvesandsurfaces;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcurvesandsurfacesHPP