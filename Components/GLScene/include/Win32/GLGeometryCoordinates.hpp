// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGeometryCoordinates.pas' rev: 36.00 (Windows)

#ifndef GlgeometrycoordinatesHPP
#define GlgeometrycoordinatesHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgeometrycoordinates
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Cylindrical_Cartesian(const float r, const float theta, const float z1, float &x, float &y, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cylindrical_Cartesian(const double r, const double theta, const double z1, double &x, double &y, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cylindrical_Cartesian(const float r, const float theta, const float z1, float &x, float &y, float &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cylindrical_Cartesian(const double r, const double theta, const double z1, double &x, double &y, double &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cartesian_Cylindrical(const float x, const float y, const float z1, float &r, float &theta, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cartesian_Cylindrical(const double x, const double y, const double z1, double &r, double &theta, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Spherical_Cartesian(const float r, const float theta, const float phi, float &x, float &y, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Spherical_Cartesian(const double r, const double theta, const double phi, double &x, double &y, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall Spherical_Cartesian(const float r, const float theta, const float phi, float &x, float &y, float &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall Spherical_Cartesian(const double r, const double theta, const double phi, double &x, double &y, double &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cartesian_Spherical(const float x, const float y, const float z, float &r, float &theta, float &phi)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cartesian_Spherical(const Glvectorgeometry::TAffineVector &v, float &r, float &theta, float &phi)/* overload */;
extern DELPHI_PACKAGE void __fastcall Cartesian_Spherical(const double x, const double y, const double z, double &r, double &theta, double &phi)/* overload */;
extern DELPHI_PACKAGE void __fastcall ProlateSpheroidal_Cartesian(const float xi, const float eta, const float phi, const float a, float &x, float &y, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall ProlateSpheroidal_Cartesian(const double xi, const double eta, const double phi, const double a, double &x, double &y, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall ProlateSpheroidal_Cartesian(const float xi, const float eta, const float phi, const float a, float &x, float &y, float &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall ProlateSpheroidal_Cartesian(const double xi, const double eta, const double phi, const double a, double &x, double &y, double &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall OblateSpheroidal_Cartesian(const float xi, const float eta, const float phi, const float a, float &x, float &y, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall OblateSpheroidal_Cartesian(const double xi, const double eta, const double phi, const double a, double &x, double &y, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall OblateSpheroidal_Cartesian(const float xi, const float eta, const float phi, const float a, float &x, float &y, float &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall OblateSpheroidal_Cartesian(const double xi, const double eta, const double phi, const double a, double &x, double &y, double &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall BipolarCylindrical_Cartesian(const float u, const float v, const float z1, const float a, float &x, float &y, float &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall BipolarCylindrical_Cartesian(const double u, const double v, const double z1, const double a, double &x, double &y, double &z)/* overload */;
extern DELPHI_PACKAGE void __fastcall BipolarCylindrical_Cartesian(const float u, const float v, const float z1, const float a, float &x, float &y, float &z, int &ierr)/* overload */;
extern DELPHI_PACKAGE void __fastcall BipolarCylindrical_Cartesian(const double u, const double v, const double z1, const double a, double &x, double &y, double &z, int &ierr)/* overload */;
}	/* namespace Glgeometrycoordinates */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGEOMETRYCOORDINATES)
using namespace Glgeometrycoordinates;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgeometrycoordinatesHPP
