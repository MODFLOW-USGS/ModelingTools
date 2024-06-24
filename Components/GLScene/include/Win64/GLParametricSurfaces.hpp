// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glparametricsurfaces.pas' rev: 36.00 (Windows)

#ifndef GlparametricsurfacesHPP
#define GlparametricsurfacesHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Opengltokens.hpp>
#include <Opengladapter.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glcurvesandsurfaces.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glpersistentclasses.hpp>
#include <Gltexture.hpp>
#include <Glstate.hpp>
#include <Glcontext.hpp>
#include <Glrendercontextinfo.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glparametricsurfaces
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMOParametricSurface;
class DELPHICLASS TFGBezierSurface;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TParametricSurfaceRenderer : unsigned char { psrGLScene, psrOpenGL };

enum DECLSPEC_DENUM TParametricSurfaceBasis : unsigned char { psbBezier, psbBSpline };

class PASCALIMPLEMENTATION TMOParametricSurface : public Glvectorfileobjects::TMeshObject
{
	typedef Glvectorfileobjects::TMeshObject inherited;
	
private:
	Glvectorlists::TAffineVectorList* FControlPoints;
	Glvectorlists::TAffineVectorList* FWeightedControlPoints;
	Glvectorlists::TSingleList* FKnotsU;
	Glvectorlists::TSingleList* FKnotsV;
	Glvectorlists::TSingleList* FWeights;
	int FOrderU;
	int FOrderV;
	int FCountU;
	int FCountV;
	int FResolution;
	bool FAutoKnots;
	Glcurvesandsurfaces::TBSplineContinuity FContinuity;
	TParametricSurfaceRenderer FRenderer;
	TParametricSurfaceBasis FBasis;
	void __fastcall SetControlPoints(Glvectorlists::TAffineVectorList* Value);
	void __fastcall SetKnotsU(Glvectorlists::TSingleList* Value);
	void __fastcall SetKnotsV(Glvectorlists::TSingleList* Value);
	void __fastcall SetWeights(Glvectorlists::TSingleList* Value);
	void __fastcall SetRenderer(TParametricSurfaceRenderer Value);
	void __fastcall SetBasis(TParametricSurfaceBasis Value);
	
public:
	__fastcall virtual TMOParametricSurface();
	__fastcall virtual ~TMOParametricSurface();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	virtual void __fastcall Clear();
	void __fastcall GenerateMesh();
	__property Glvectorlists::TAffineVectorList* ControlPoints = {read=FControlPoints, write=SetControlPoints};
	__property Glvectorlists::TSingleList* KnotsU = {read=FKnotsU, write=SetKnotsU};
	__property Glvectorlists::TSingleList* KnotsV = {read=FKnotsV, write=SetKnotsV};
	__property Glvectorlists::TSingleList* Weights = {read=FWeights, write=SetWeights};
	__property int OrderU = {read=FOrderU, write=FOrderU, nodefault};
	__property int OrderV = {read=FOrderV, write=FOrderV, nodefault};
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property bool AutoKnots = {read=FAutoKnots, write=FAutoKnots, nodefault};
	__property Glcurvesandsurfaces::TBSplineContinuity Continuity = {read=FContinuity, write=FContinuity, nodefault};
	__property TParametricSurfaceRenderer Renderer = {read=FRenderer, write=SetRenderer, nodefault};
	__property TParametricSurfaceBasis Basis = {read=FBasis, write=SetBasis, nodefault};
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TMOParametricSurface(Glvectorfileobjects::TGLMeshObjectList* AOwner) : Glvectorfileobjects::TMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMOParametricSurface(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMeshObject(reader) { }
	
};


class PASCALIMPLEMENTATION TFGBezierSurface : public Glvectorfileobjects::TGLFaceGroup
{
	typedef Glvectorfileobjects::TGLFaceGroup inherited;
	
private:
	int FCountU;
	int FCountV;
	Glvectorlists::TIntegerList* FControlPointIndices;
	Glvectorlists::TIntegerList* FTexCoordIndices;
	int FResolution;
	float FMinU;
	float FMaxU;
	float FMinV;
	float FMaxV;
	Glvectorlists::TAffineVectorList* FTempControlPoints;
	Glvectorlists::TAffineVectorList* FTempTexCoords;
	
protected:
	void __fastcall SetControlPointIndices(Glvectorlists::TIntegerList* const Value);
	void __fastcall SetTexCoordIndices(Glvectorlists::TIntegerList* const Value);
	
public:
	__fastcall virtual TFGBezierSurface();
	__fastcall virtual ~TFGBezierSurface();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &mrci);
	virtual void __fastcall Prepare();
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property float MinU = {read=FMinU, write=FMinU};
	__property float MaxU = {read=FMaxU, write=FMaxU};
	__property float MinV = {read=FMinV, write=FMinV};
	__property float MaxV = {read=FMaxV, write=FMaxV};
	__property Glvectorlists::TIntegerList* ControlPointIndices = {read=FControlPointIndices, write=SetControlPointIndices};
	__property Glvectorlists::TIntegerList* TexCoordIndices = {read=FTexCoordIndices, write=SetTexCoordIndices};
public:
	/* TGLFaceGroup.CreateOwned */ inline __fastcall virtual TFGBezierSurface(Glvectorfileobjects::TGLFaceGroups* aOwner) : Glvectorfileobjects::TGLFaceGroup(aOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGBezierSurface(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TGLFaceGroup(reader) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glparametricsurfaces */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPARAMETRICSURFACES)
using namespace Glparametricsurfaces;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlparametricsurfacesHPP
