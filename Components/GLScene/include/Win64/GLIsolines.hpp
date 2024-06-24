// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glisolines.pas' rev: 36.00 (Windows)

#ifndef GlisolinesHPP
#define GlisolinesHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Sysutils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.Generics.Collections.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Globjects.hpp>
#include <Glmultipolygon.hpp>
#include <Glcoordinates.hpp>
#include <Gltypes.hpp>
#include <Glcolor.hpp>
#include <Glspline.hpp>
#include <Glspacetext.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorfileobjects.hpp>
#include <Glscene.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glisolines
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLIsoline;
class DELPHICLASS TGLIsolines;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<float> TVectorArr;

typedef System::DynamicArray<System::Byte> TByteVectorArr;

typedef System::DynamicArray<float> Glisolines__1;

typedef System::DynamicArray<System::DynamicArray<float> > TMatrixArr;

typedef System::DynamicArray<System::Byte> Glisolines__2;

typedef System::DynamicArray<System::DynamicArray<System::Byte> > TByteMatrixArr;

typedef System::StaticArray<float, 5> TVectorL4D;

typedef System::StaticArray<int, 5> TVectorL4I;

typedef System::StaticArray<System::StaticArray<System::StaticArray<int, 3>, 3>, 3> TCastArray;

typedef System::StaticArray<Gltypes::TxPoint2D, 32768> TVertex2DArr;

typedef TVertex2DArr *PVertex2DArr;

typedef TGLIsoline* *PGLIsoline;

class PASCALIMPLEMENTATION TGLIsoline : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int NP;
	PVertex2DArr Line;
	__fastcall virtual TGLIsoline(int LineSize);
	__fastcall virtual ~TGLIsoline();
};


enum DECLSPEC_DENUM TGLIsolineState : unsigned char { ilsEmpty, ilsCalculating, ilsReady };

class PASCALIMPLEMENTATION TGLIsolines : public Globjects::TGLLines
{
	typedef Globjects::TGLLines inherited;
	
	
private:
	typedef System::DynamicArray<Glspacetext::TGLSpaceText*> _TGLIsolines__1;
	
	
public:
	Glvectorgeometry::TAffineVector IsoVertex;
	_TGLIsolines__1 GLSpaceTextSF;
	void __fastcall MakeIsolines(TMatrixArr &Depths, int bmSize, float StartDepth, float EndDepth, int Interval);
	void __fastcall FreeList();
	__fastcall virtual TGLIsolines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLIsolines();
	void __fastcall Conrec(int PlaneSFindex, Glvectorfileobjects::TGLFreeForm* PlaneSF, TMatrixArr Data, int ilb, int iub, int jlb, int jub, TVectorArr X, TVectorArr Y, int NC, TVectorArr HgtL, float Z_Kfix, float res3Dmax, float res3Dmin);
	
private:
	int CoordRange;
	System::Classes::TList* LineList;
	TGLIsolineState IsolineState;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLIsolines(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLLines(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Initialize_Contouring(TMatrixArr &DataGrid, int NXpoints, int NYpoints, float CurrentIsoline);
extern DELPHI_PACKAGE void __fastcall Release_Memory_Isoline(void);
extern DELPHI_PACKAGE bool __fastcall GetNextIsoline(TGLIsoline* &Isoline);
extern DELPHI_PACKAGE void __fastcall TriangleElevationSegments(const Glvectorgeometry::TAffineVector &p1, const Glvectorgeometry::TAffineVector &p2, const Glvectorgeometry::TAffineVector &p3, float ElevationDelta, Glvectorlists::TAffineVectorList* Segments);
}	/* namespace Glisolines */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLISOLINES)
using namespace Glisolines;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlisolinesHPP
