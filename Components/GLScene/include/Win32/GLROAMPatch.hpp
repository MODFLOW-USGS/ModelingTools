// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLROAMPatch.pas' rev: 36.00 (Windows)

#ifndef GlroampatchHPP
#define GlroampatchHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <XOpenGL.hpp>
#include <GLVectorGeometry.hpp>
#include <GLHeightData.hpp>
#include <GLVectorLists.hpp>
#include <GLContext.hpp>
#include <GLVectorTypes.hpp>
#include <GLIsolines.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glroampatch
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLROAMException;
struct TROAMTriangleNode;
struct TROAMRenderPoint;
class DELPHICLASS TGLROAMPatch;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLROAMException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLROAMException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLROAMException(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLROAMException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLROAMException(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLROAMException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLROAMException() { }
	
};

#pragma pack(pop)

typedef TROAMTriangleNode *PROAMTriangleNode;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TROAMTriangleNode
{
public:
	PROAMTriangleNode Base;
	PROAMTriangleNode Left;
	PROAMTriangleNode Right;
	PROAMTriangleNode LeftChild;
	PROAMTriangleNode RightChild;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TROAMRenderPoint
{
public:
	int X;
	int Y;
	int Idx;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLROAMPatch : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<unsigned> _TGLROAMPatch__1;
	
	
private:
	int FID;
	Glheightdata::TGLHeightData* FHeightData;
	Glheightdata::PSmallIntRaster FHeightRaster;
	int FTLNode;
	int FBRNode;
	_TGLROAMPatch__1 FTLVariance;
	_TGLROAMPatch__1 FBRVariance;
	int FPatchSize;
	int FTriangleCount;
	Glcontext::TGLListHandle* FListHandle;
	int FTag;
	Glvectorgeometry::TAffineVector FObserverPosition;
	TGLROAMPatch* FNorth;
	TGLROAMPatch* FSouth;
	TGLROAMPatch* FWest;
	TGLROAMPatch* FEast;
	bool FHighRes;
	int FMaxDepth;
	Glvectorgeometry::TAffineVector FVertexScale;
	Glvectorgeometry::TAffineVector FVertexOffset;
	Glvectorgeometry::TAffineVector FTextureScale;
	Glvectorgeometry::TAffineVector FTextureOffset;
	int FMaxTLVarianceDepth;
	int FMaxBRVarianceDepth;
	Glcontext::TGLOcclusionQueryHandle* FOcclusionQuery;
	int FOcclusionSkip;
	int FOcclusionCounter;
	bool FLastOcclusionTestPassed;
	int FContourInterval;
	int FContourWidth;
	
protected:
	void __fastcall SetHeightData(Glheightdata::TGLHeightData* Val);
	void __fastcall SetOcclusionSkip(int Val);
	void __fastcall RenderROAM(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, Glvectorlists::TTexPointList* TexCoords);
	void __fastcall RenderAsStrips(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, Glvectorlists::TTexPointList* TexCoords);
	
public:
	__fastcall TGLROAMPatch();
	__fastcall virtual ~TGLROAMPatch();
	void __fastcall ComputeVariance(int Variance);
	void __fastcall ResetTessellation();
	void __fastcall ConnectToTheWest(TGLROAMPatch* WestPatch);
	void __fastcall ConnectToTheNorth(TGLROAMPatch* NorthPatch);
	bool __fastcall Tesselate();
	bool __fastcall SafeTesselate();
	void __fastcall RenderHighRes(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, Glvectorlists::TTexPointList* TexCoords, bool ForceROAM);
	void __fastcall RenderAccum(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, Glvectorlists::TTexPointList* TexCoords, int AutoFlushVertexCount);
	__classmethod void __fastcall FlushAccum(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, Glvectorlists::TTexPointList* TexCoords);
	__property Glheightdata::TGLHeightData* HeightData = {read=FHeightData, write=SetHeightData};
	__property Glvectorgeometry::TAffineVector VertexScale = {read=FVertexScale, write=FVertexScale};
	__property Glvectorgeometry::TAffineVector VertexOffset = {read=FVertexOffset, write=FVertexOffset};
	__property Glvectorgeometry::TAffineVector ObserverPosition = {read=FObserverPosition, write=FObserverPosition};
	__property Glvectorgeometry::TAffineVector TextureScale = {read=FTextureScale, write=FTextureScale};
	__property Glvectorgeometry::TAffineVector TextureOffset = {read=FTextureOffset, write=FTextureOffset};
	__property bool HighRes = {read=FHighRes, write=FHighRes, nodefault};
	__property int OcclusionSkip = {read=FOcclusionSkip, write=SetOcclusionSkip, nodefault};
	__property int OcclusionCounter = {read=FOcclusionCounter, write=FOcclusionCounter, nodefault};
	__property bool LastOcclusionTestPassed = {read=FLastOcclusionTestPassed, nodefault};
	__property int ID = {read=FID, nodefault};
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property int ContourInterval = {read=FContourInterval, write=FContourInterval, default=0};
	__property int ContourWidth = {read=FContourWidth, write=FContourWidth, default=1};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetROAMTrianglesCapacity(int nb);
extern DELPHI_PACKAGE int __fastcall GetROAMTrianglesCapacity(void);
extern DELPHI_PACKAGE void __fastcall DrawContours(Glvectorlists::TAffineVectorList* Vertices, Glvectorlists::TIntegerList* VertexIndices, int ContourInterval, int ContourWidth, int DecVal);
}	/* namespace Glroampatch */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLROAMPATCH)
using namespace Glroampatch;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlroampatchHPP
