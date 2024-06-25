// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMesh.pas' rev: 36.00 (Windows)

#ifndef GlmeshHPP
#define GlmeshHPP

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
#include <OpenGLTokens.hpp>
#include <OpenGLAdapter.hpp>
#include <GLStrings.hpp>
#include <XOpenGL.hpp>
#include <GLContext.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLState.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmesh
{
//-- forward type declarations -----------------------------------------------
struct TGLVertexData;
class DELPHICLASS TGLVertexList;
class DELPHICLASS TGLMesh;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLMeshMode : unsigned char { mmTriangleStrip, mmTriangleFan, mmTriangles, mmQuadStrip, mmQuads, mmPolygon };

enum DECLSPEC_DENUM TGLVertexMode : unsigned char { vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLVertexData
{
public:
	Glvectorgeometry::TTexPoint textCoord;
	Glvectorgeometry::TVector color;
	Glvectorgeometry::TAffineVector normal;
	Glvectorgeometry::TVertex coord;
};
#pragma pack(pop)


typedef TGLVertexData *PGLVertexData;

typedef System::StaticArray<TGLVertexData, 33554432> TGLVertexDataArray;

typedef TGLVertexDataArray *PGLVertexDataArray;

class PASCALIMPLEMENTATION TGLVertexList : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
public:
	TGLVertexData operator[](int index) { return this->Vertices[index]; }
	
private:
	PGLVertexDataArray FValues;
	int FCount;
	int FCapacity;
	int FGrowth;
	PGLVertexDataArray FLockedOldValues;
	
protected:
	void __fastcall SetCapacity(const int val);
	void __fastcall SetGrowth(const int val);
	void __fastcall Grow();
	void __fastcall SetVertices(int index, const TGLVertexData &val);
	TGLVertexData __fastcall GetVertices(int index);
	void __fastcall SetVertexCoord(int index, const Glvectorgeometry::TAffineVector &val);
	Glvectorgeometry::TAffineVector __fastcall GetVertexCoord(int index);
	void __fastcall SetVertexNormal(int index, const Glvectorgeometry::TAffineVector &val);
	Glvectorgeometry::TAffineVector __fastcall GetVertexNormal(int index);
	void __fastcall SetVertexTexCoord(int index, const Glvectorgeometry::TTexPoint &val);
	Glvectorgeometry::TTexPoint __fastcall GetVertexTexCoord(int index);
	void __fastcall SetVertexColor(int index, const Glvectortypes::TVector4f &val);
	Glvectortypes::TVector4f __fastcall GetVertexColor(int index);
	Opengltokens::PGLfloat __fastcall GetFirstEntry();
	Opengltokens::PGLfloat __fastcall GetFirstColor();
	Opengltokens::PGLfloat __fastcall GetFirstNormal();
	Opengltokens::PGLfloat __fastcall GetFirstVertex();
	Opengltokens::PGLfloat __fastcall GetFirstTexPoint();
	bool __fastcall GetLocked();
	void __fastcall SetLocked(bool val);
	
public:
	__fastcall virtual TGLVertexList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLVertexList();
	TGLVertexList* __fastcall CreateInterpolatedCoords(TGLVertexList* list2, float lerpFactor);
	void __fastcall AddVertex(const TGLVertexData &vertexData)/* overload */;
	void __fastcall AddVertex3(const TGLVertexData &vd1, const TGLVertexData &vd2, const TGLVertexData &vd3)/* overload */;
	void __fastcall AddVertex(const Glvectorgeometry::TVertex &aVertex, const Glvectorgeometry::TAffineVector &aNormal, const Glcolor::TColorVector &aColor, const Glvectorgeometry::TTexPoint &aTexPoint)/* overload */;
	void __fastcall AddVertex(const Glvectorgeometry::TVertex &vertex, const Glvectorgeometry::TAffineVector &normal, const Glcolor::TColorVector &color)/* overload */;
	void __fastcall AddVertex(const Glvectorgeometry::TVertex &vertex, const Glvectorgeometry::TAffineVector &normal)/* overload */;
	void __fastcall DuplicateVertex(int index);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear();
	__property TGLVertexData Vertices[int index] = {read=GetVertices, write=SetVertices/*, default*/};
	__property Glvectorgeometry::TAffineVector VertexCoord[int index] = {read=GetVertexCoord, write=SetVertexCoord};
	__property Glvectorgeometry::TAffineVector VertexNormal[int index] = {read=GetVertexNormal, write=SetVertexNormal};
	__property Glvectorgeometry::TTexPoint VertexTexCoord[int index] = {read=GetVertexTexCoord, write=SetVertexTexCoord};
	__property Glvectortypes::TVector4f VertexColor[int index] = {read=GetVertexColor, write=SetVertexColor};
	__property int Count = {read=FCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Growth = {read=FGrowth, write=SetGrowth, nodefault};
	Glvectorgeometry::TAffineVector __fastcall SumVertexCoords();
	void __fastcall GetExtents(Glvectorgeometry::TAffineVector &min, Glvectorgeometry::TAffineVector &max);
	void __fastcall NormalizeNormals();
	void __fastcall Translate(const Glvectorgeometry::TAffineVector &v);
	void __fastcall DefineOpenGLArrays();
	__property Opengltokens::PGLfloat FirstColor = {read=GetFirstColor};
	__property Opengltokens::PGLfloat FirstEntry = {read=GetFirstEntry};
	__property Opengltokens::PGLfloat FirstNormal = {read=GetFirstNormal};
	__property Opengltokens::PGLfloat FirstVertex = {read=GetFirstVertex};
	__property Opengltokens::PGLfloat FirstTexPoint = {read=GetFirstTexPoint};
	__property bool Locked = {read=GetLocked, write=SetLocked, nodefault};
	void __fastcall EnterLockSection();
	void __fastcall LeaveLockSection();
};


class PASCALIMPLEMENTATION TGLMesh : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TGLVertexList* FVertices;
	TGLMeshMode FMode;
	TGLVertexMode FVertexMode;
	Glvectorgeometry::TVector FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetMode(TGLMeshMode AValue);
	void __fastcall SetVertices(TGLVertexList* AValue);
	void __fastcall SetVertexMode(TGLVertexMode AValue);
	void __fastcall VerticesChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMesh();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall CalcNormals(Glstate::TFaceWinding Frontface);
	__property TGLVertexList* Vertices = {read=FVertices, write=SetVertices};
	virtual Glvectorgeometry::TVector __fastcall AxisAlignedDimensionsUnscaled();
	virtual void __fastcall StructureChanged();
	float __fastcall Length();
	float __fastcall Area();
	float __fastcall Volume();
	
__published:
	__property TGLMeshMode Mode = {read=FMode, write=SetMode, nodefault};
	__property TGLVertexMode VertexMode = {read=FVertexMode, write=SetVertexMode, default=3};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMesh(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<unsigned, 6> cMeshModeToGLEnum;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 6> cVertexModeToGLEnum;
}	/* namespace Glmesh */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESH)
using namespace Glmesh;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshHPP
