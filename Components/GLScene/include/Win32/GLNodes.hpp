// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLNodes.pas' rev: 36.00 (Windows)

#ifndef GlnodesHPP
#define GlnodesHPP

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
#include <OpenGLTokens.hpp>
#include <OpenGLAdapter.hpp>
#include <GLVectorGeometry.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLContext.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLSpline.hpp>
#include <XOpenGL.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glnodes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLNode;
class DELPHICLASS TGLNodes;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNode : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glvectorgeometry::TVector FCoords;
	System::TObject* FTagObject;
	void __fastcall SetAsVector(const Glvectorgeometry::TVector &Value);
	void __fastcall SetAsAffineVector(const Glvectorgeometry::TAffineVector &Value);
	Glvectorgeometry::TAffineVector __fastcall GetAsAffineVector();
	void __fastcall SetCoordinate(int AIndex, Opengltokens::TGLfloat AValue);
	Opengltokens::TGLfloat __fastcall GetCoordinate(const int Index);
	
protected:
	bool __fastcall StoreCoordinate(int AIndex);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLNode(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	Opengltokens::PGLfloat __fastcall AsAddress();
	__property Glvectorgeometry::TVector AsVector = {read=FCoords, write=SetAsVector};
	__property Glvectorgeometry::TAffineVector AsAffineVector = {read=GetAsAffineVector, write=SetAsAffineVector};
	__property Opengltokens::TGLfloat W = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=3};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	
__published:
	__property Opengltokens::TGLfloat X = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=0};
	__property Opengltokens::TGLfloat Y = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=1};
	__property Opengltokens::TGLfloat Z = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=2};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNodes : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLNode* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int Index, TGLNode* const Val);
	TGLNode* __fastcall GetItems(int Index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLNodes(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass);
	TGLNodes* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLNode* __fastcall Add();
	HIDESBASE TGLNode* __fastcall FindItemID(int ID);
	__property TGLNode* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLNode* __fastcall First();
	TGLNode* __fastcall Last();
	virtual void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
	void __fastcall AddNode(Glcoordinates::TGLCustomCoordinates* const Coords)/* overload */;
	void __fastcall AddNode(const Opengltokens::TGLfloat X, const Opengltokens::TGLfloat Y, const Opengltokens::TGLfloat Z)/* overload */;
	void __fastcall AddNode(const Glvectorgeometry::TVector &Value)/* overload */;
	void __fastcall AddNode(const Glvectorgeometry::TAffineVector &Value)/* overload */;
	void __fastcall AddXYArc(float XRadius, float YRadius, float StartAngle, float StopAngle, int NbSegments, const Glvectorgeometry::TAffineVector &Center);
	Glvectorgeometry::TAffineVector __fastcall Barycenter();
	Glvectorgeometry::TAffineVector __fastcall Normal();
	Glvectorgeometry::TAffineVector __fastcall Vector(int I);
	void __fastcall GetExtents(Glvectorgeometry::TAffineVector &Min, Glvectorgeometry::TAffineVector &Max);
	void __fastcall Translate(const Glvectorgeometry::TAffineVector &Tv);
	void __fastcall Scale(const Glvectorgeometry::TAffineVector &Fv)/* overload */;
	void __fastcall Scale(float F)/* overload */;
	void __fastcall RotateAroundX(float Angle);
	void __fastcall RotateAroundY(float Angle);
	void __fastcall RotateAroundZ(float Angle);
	void __fastcall RenderTesselatedPolygon(bool ATextured, Glvectorgeometry::PAffineVector ANormal = (Glvectorgeometry::PAffineVector)(0x0), int ASplineDivisions = 0x1, bool AInvertNormals = false);
	Glspline::TCubicSpline* __fastcall CreateNewCubicSpline();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLNodes() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLNodesClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glnodes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLNODES)
using namespace Glnodes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlnodesHPP
