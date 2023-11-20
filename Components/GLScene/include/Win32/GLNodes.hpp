// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLNodes.pas' rev: 35.00 (Windows)

#ifndef GlnodesHPP
#define GlnodesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
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
	Glvectortypes::TVector4f FCoords;
	System::TObject* FTagObject;
	void __fastcall SetAsVector(const Glvectortypes::TVector4f &Value);
	void __fastcall SetAsAffineVector(const Glvectortypes::TVector3f &Value);
	Glvectortypes::TVector3f __fastcall GetAsAffineVector();
	void __fastcall SetCoordinate(int AIndex, float AValue);
	float __fastcall GetCoordinate(const int Index);
	
protected:
	bool __fastcall StoreCoordinate(int AIndex);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLNode(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLNode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::PSingle __fastcall AsAddress();
	__property Glvectortypes::TVector4f AsVector = {read=FCoords, write=SetAsVector};
	__property Glvectortypes::TVector3f AsAffineVector = {read=GetAsAffineVector, write=SetAsAffineVector};
	__property float W = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=3};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	
__published:
	__property float X = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=0};
	__property float Y = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=1};
	__property float Z = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=2};
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
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Glvectortypes::TVector3f &Value)/* overload */;
	void __fastcall AddXYArc(float XRadius, float YRadius, float StartAngle, float StopAngle, int NbSegments, const Glvectortypes::TVector3f &Center);
	Glvectortypes::TVector3f __fastcall Barycenter();
	Glvectortypes::TVector3f __fastcall Normal();
	Glvectortypes::TVector3f __fastcall Vector(int I);
	void __fastcall GetExtents(Glvectortypes::TVector3f &Min, Glvectortypes::TVector3f &Max);
	void __fastcall Translate(const Glvectortypes::TVector3f &Tv);
	void __fastcall Scale(const Glvectortypes::TVector3f &Fv)/* overload */;
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
