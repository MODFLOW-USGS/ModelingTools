// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSilhouette.pas' rev: 35.00 (Windows)

#ifndef GlsilhouetteHPP
#define GlsilhouetteHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsilhouette
{
//-- forward type declarations -----------------------------------------------
struct TGLSilhouetteParameters;
class DELPHICLASS TGLSilhouette;
class DELPHICLASS TBaseConnectivity;
class DELPHICLASS TConnectivity;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSilhouetteStyle : unsigned char { ssOmni, ssParallel };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLSilhouetteParameters
{
public:
	Glvectortypes::TVector3f SeenFrom;
	Glvectortypes::TVector3f LightDirection;
	TGLSilhouetteStyle Style;
	bool CappingRequired;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSilhouette : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glvectorlists::TVectorList* FVertices;
	Glvectorlists::TIntegerList* FIndices;
	Glvectorlists::TIntegerList* FCapIndices;
	TGLSilhouetteParameters FParameters;
	
protected:
	void __fastcall SetIndices(Glvectorlists::TIntegerList* const value);
	void __fastcall SetCapIndices(Glvectorlists::TIntegerList* const value);
	void __fastcall SetVertices(Glvectorlists::TVectorList* const value);
	
public:
	__fastcall virtual TGLSilhouette();
	__fastcall virtual ~TGLSilhouette();
	__property TGLSilhouetteParameters Parameters = {read=FParameters, write=FParameters};
	__property Glvectorlists::TVectorList* Vertices = {read=FVertices, write=SetVertices};
	__property Glvectorlists::TIntegerList* Indices = {read=FIndices, write=SetIndices};
	__property Glvectorlists::TIntegerList* CapIndices = {read=FCapIndices, write=SetCapIndices};
	virtual void __fastcall Flush();
	void __fastcall Clear();
	void __fastcall ExtrudeVerticesToInfinity(const Glvectortypes::TVector3f &origin);
	void __fastcall AddEdgeToSilhouette(const Glvectortypes::TVector3f &v0, const Glvectortypes::TVector3f &v1, bool tightButSlow);
	void __fastcall AddIndexedEdgeToSilhouette(const int Vi0, const int Vi1);
	void __fastcall AddCapToSilhouette(const Glvectortypes::TVector3f &v0, const Glvectortypes::TVector3f &v1, const Glvectortypes::TVector3f &v2, bool tightButSlow);
	void __fastcall AddIndexedCapToSilhouette(const int Vi0, const int Vi1, const int vi2);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseConnectivity : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FPrecomputeFaceNormal;
	virtual int __fastcall GetEdgeCount();
	virtual int __fastcall GetFaceCount();
	
public:
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__property bool PrecomputeFaceNormal = {read=FPrecomputeFaceNormal, nodefault};
	virtual void __fastcall CreateSilhouette(const TGLSilhouetteParameters &ASilhouetteParameters, TGLSilhouette* &ASilhouette, bool AddToSilhouette);
	__fastcall virtual TBaseConnectivity(bool APrecomputeFaceNormal);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBaseConnectivity() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConnectivity : public TBaseConnectivity
{
	typedef TBaseConnectivity inherited;
	
protected:
	Glvectorlists::TIntegerList* FEdgeVertices;
	Glvectorlists::TIntegerList* FEdgeFaces;
	Glvectorlists::TByteList* FFaceVisible;
	Glvectorlists::TIntegerList* FFaceVertexIndex;
	Glvectorlists::TAffineVectorList* FFaceNormal;
	Glvectorlists::TIntegerList* FVertexMemory;
	Glvectorlists::TAffineVectorList* FVertices;
	HIDESBASE int __fastcall GetEdgeCount();
	HIDESBASE int __fastcall GetFaceCount();
	int __fastcall ReuseOrFindVertexID(const Glvectortypes::TVector3f &SeenFrom, TGLSilhouette* ASilhouette, int index);
	
public:
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall CreateSilhouette(const TGLSilhouetteParameters &silhouetteParameters, TGLSilhouette* &ASilhouette, bool AddToSilhouette);
	int __fastcall AddIndexedEdge(int vertexIndex0, int vertexIndex1, int FaceID);
	int __fastcall AddIndexedFace(int Vi0, int Vi1, int vi2);
	int __fastcall AddFace(const Glvectortypes::TVector3f &vertex0, const Glvectortypes::TVector3f &vertex1, const Glvectortypes::TVector3f &vertex2);
	int __fastcall AddQuad(const Glvectortypes::TVector3f &vertex0, const Glvectortypes::TVector3f &vertex1, const Glvectortypes::TVector3f &vertex2, const Glvectortypes::TVector3f &vertex3);
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__fastcall virtual TConnectivity(bool APrecomputeFaceNormal);
	__fastcall virtual ~TConnectivity();
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsilhouette */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSILHOUETTE)
using namespace Glsilhouette;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsilhouetteHPP
