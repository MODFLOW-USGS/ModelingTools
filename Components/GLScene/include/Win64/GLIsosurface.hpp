// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLIsosurface.pas' rev: 35.00 (Windows)

#ifndef GlisosurfaceHPP
#define GlisosurfaceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLMesh.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorTypes.hpp>
#include <GLTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glisosurface
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMarchingCube;
class DELPHICLASS TIsoSurfaceExtractor;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<float> Glisosurface__1;

typedef System::DynamicArray<System::DynamicArray<float> > Glisosurface__2;

typedef System::DynamicArray<System::DynamicArray<System::DynamicArray<float> > > TSingle3DArray;

typedef System::DynamicArray<Glvectortypes::TVector3f> TVertexArray;

typedef System::DynamicArray<int> TIntegerArray;

class PASCALIMPLEMENTATION TGLMarchingCube : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float FIsoValue;
	Glvectorgeometry::TIntegerVector *PVertsX;
	Glvectorgeometry::TIntegerVector *PVertsY;
	Glvectorgeometry::TIntegerVector *PVertsZ;
	int _Nverts;
	int _Ntrigs;
	int _Sverts;
	int _Strigs;
	Gltypes::TxVertexArray *PVertices;
	Gltypes::TxTriangleArray *PTriangles;
	unsigned _i;
	unsigned _j;
	unsigned _k;
	System::StaticArray<Gltypes::TxVoxel, 8> _Cube;
	System::Byte _lut_entry;
	void __fastcall Init_temps();
	void __fastcall Init_all();
	void __fastcall Init_space();
	void __fastcall Clean_temps();
	void __fastcall Clean_all(bool keepFacets = false);
	void __fastcall Clean_space();
	void __fastcall Test_vertex_addiction();
	
protected:
	bool FOriginalMC;
	int FSizeX;
	int FSizeY;
	int FSizeZ;
	float FxMin;
	float FxMax;
	float FyMin;
	float FyMax;
	float FzMin;
	float FzMax;
	float FStepX;
	float FStepY;
	float FStepZ;
	Gltypes::TxVoxelData *VoxelData;
	void __fastcall Process_cube();
	void __fastcall Compute_Intersection_Points();
	void __fastcall Add_Triangle(int *trig, const int trig_High, System::Byte N, int v12 = 0xffffffff);
	int __fastcall Add_x_vertex();
	int __fastcall Add_y_vertex();
	int __fastcall Add_z_vertex();
	int __fastcall Add_c_vertex();
	float __fastcall Get_x_grad(int i, int j, int k);
	float __fastcall Get_y_grad(int i, int j, int k);
	float __fastcall Get_z_grad(int i, int j, int k);
	int __fastcall Get_x_vert(int i, int j, int k);
	int __fastcall Get_y_vert(int i, int j, int k);
	int __fastcall Get_z_vert(int i, int j, int k);
	void __fastcall Set_x_vert(int a_val, int i, int j, int k);
	void __fastcall Set_y_vert(int a_val, int i, int j, int k);
	void __fastcall Set_z_vert(int a_val, int i, int j, int k);
	float __fastcall GetVoxelValue(int i, int j, int k);
	void __fastcall SetVoxelValue(int i, int j, int k, float HfValue);
	Gltypes::TxVoxel __fastcall GetVoxelData(int i, int j, int k);
	Gltypes::PxVoxel __fastcall Voxel(int i, int j, int k);
	virtual float __fastcall calc_u(float v1, float v2);
	
public:
	Gltypes::TxScalarField ScalarField;
	__fastcall virtual TGLMarchingCube()/* overload */;
	__fastcall virtual TGLMarchingCube(int SizeX, int SizeY, int SizeZ, float AIsoValue, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax)/* overload */;
	virtual void __fastcall ReDim(int ASizeX, int ASizeY, int ASizeZ, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax);
	__fastcall virtual ~TGLMarchingCube();
	void __fastcall Run()/* overload */;
	void __fastcall Run(float IsoValue)/* overload */;
	virtual bool __fastcall Internal(float AValue);
	virtual void __fastcall FillVoxelData()/* overload */;
	virtual void __fastcall FillVoxelData(float AIsoValue, Gltypes::TxScalarField AScalarField = 0x0)/* overload */;
	virtual void __fastcall FillVoxelData(float AIsoValue, Gltypes::TxScalarFieldInt AScalarField)/* overload */;
	void __fastcall CalcVertices(Glmesh::TGLVertexList* Vertices, float Alpha = 1.000000E+00f);
	void __fastcall CalcMeshObject(Glvectorfileobjects::TMeshObject* AMeshObject, float Alpha = 1.000000E+00f);
	__property float IsoValue = {read=FIsoValue, write=FIsoValue};
};


class PASCALIMPLEMENTATION TIsoSurfaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSingle3DArray Data;
	System::StaticArray<int, 3> Dimensions;
	System::Word __fastcall BuildIndex(float *ADatavals, const int ADatavals_High, float Isovalue);
	Glvectortypes::TVector3f __fastcall Interpolate(const Glvectortypes::TVector3f &V0, const Glvectortypes::TVector3f &V1, float &Val0, float &Val1, float &Isovalue, bool isPolished);
	
public:
	__fastcall TIsoSurfaceExtractor()/* overload */;
	__fastcall TIsoSurfaceExtractor(int Xdim, int Ydim, int Zdim, TSingle3DArray &AData)/* overload */;
	__fastcall virtual ~TIsoSurfaceExtractor();
	void __fastcall AssignData(int Xdim, int Ydim, int Zdim, TSingle3DArray &AData);
	void __fastcall MarchingCubes(float Isovalue, /* out */ TVertexArray &Vertices, /* out */ TIntegerArray &Triangles, bool isPolished);
	void __fastcall MarchingTetrahedra(float Isovalue, /* out */ TVertexArray &Vertices, /* out */ TIntegerArray &Triangles, bool isPolished);
};


struct DECLSPEC_DRECORD Glisosurface__5
{
public:
	Gltypes::TxScalarField ScalarField;
	float IsoValue;
};


//-- var, const, procedure ---------------------------------------------------
static const int ALLOC_SIZE = int(0x10000);
extern DELPHI_PACKAGE System::StaticArray<Glisosurface__5, 7> DemoScalarField;
extern DELPHI_PACKAGE float __fastcall SFSphere(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFToroidal(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFDoubleTorus(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFChmutov1(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFChmutov2(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFKleinBottle(float X, float Y, float Z);
extern DELPHI_PACKAGE float __fastcall SFMinkowski(float X, float Y, float Z);
}	/* namespace Glisosurface */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLISOSURFACE)
using namespace Glisosurface;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlisosurfaceHPP
