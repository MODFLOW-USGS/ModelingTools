// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLIsosurface.pas' rev: 36.00 (Windows)

#ifndef GLIsosurfaceHPP
#define GLIsosurfaceHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
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

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMarchingCube : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gltypes::TxScalarValue FIsoValue;
	Glvectorgeometry::PIntegerArray PVertsX;
	Glvectorgeometry::PIntegerArray PVertsY;
	Glvectorgeometry::PIntegerArray PVertsZ;
	int _Nverts;
	int _Ntrigs;
	int _Sverts;
	int _Strigs;
	Gltypes::PxVertexArray PVertices;
	Gltypes::PxTriangleArray PTriangles;
	System::LongWord _i;
	System::LongWord _j;
	System::LongWord _k;
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
	Gltypes::PxVoxelData VoxelData;
	void __fastcall Process_cube();
	void __fastcall Compute_Intersection_Points();
	void __fastcall Add_Triangle(int *trig, const System::NativeInt trig_High, System::Byte N, int v12 = 0xffffffff);
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
	Gltypes::TxScalarValue __fastcall GetVoxelValue(int i, int j, int k);
	void __fastcall SetVoxelValue(int i, int j, int k, Gltypes::TxScalarValue HfValue);
	Gltypes::TxVoxel __fastcall GetVoxelData(int i, int j, int k);
	Gltypes::PxVoxel __fastcall Voxel(int i, int j, int k);
	virtual float __fastcall calc_u(float v1, float v2);
	
public:
	Gltypes::TxScalarField ScalarField;
	__fastcall virtual TGLMarchingCube()/* overload */;
	__fastcall virtual TGLMarchingCube(int SizeX, int SizeY, int SizeZ, Gltypes::TxScalarValue AIsoValue, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax)/* overload */;
	virtual void __fastcall ReDim(int ASizeX, int ASizeY, int ASizeZ, float xMin, float xMax, float yMin, float yMax, float zMin, float zMax);
	__fastcall virtual ~TGLMarchingCube();
	void __fastcall Run()/* overload */;
	void __fastcall Run(Gltypes::TxScalarValue IsoValue)/* overload */;
	virtual bool __fastcall Internal(Gltypes::TxScalarValue AValue);
	virtual void __fastcall FillVoxelData()/* overload */;
	virtual void __fastcall FillVoxelData(Gltypes::TxScalarValue AIsoValue, Gltypes::TxScalarField AScalarField = 0x0)/* overload */;
	virtual void __fastcall FillVoxelData(Gltypes::TxScalarValue AIsoValue, Gltypes::TxScalarFieldInt AScalarField)/* overload */;
	void __fastcall CalcVertices(Glmesh::TGLVertexList* Vertices, float Alpha = 1.000000E+00f);
	void __fastcall CalcMeshObject(Glvectorfileobjects::TMeshObject* AMeshObject, float Alpha = 1.000000E+00f);
	__property Gltypes::TxScalarValue IsoValue = {read=FIsoValue, write=FIsoValue};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TIsoSurfaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSingle3DArray Data;
	System::StaticArray<int, 3> Dimensions;
	System::Word __fastcall BuildIndex(float *ADatavals, const System::NativeInt ADatavals_High, float Isovalue);
	Glvectorgeometry::TVertex __fastcall Interpolate(const Glvectorgeometry::TAffineVector &V0, const Glvectorgeometry::TAffineVector &V1, float &Val0, float &Val1, float &Isovalue, bool isPolished);
	
public:
	__fastcall TIsoSurfaceExtractor()/* overload */;
	__fastcall TIsoSurfaceExtractor(int Xdim, int Ydim, int Zdim, TSingle3DArray &AData)/* overload */;
	__fastcall virtual ~TIsoSurfaceExtractor();
	void __fastcall AssignData(int Xdim, int Ydim, int Zdim, TSingle3DArray &AData);
	void __fastcall MarchingCubes(float Isovalue, /* out */ TVertexArray &Vertices, /* out */ TIntegerArray &Triangles, bool isPolished);
	void __fastcall MarchingTetrahedra(float Isovalue, /* out */ TVertexArray &Vertices, /* out */ TIntegerArray &Triangles, bool isPolished);
};

#pragma pack(pop)

struct DECLSPEC_DRECORD Glisosurface__5
{
public:
	Gltypes::TxScalarField ScalarField;
	Gltypes::TxScalarValue IsoValue;
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST int ALLOC_SIZE = int(0x10000);
extern DELPHI_PACKAGE System::StaticArray<Glisosurface__5, 7> DemoScalarField;
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFSphere(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFToroidal(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFDoubleTorus(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFChmutov1(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFChmutov2(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFKleinBottle(float X, float Y, float Z);
extern DELPHI_PACKAGE Gltypes::TxScalarValue __fastcall SFMinkowski(float X, float Y, float Z);
}	/* namespace Glisosurface */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLISOSURFACE)
using namespace Glisosurface;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLIsosurfaceHPP
