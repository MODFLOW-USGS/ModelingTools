// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileASE.pas' rev: 34.00 (Windows)

#ifndef GlfileaseHPP
#define GlfileaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilease
{
//-- forward type declarations -----------------------------------------------
struct TGLASEFaceTexure;
struct TGLASEFaceTexureChannels;
struct TGLASESmoothingGroups;
class DELPHICLASS TGLASEFace;
class DELPHICLASS TGLASEFaceList;
class DELPHICLASS TGLASEMeshObject;
struct TGLASEMaterialTextureMap;
struct TGLASEMaterialTextureMaps;
struct TGLASESubMaterial;
struct TGLASESubMaterialList;
class DELPHICLASS TGLASEMaterial;
class DELPHICLASS TGLASEMaterialList;
class DELPHICLASS TGLASEVectorFile;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLASEFaceTexure
{
public:
	int Idx0;
	int Idx1;
	int Idx2;
};


struct DECLSPEC_DRECORD TGLASEFaceTexureChannels
{
public:
	int Count;
	System::StaticArray<TGLASEFaceTexure, 12> ChanelTexture;
};


struct DECLSPEC_DRECORD TGLASESmoothingGroups
{
public:
	int Count;
	System::StaticArray<int, 6> Groups;
};


class PASCALIMPLEMENTATION TGLASEFace : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::StaticArray<int, 3> FV;
	Glvectortypes::TVector3f FNormal;
	System::StaticArray<Glvectortypes::TVector3f, 3> FN;
	TGLASESmoothingGroups FSmoothing;
	int FSubMaterialID;
	TGLASEFaceTexureChannels FTextChannels;
	
public:
	__fastcall TGLASEFace();
	// [SKIPPED] __property int VertIdx1 = {read=FV[0], nodefault};
	// [SKIPPED] __property int VertIdx2 = {read=FV[1], nodefault};
	// [SKIPPED] __property int VertIdx3 = {read=FV[2], nodefault};
	__property Glvectortypes::TVector3f Normal = {read=FNormal};
	// [SKIPPED] __property Glvectortypes::TVector3f Normal1 = {read=FN[0]};
	// [SKIPPED] __property Glvectortypes::TVector3f Normal2 = {read=FN[1]};
	// [SKIPPED] __property Glvectortypes::TVector3f Normal3 = {read=FN[2]};
	__property TGLASEFaceTexureChannels TextChannels = {read=FTextChannels};
	__property TGLASESmoothingGroups Smoothing = {read=FSmoothing};
	__property int SubMaterialID = {read=FSubMaterialID, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLASEFace() { }
	
};


class PASCALIMPLEMENTATION TGLASEFaceList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TGLASEFace* operator[](int Index) { return this->Face[Index]; }
	
private:
	System::Classes::TList* FItems;
	TGLASEFace* __fastcall GetFace(int Index);
	int __fastcall GetCount();
	
public:
	__fastcall TGLASEFaceList();
	__fastcall virtual ~TGLASEFaceList();
	TGLASEFace* __fastcall Add();
	void __fastcall Delete(int aIndex);
	void __fastcall Clear();
	__property TGLASEFace* Face[int Index] = {read=GetFace/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};


class PASCALIMPLEMENTATION TGLASEMeshObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLASEFaceList* FFaces;
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectortypes::TMatrix4f FMatrix;
	Glvectortypes::TVector3f FInheritedPosition;
	Glvectortypes::TVector3f FInheritedScale;
	Glvectortypes::TVector3f FInheritedRotation;
	float FRotationAngle;
	Glvectortypes::TVector3f FRotationAxis;
	Glvectortypes::TVector4f FPosition;
	Glvectortypes::TVector3f FScale;
	float FScaleAxisAngle;
	Glvectortypes::TVector3f FScaleAxis;
	System::StaticArray<Glvectorlists::TAffineVectorList*, 12> FTexChannels;
	int FTexChannelsCount;
	bool FHasNormals;
	int FMaterialID;
	Glvectorlists::TAffineVectorList* __fastcall AddTexChannel();
	Glvectorlists::TAffineVectorList* __fastcall GetTextChannel(int Channel);
	
public:
	__fastcall TGLASEMeshObject();
	__fastcall virtual ~TGLASEMeshObject();
	__property TGLASEFaceList* Faces = {read=FFaces};
	__property Glvectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Glvectorlists::TAffineVectorList* TextChannel[int Channel] = {read=GetTextChannel};
	__property int TextChannelsCount = {read=FTexChannelsCount, nodefault};
	__property Glvectortypes::TMatrix4f Matrix = {read=FMatrix};
	__property Glvectortypes::TVector3f InheritedPosition = {read=FInheritedPosition};
	__property Glvectortypes::TVector3f InheritedRotation = {read=FInheritedRotation};
	__property Glvectortypes::TVector3f InheritedScale = {read=FInheritedScale};
	__property Glvectortypes::TVector4f Position = {read=FPosition};
	__property Glvectortypes::TVector3f RotationAxis = {read=FRotationAxis};
	__property float RotationAngle = {read=FRotationAngle};
	__property Glvectortypes::TVector3f Scale = {read=FScale};
	__property Glvectortypes::TVector3f ScaleAxis = {read=FScaleAxis};
	__property float ScaleAxisAngle = {read=FScaleAxisAngle};
	__property bool HasNormals = {read=FHasNormals, nodefault};
	__property int MaterialID = {read=FMaterialID, nodefault};
};


struct DECLSPEC_DRECORD TGLASEMaterialTextureMap
{
public:
	System::UnicodeString Kind;
	System::UnicodeString Name;
	System::UnicodeString _Class;
	int No;
	float Amount;
	System::UnicodeString Bitmap;
	float UOffset;
	float VOffset;
	float UTiling;
	float VTiling;
	float Angle;
	float Blur;
	float BlurOffset;
	float NouseAmount;
	float NoiseSize;
	int NoiseLevel;
	float NoisePhase;
};


struct DECLSPEC_DRECORD TGLASEMaterialTextureMaps
{
	
private:
	typedef System::StaticArray<TGLASEMaterialTextureMap, 12> _TGLASEMaterialTextureMaps__1;
	
	
public:
	_TGLASEMaterialTextureMaps__1 Map;
	int Count;
};


struct DECLSPEC_DRECORD TGLASESubMaterial
{
public:
	System::UnicodeString Name;
	Glvectortypes::TVector3f Ambient;
	Glvectortypes::TVector3f Diffuse;
	Glvectortypes::TVector3f Specular;
	float Shiness;
	float ShineStrength;
	float Transparency;
	float WireSize;
	float SelfIllumination;
	TGLASEMaterialTextureMaps TextureMaps;
};


struct DECLSPEC_DRECORD TGLASESubMaterialList
{
	
private:
	typedef System::StaticArray<TGLASESubMaterial, 5> _TGLASESubMaterialList__1;
	
	
public:
	_TGLASESubMaterialList__1 SubMaterial;
	int Count;
};


class PASCALIMPLEMENTATION TGLASEMaterial : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float FWireSize;
	float FShineStrength;
	float FShiness;
	float FTransparency;
	System::UnicodeString FName;
	Glvectortypes::TVector3f FDiffuse;
	Glvectortypes::TVector3f FAmbient;
	Glvectortypes::TVector3f FSpecular;
	TGLASESubMaterialList FSubMaterials;
	TGLASEMaterialTextureMaps FTextureMaps;
	
public:
	__fastcall TGLASEMaterial();
	__property System::UnicodeString Name = {read=FName};
	__property Glvectortypes::TVector3f Ambient = {read=FAmbient};
	__property Glvectortypes::TVector3f Diffuse = {read=FDiffuse};
	__property Glvectortypes::TVector3f Specular = {read=FSpecular};
	__property float Shiness = {read=FShiness};
	__property float ShineStrength = {read=FShineStrength};
	__property float Transparency = {read=FTransparency};
	__property float WireSize = {read=FWireSize};
	__property TGLASEMaterialTextureMaps TextureMaps = {read=FTextureMaps};
	__property TGLASESubMaterialList SubMaterials = {read=FSubMaterials};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLASEMaterial() { }
	
};


class PASCALIMPLEMENTATION TGLASEMaterialList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TGLASEMaterial* operator[](int Index) { return this->Material[Index]; }
	
private:
	System::Classes::TList* FItems;
	int __fastcall GetCount();
	TGLASEMaterial* __fastcall GetMaterial(int Index);
	
public:
	__fastcall TGLASEMaterialList();
	__fastcall virtual ~TGLASEMaterialList();
	TGLASEMaterial* __fastcall Add();
	void __fastcall Delete(int aIndex);
	void __fastcall Clear();
	__property TGLASEMaterial* Material[int Index] = {read=GetMaterial/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};


class PASCALIMPLEMENTATION TGLASEVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	System::Classes::TStringList* FStringData;
	System::UnicodeString FHeader;
	System::UnicodeString FComment;
	bool FRECVShadow;
	bool FCastShadow;
	bool FMotionBlur;
	TGLASEMaterialList* FMaterialList;
	bool __fastcall ContainString(const System::UnicodeString aData, const System::UnicodeString aString);
	int __fastcall GetTagOnData(const System::UnicodeString aData);
	bool __fastcall IsEndOfSection(const System::UnicodeString aData);
	Glvectortypes::TVector3f __fastcall GetValue3D(const System::UnicodeString aData);
	Glvectortypes::TVector3f __fastcall GetValue4D(const System::UnicodeString aData, int &Value0);
	System::UnicodeString __fastcall GetStringValue(const System::UnicodeString aData);
	double __fastcall GetDoubleValue(const System::UnicodeString aData);
	System::UnicodeString __fastcall GetFirstValue(System::UnicodeString aData);
	int __fastcall GetEndOfFirstValue(const System::UnicodeString aData);
	void __fastcall SkipSection(int &aLineIndex);
	bool __fastcall IsSectionBegingin(const System::UnicodeString aData);
	bool __fastcall CheckUnknownData(int &aLineIndex);
	void __fastcall ParseFaceString(const System::UnicodeString aData, int &Index, int &A, int &B, int &C, int &AB, int &BC, int &CA, int &MatID, TGLASESmoothingGroups &Smooth);
	void __fastcall ParseScene(int &aLineIndex);
	void __fastcall ParseGeomObject(int &aLineIndex);
	void __fastcall ParseMeshOptions(int &aLineIndex, TGLASEMeshObject* aMesh);
	void __fastcall ParseMeshGeom(int &aLineIndex, TGLASEMeshObject* aMesh);
	void __fastcall ParseMappingChannel(int &aLineIndex, TGLASEMeshObject* aMesh);
	void __fastcall ParseMeshVertices(int &aLineIndex, TGLASEMeshObject* aMesh, int VerticesCount);
	void __fastcall ParseMeshFaces(int &aLineIndex, TGLASEMeshObject* aMesh, int FacesCount);
	void __fastcall ParseMeshNormals(int &aLineIndex, TGLASEMeshObject* aMesh, int FacesCount);
	void __fastcall ParseMeshTextureVertices(int &aLineIndex, TGLASEMeshObject* aMesh, int TextureVerticesCount);
	void __fastcall ParseMeshTextureFaces(int &aLineIndex, TGLASEMeshObject* aMesh, int TextureFacesCount);
	void __fastcall ParseMaterialList(int &aLineIndex);
	void __fastcall ParseMaterial(int &aLineIndex, TGLASEMaterial* aMaterial);
	void __fastcall ParseSubMaterial(int &aLineIndex, TGLASEMaterial* aMaterial);
	bool __fastcall CheckTextureMap(int &aLineIndex, TGLASEMaterialTextureMaps &aMaps);
	void __fastcall ParseTextureMap(int &aLineIndex, TGLASEMaterialTextureMaps &aMaps, const System::UnicodeString aMapKind);
	bool __fastcall GetPropMBlur(const System::UnicodeString aData);
	bool __fastcall GetPropCastShadow(const System::UnicodeString aData);
	bool __fastcall GetPropRECVShadow(const System::UnicodeString aData);
	void __fastcall Parse();
	
public:
	__fastcall virtual TGLASEVectorFile(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLASEVectorFile();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	__property System::UnicodeString Header = {read=FHeader};
	__property System::UnicodeString Comment = {read=FComment};
	__property bool MotionBlur = {read=FMotionBlur, nodefault};
	__property bool CastShadow = {read=FCastShadow, nodefault};
	__property bool RECVShadow = {read=FRECVShadow, nodefault};
};


enum DECLSPEC_DENUM TASETextureMap : unsigned char { tmGeneric, tmAmbient, tmDiffuse, tmSpecular, tmShine, tmShinestrength, tmSelfillum, tmOpacity, tmFiltercolor, tmBump, tmReflect, tmRefract };

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 GL_ASE_MAX_TEXURE_CHANNELS = System::Int8(0xc);
static const System::Int8 GL_ASE_MAX_SUBMATERIALS = System::Int8(0x5);
static const System::Int8 GL_ASE_MAX_SMOOTH_GROUPS = System::Int8(0x5);
static const System::Int8 GL_ASE_MAX_TEXTURE_MAPS = System::Int8(0xc);
extern DELPHI_PACKAGE void __fastcall ASESetPreferredTexture(TASETextureMap aMap, int aSubMaterialIndex = 0xffffffff);
extern DELPHI_PACKAGE void __fastcall ASESetPreferredLightmap(TASETextureMap aMap, int aSubMaterialIndex = 0xffffffff);
}	/* namespace Glfilease */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEASE)
using namespace Glfilease;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileaseHPP
