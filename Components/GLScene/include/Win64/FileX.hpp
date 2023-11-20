// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileX.pas' rev: 35.00 (Windows)

#ifndef FilexHPP
#define FilexHPP

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
#include <GLPersistentClasses.hpp>
#include <GLUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Filex
{
//-- forward type declarations -----------------------------------------------
struct TDXFileHeader;
class DELPHICLASS TDXNode;
class DELPHICLASS TDXMaterial;
class DELPHICLASS TDXMaterialList;
class DELPHICLASS TDXFrame;
class DELPHICLASS TDXMesh;
class DELPHICLASS TDXFile;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TDXFileHeader
{
public:
	System::StaticArray<char, 4> Magic;
	System::StaticArray<char, 2> Major;
	System::StaticArray<char, 2> Minor;
	System::StaticArray<char, 4> FileType;
	System::StaticArray<char, 4> FloatType;
};


class PASCALIMPLEMENTATION TDXNode : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FTypeName;
	TDXNode* FOwner;
	TDXNode* __fastcall GetItem(int index);
	
public:
	__fastcall TDXNode(TDXNode* AOwner);
	__fastcall virtual TDXNode();
	virtual void __fastcall Clear();
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString TypeName = {read=FTypeName, write=FTypeName};
	__property TDXNode* Owner = {read=FOwner};
	__property TDXNode* Items[int index] = {read=GetItem};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXNode() { }
	
};


class PASCALIMPLEMENTATION TDXMaterial : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	Glvectortypes::TVector4f FDiffuse;
	float FSpecPower;
	Glvectortypes::TVector3f FSpecular;
	Glvectortypes::TVector3f FEmissive;
	System::UnicodeString FTexture;
	
public:
	__fastcall TDXMaterial(TDXMaterialList* AOwner);
	__property Glvectortypes::TVector4f Diffuse = {read=FDiffuse, write=FDiffuse};
	__property float SpecPower = {read=FSpecPower, write=FSpecPower};
	__property Glvectortypes::TVector3f Specular = {read=FSpecular, write=FSpecular};
	__property Glvectortypes::TVector3f Emissive = {read=FEmissive, write=FEmissive};
	__property System::UnicodeString Texture = {read=FTexture, write=FTexture};
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TDXMaterial() : Glpersistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TDXMaterial(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TDXMaterial() { }
	
};


class PASCALIMPLEMENTATION TDXMaterialList : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	TDXMaterial* __fastcall GetMaterial(int index);
	
public:
	__property TDXMaterial* Items[int index] = {read=GetMaterial};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXMaterialList(TDXNode* AOwner) : TDXNode(AOwner) { }
	/* TDXNode.Create */ inline __fastcall virtual TDXMaterialList() : TDXNode() { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXMaterialList() { }
	
};


class PASCALIMPLEMENTATION TDXFrame : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	Glvectortypes::TMatrix4f FMatrix;
	
public:
	__fastcall virtual TDXFrame();
	Glvectortypes::TMatrix4f __fastcall GlobalMatrix();
	__property Glvectortypes::TMatrix4f Matrix = {read=FMatrix, write=FMatrix};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXFrame(TDXNode* AOwner) : TDXNode(AOwner) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXFrame() { }
	
};


class PASCALIMPLEMENTATION TDXMesh : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectorlists::TAffineVectorList* FNormals;
	Glvectorlists::TAffineVectorList* FTexCoords;
	Glvectorlists::TIntegerList* FVertexIndices;
	Glvectorlists::TIntegerList* FNormalIndices;
	Glvectorlists::TIntegerList* FMaterialIndices;
	Glvectorlists::TIntegerList* FVertCountIndices;
	TDXMaterialList* FMaterialList;
	
public:
	__fastcall virtual TDXMesh();
	__fastcall virtual ~TDXMesh();
	__property Glvectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Glvectorlists::TAffineVectorList* Normals = {read=FNormals};
	__property Glvectorlists::TAffineVectorList* TexCoords = {read=FTexCoords};
	__property Glvectorlists::TIntegerList* VertexIndices = {read=FVertexIndices};
	__property Glvectorlists::TIntegerList* NormalIndices = {read=FNormalIndices};
	__property Glvectorlists::TIntegerList* MaterialIndices = {read=FMaterialIndices};
	__property Glvectorlists::TIntegerList* VertCountIndices = {read=FVertCountIndices};
	__property TDXMaterialList* MaterialList = {read=FMaterialList};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXMesh(TDXNode* AOwner) : TDXNode(AOwner) { }
	
};


class PASCALIMPLEMENTATION TDXFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TDXNode* FRootNode;
	TDXFileHeader FHeader;
	
protected:
	void __fastcall ParseText(System::Classes::TStream* Stream);
	void __fastcall ParseBinary(System::Classes::TStream* Stream);
	
public:
	__fastcall TDXFile();
	__fastcall virtual ~TDXFile();
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	__property TDXFileHeader Header = {read=FHeader};
	__property TDXNode* RootNode = {read=FRootNode};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEX)
using namespace Filex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FilexHPP
