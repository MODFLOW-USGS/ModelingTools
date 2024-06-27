// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileLMTS.pas' rev: 36.00 (Windows)

#ifndef GlfilelmtsHPP
#define GlfilelmtsHPP

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
#include <Vcl.Graphics.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorGeometry.hpp>
#include <GLTexture.hpp>
#include <GLPersistentClasses.hpp>
#include <GLGraphics.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilelmts
{
//-- forward type declarations -----------------------------------------------
struct TLMTS_Header;
struct TLMTS_TexData;
struct TLMTS_Subset;
struct TLMTS_Vertex;
struct TLMTS;
struct TMaterialInfo;
class DELPHICLASS TGLLMTSVectorFile;
//-- type declarations -------------------------------------------------------
typedef TLMTS_Header *PLMTS_Header;

struct DECLSPEC_DRECORD TLMTS_Header
{
public:
	unsigned ID;
	unsigned Ver;
	unsigned headerSize;
	System::Word nTexts;
	System::Word nSubsets;
	unsigned nTris;
	System::Word subSize;
	System::Word vtxSize;
};


typedef TLMTS_TexData *PLMTS_TexData;

struct DECLSPEC_DRECORD TLMTS_TexData
{
public:
	System::StaticArray<char, 256> fName;
	System::Word Flags;
};


typedef TLMTS_Subset *PLMTS_Subset;

struct DECLSPEC_DRECORD TLMTS_Subset
{
public:
	System::LongInt Offset;
	System::LongInt Count;
	System::Word TextID1;
	System::Word TextID2;
};


typedef TLMTS_Vertex *PLMTS_Vertex;

struct DECLSPEC_DRECORD TLMTS_Vertex
{
public:
	float x;
	float y;
	float z;
	float u1;
	float v1;
	float u2;
	float v2;
};


typedef TLMTS *PLMTS;

struct DECLSPEC_DRECORD TLMTS
{
public:
	TLMTS_Header header;
	void *usrData;
	unsigned usrSize;
	void *texData;
	void *subsets;
	void *tris;
	bool ok;
};


struct DECLSPEC_DRECORD TMaterialInfo
{
public:
	Glmaterial::TShininess FShininess;
	Glmaterial::TShininess BShininess;
	Glvectorgeometry::TVector FAmbient;
	Glvectorgeometry::TVector FDiffuse;
	Glvectorgeometry::TVector FEmission;
	Glvectorgeometry::TVector FSpecular;
	Glvectorgeometry::TVector BAmbient;
	Glvectorgeometry::TVector BDiffuse;
	Glvectorgeometry::TVector BEmission;
	Glvectorgeometry::TVector BSpecular;
	Gltexture::TGLTextureImageAlpha ImageAlpha;
	Gltexture::TGLMagFilter magFilter;
	Gltexture::TGLMinFilter minFilter;
	Gltexture::TGLTextureMode TextureMode;
	Gltexture::TGLTextureWrap TextureWrap;
	Glmaterial::TGLBlendingMode Blendingmode;
	Glmaterial::TFaceCulling FaceCulling;
	int mathash;
};


class PASCALIMPLEMENTATION TGLLMTSVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLLMTSVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLMTSVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const int C_LMTS_ID = int(0x53544d4c);
static const System::Int8 C_LMTS_VER = System::Int8(0x4);
static const int C_LMTS_SUBS = int(0x53425553);
static const int C_LMTS_TEXT = int(0x54584554);
static const int C_LMTS_TRIS = int(0x53495254);
static const System::Byte C_LMTS_TEXFNLEN = System::Byte(0xff);
}	/* namespace Glfilelmts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILELMTS)
using namespace Glfilelmts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilelmtsHPP
