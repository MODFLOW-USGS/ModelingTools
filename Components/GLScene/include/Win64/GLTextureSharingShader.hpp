// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTextureSharingShader.pas' rev: 36.00 (Windows)

#ifndef GltexturesharingshaderHPP
#define GltexturesharingshaderHPP

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
#include <GLScene.hpp>
#include <GLContext.hpp>
#include <GLTexture.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLColor.hpp>
#include <GLMaterial.hpp>
#include <GLStrings.hpp>
#include <GLVectorFileObjects.hpp>
#include <XOpenGL.hpp>
#include <GLState.hpp>
#include <GLPersistentClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltexturesharingshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureSharingShaderMaterial;
class DELPHICLASS TGLTextureSharingShaderMaterials;
class DELPHICLASS TGLTextureSharingShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterial : public Glpersistentclasses::TGLInterfacedCollectionItem
{
	typedef Glpersistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Glvectorgeometry::TMatrix FTextureMatrix;
	bool FNeedToUpdateTextureMatrix;
	bool FTextureMatrixIsUnitary;
	Glmaterial::TGLLibMaterial* FLibMaterial;
	Glcoordinates::TGLCoordinates2* FTexOffset;
	Glcoordinates::TGLCoordinates2* FTexScale;
	Glmaterial::TGLBlendingMode FBlendingMode;
	Glcolor::TGLColor* FSpecular;
	Glcolor::TGLColor* FAmbient;
	Glcolor::TGLColor* FDiffuse;
	Glcolor::TGLColor* FEmission;
	Glmaterial::TShininess FShininess;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Glmaterial::TGLLibMaterialName FLibMaterialName;
	void __fastcall SetAmbient(Glcolor::TGLColor* const Value);
	void __fastcall SetDiffuse(Glcolor::TGLColor* const Value);
	void __fastcall SetEmission(Glcolor::TGLColor* const Value);
	void __fastcall SetShininess(const Glmaterial::TShininess Value);
	void __fastcall SetSpecular(Glcolor::TGLColor* const Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetLibMaterialName(const Glmaterial::TGLLibMaterialName Value);
	void __fastcall SetBlendingMode(const Glmaterial::TGLBlendingMode Value);
	void __fastcall SetLibMaterial(Glmaterial::TGLLibMaterial* const Value);
	void __fastcall SetTexOffset(Glcoordinates::TGLCoordinates2* const Value);
	void __fastcall SetTexScale(Glcoordinates::TGLCoordinates2* const Value);
	Glvectorgeometry::TMatrix __fastcall GetTextureMatrix();
	bool __fastcall GetTextureMatrixIsUnitary();
	
protected:
	void __fastcall coordNotifychange(System::TObject* Sender);
	void __fastcall OtherNotifychange(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDisplayName();
	TGLTextureSharingShader* __fastcall GetTextureSharingShader();
	virtual Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
public:
	void __fastcall Apply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__fastcall virtual TGLTextureSharingShaderMaterial(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLTextureSharingShaderMaterial();
	__property Glmaterial::TGLLibMaterial* LibMaterial = {read=FLibMaterial, write=SetLibMaterial};
	__property Glvectorgeometry::TMatrix TextureMatrix = {read=GetTextureMatrix};
	__property bool TextureMatrixIsUnitary = {read=GetTextureMatrixIsUnitary, nodefault};
	
__published:
	__property Glcoordinates::TGLCoordinates2* TexOffset = {read=FTexOffset, write=SetTexOffset};
	__property Glcoordinates::TGLCoordinates2* TexScale = {read=FTexScale, write=SetTexScale};
	__property Glmaterial::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, nodefault};
	__property Glcolor::TGLColor* Emission = {read=FEmission, write=SetEmission};
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Glcolor::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property Glcolor::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
	__property Glmaterial::TShininess Shininess = {read=FShininess, write=SetShininess, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property Glmaterial::TGLLibMaterialName LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
private:
	void *__IGLMaterialLibrarySupported;	// Glmaterial::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterials : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLTextureSharingShaderMaterial* operator[](const int AIndex) { return this->Items[AIndex]; }
	
protected:
	TGLTextureSharingShaderMaterial* __fastcall GetItems(const int AIndex);
	void __fastcall SetItems(const int AIndex, TGLTextureSharingShaderMaterial* const Value);
	TGLTextureSharingShader* __fastcall GetParent();
	
public:
	HIDESBASE TGLTextureSharingShaderMaterial* __fastcall Add();
	__fastcall TGLTextureSharingShaderMaterials(TGLTextureSharingShader* AOwner);
	__property TGLTextureSharingShaderMaterial* Items[const int AIndex] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureSharingShaderMaterials() { }
	
};


class PASCALIMPLEMENTATION TGLTextureSharingShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	TGLTextureSharingShaderMaterials* FMaterials;
	int FCurrentPass;
	void __fastcall SetMaterials(TGLTextureSharingShaderMaterials* const Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTextureSharingShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureSharingShader();
	TGLTextureSharingShaderMaterial* __fastcall AddLibMaterial(Glmaterial::TGLLibMaterial* const ALibMaterial);
	TGLTextureSharingShaderMaterial* __fastcall FindLibMaterial(Glmaterial::TGLLibMaterial* const ALibMaterial);
	
__published:
	__property TGLTextureSharingShaderMaterials* Materials = {read=FMaterials, write=SetMaterials};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexturesharingshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTURESHARINGSHADER)
using namespace Gltexturesharingshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexturesharingshaderHPP
