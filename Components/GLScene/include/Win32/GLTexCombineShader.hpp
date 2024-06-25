// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexCombineShader.pas' rev: 36.00 (Windows)

#ifndef GltexcombineshaderHPP
#define GltexcombineshaderHPP

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
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLTextureCombiners.hpp>
#include <OpenGLTokens.hpp>
#include <GLState.hpp>
#include <XOpenGL.hpp>
#include <GLContext.hpp>
#include <GLUtils.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gltexcombineshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTexCombineShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLTexCombineShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	System::Classes::TStringList* FCombiners;
	Gltexturecombiners::TCombinerCache FCommandCache;
	bool FCombinerIsValid;
	bool FDesignTimeEnabled;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Glmaterial::TGLLibMaterialName FLibMaterial3Name;
	Glmaterial::TGLLibMaterial* currentLibMaterial3;
	Glmaterial::TGLLibMaterialName FLibMaterial4Name;
	Glmaterial::TGLLibMaterial* currentLibMaterial4;
	bool FApplied3;
	bool FApplied4;
	
protected:
	void __fastcall SetCombiners(System::Classes::TStringList* const val);
	void __fastcall SetDesignTimeEnabled(const bool val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetLibMaterial3Name(const Glmaterial::TGLLibMaterialName val);
	void __fastcall SetLibMaterial4Name(const Glmaterial::TGLLibMaterialName val);
	void __fastcall NotifyLibMaterial3Destruction();
	void __fastcall NotifyLibMaterial4Destruction();
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoFinalize();
	
public:
	__fastcall virtual TGLTexCombineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexCombineShader();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property System::Classes::TStringList* Combiners = {read=FCombiners, write=SetCombiners};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property Glmaterial::TGLLibMaterialName LibMaterial3Name = {read=FLibMaterial3Name, write=SetLibMaterial3Name};
	__property Glmaterial::TGLLibMaterialName LibMaterial4Name = {read=FLibMaterial4Name, write=SetLibMaterial4Name};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexcombineshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXCOMBINESHADER)
using namespace Gltexcombineshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexcombineshaderHPP
