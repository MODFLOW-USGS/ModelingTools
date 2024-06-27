// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiMaterialShader.pas' rev: 36.00 (Windows)

#ifndef GlmultimaterialshaderHPP
#define GlmultimaterialshaderHPP

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
#include <GLMaterial.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmultimaterialshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultiMaterialShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMultiMaterialShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPass;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FVisibleAtDesignTime;
	bool FShaderActiveAtDesignTime;
	Glmaterial::TGLShaderStyle FShaderStyle;
	void __fastcall SetVisibleAtDesignTime(const bool Value);
	void __fastcall SetShaderStyle(const Glmaterial::TGLShaderStyle Value);
	
protected:
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLMultiMaterialShader(System::Classes::TComponent* aOwner);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property bool VisibleAtDesignTime = {read=FVisibleAtDesignTime, write=SetVisibleAtDesignTime, nodefault};
	__property Glmaterial::TGLShaderStyle ShaderStyle = {read=FShaderStyle, write=SetShaderStyle, nodefault};
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLMultiMaterialShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultimaterialshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIMATERIALSHADER)
using namespace Glmultimaterialshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultimaterialshaderHPP
