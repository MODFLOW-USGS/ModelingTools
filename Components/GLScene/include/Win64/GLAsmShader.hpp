// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAsmShader.pas' rev: 35.00 (Windows)

#ifndef GlasmshaderHPP
#define GlasmshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <GLTexture.hpp>
#include <GLContext.hpp>
#include <GLCustomShader.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glasmshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLAsmShaderParameter;
class DELPHICLASS TGLCustomAsmShader;
class DELPHICLASS TGLAsmShader;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGLAsmShaderEvent)(TGLCustomAsmShader* Shader);

typedef void __fastcall (__closure *TGLAsmShaderUnUplyEvent)(TGLCustomAsmShader* Shader, bool &ThereAreMorePasses);

class PASCALIMPLEMENTATION TGLAsmShaderParameter : public Glcustomshader::TGLCustomShaderParameter
{
	typedef Glcustomshader::TGLCustomShaderParameter inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLAsmShaderParameter() : Glcustomshader::TGLCustomShaderParameter() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLAsmShaderParameter() { }
	
};


class PASCALIMPLEMENTATION TGLCustomAsmShader : public Glcustomshader::TGLCustomShader
{
	typedef Glcustomshader::TGLCustomShader inherited;
	
private:
	Glcontext::TGLARBVertexProgramHandle* FVPHandle;
	Glcontext::TGLARBFragmentProgramHandle* FFPHandle;
	Glcontext::TGLARBGeometryProgramHandle* FGPHandle;
	TGLAsmShaderEvent FOnInitialize;
	TGLAsmShaderEvent FOnApply;
	TGLAsmShaderUnUplyEvent FOnUnApply;
	
protected:
	void __fastcall ApplyShaderPrograms();
	void __fastcall UnApplyShaderPrograms();
	virtual void __fastcall DestroyARBPrograms();
	__property TGLAsmShaderEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TGLAsmShaderUnUplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
	__property TGLAsmShaderEvent OnInitialize = {read=FOnInitialize, write=FOnInitialize};
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoFinalize();
	
public:
	__fastcall virtual TGLCustomAsmShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomAsmShader();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported();
};


class PASCALIMPLEMENTATION TGLAsmShader : public TGLCustomAsmShader
{
	typedef TGLCustomAsmShader inherited;
	
__published:
	__property FragmentProgram;
	__property VertexProgram;
	__property GeometryProgram;
	__property OnApply;
	__property OnUnApply;
	__property OnInitialize;
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
public:
	/* TGLCustomAsmShader.Create */ inline __fastcall virtual TGLAsmShader(System::Classes::TComponent* AOwner) : TGLCustomAsmShader(AOwner) { }
	/* TGLCustomAsmShader.Destroy */ inline __fastcall virtual ~TGLAsmShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glasmshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLASMSHADER)
using namespace Glasmshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlasmshaderHPP
