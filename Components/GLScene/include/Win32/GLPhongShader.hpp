// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPhongShader.pas' rev: 35.00 (Windows)

#ifndef GlphongshaderHPP
#define GlphongshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLTexture.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLAsmShader.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLCustomShader.hpp>
#include <GLState.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glphongshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPhongShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPhongShader : public Glasmshader::TGLCustomAsmShader
{
	typedef Glasmshader::TGLCustomAsmShader inherited;
	
private:
	Glvectorlists::TIntegerList* FLightIDs;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	void __fastcall SetDesignTimeEnabled(const bool Value);
	
protected:
	virtual void __fastcall DoLightPass(unsigned lightID);
	virtual void __fastcall DoAmbientPass(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall UnApplyLights(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoInitialize(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLPhongShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPhongShader();
	virtual bool __fastcall ShaderSupported();
	
__published:
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glphongshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPHONGSHADER)
using namespace Glphongshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlphongshaderHPP
