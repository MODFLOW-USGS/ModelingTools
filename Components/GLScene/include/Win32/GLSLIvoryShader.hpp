// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLIvoryShader.pas' rev: 36.00 (Windows)

#ifndef GlslivoryshaderHPP
#define GlslivoryshaderHPP

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
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLBaseClasses.hpp>
#include <GLState.hpp>
#include <GLContext.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLVectorGeometry.hpp>
#include <GLCoordinates.hpp>
#include <GLTextureFormat.hpp>
#include <GLColor.hpp>
#include <GLTexture.hpp>
#include <GLMaterial.hpp>
#include <GLSLShader.hpp>
#include <GLCustomShader.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glslivoryshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLIvoryShader;
class DELPHICLASS TGLSLIvoryShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLIvoryShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLIvoryShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLIvoryShader();
};


class PASCALIMPLEMENTATION TGLSLIvoryShader : public TGLCustomGLSLIvoryShader
{
	typedef TGLCustomGLSLIvoryShader inherited;
	
public:
	/* TGLCustomGLSLIvoryShader.Create */ inline __fastcall virtual TGLSLIvoryShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLIvoryShader(AOwner) { }
	/* TGLCustomGLSLIvoryShader.Destroy */ inline __fastcall virtual ~TGLSLIvoryShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslivoryshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLIVORYSHADER)
using namespace Glslivoryshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslivoryshaderHPP
