// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLToonShader.pas' rev: 35.00 (Windows)

#ifndef GlsltoonshaderHPP
#define GlsltoonshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
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

namespace Glsltoonshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomGLSLToonShader;
class DELPHICLASS TGLSLToonShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCustomGLSLToonShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	Glcolor::TGLColor* FHighlightColor;
	Glcolor::TGLColor* FMidColor;
	Glcolor::TGLColor* FLightenShadowColor;
	Glcolor::TGLColor* FDarkenShadowColor;
	Glcolor::TGLColor* FOutlineColor;
	float FHighlightSize;
	float FMidSize;
	float FShadowSize;
	float FOutlineWidth;
	void __fastcall SetHighLightColor(Glcolor::TGLColor* AValue);
	void __fastcall SetMidColor(Glcolor::TGLColor* AValue);
	void __fastcall SetLightenShadowColor(Glcolor::TGLColor* AValue);
	void __fastcall SetDarkenShadowColor(Glcolor::TGLColor* AValue);
	void __fastcall SetOutlineColor(Glcolor::TGLColor* AValue);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLToonShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLToonShader();
	__property Glcolor::TGLColor* HighlightColor = {read=FHighlightColor, write=SetHighLightColor};
	__property Glcolor::TGLColor* MidColor = {read=FMidColor, write=SetMidColor};
	__property Glcolor::TGLColor* LightenShadowColor = {read=FLightenShadowColor, write=SetLightenShadowColor};
	__property Glcolor::TGLColor* DarkenShadowrColor = {read=FDarkenShadowColor, write=SetDarkenShadowColor};
	__property Glcolor::TGLColor* OutlinetColor = {read=FOutlineColor, write=SetOutlineColor};
	__property float HighlightSize = {read=FHighlightSize, write=FHighlightSize};
	__property float MidSize = {read=FMidSize, write=FMidSize};
	__property float ShadowSize = {read=FShadowSize, write=FShadowSize};
	__property float OutlineWidth = {read=FOutlineWidth, write=FOutlineWidth};
};


class PASCALIMPLEMENTATION TGLSLToonShader : public TGLCustomGLSLToonShader
{
	typedef TGLCustomGLSLToonShader inherited;
	
__published:
	__property HighlightColor;
	__property MidColor;
	__property LightenShadowColor;
	__property DarkenShadowrColor;
	__property OutlinetColor;
	__property HighlightSize = {default=0};
	__property MidSize = {default=0};
	__property ShadowSize = {default=0};
	__property OutlineWidth = {default=0};
public:
	/* TGLCustomGLSLToonShader.Create */ inline __fastcall virtual TGLSLToonShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLToonShader(AOwner) { }
	/* TGLCustomGLSLToonShader.Destroy */ inline __fastcall virtual ~TGLSLToonShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsltoonshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLTOONSHADER)
using namespace Glsltoonshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsltoonshaderHPP
