// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCelShader.pas' rev: 36.00 (Windows)

#ifndef GlcelshaderHPP
#define GlcelshaderHPP

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
#include <OpenGLTokens.hpp>
#include <GLTexture.hpp>
#include <GLContext.hpp>
#include <GLGraphics.hpp>
#include <GLUtils.hpp>
#include <GLVectorGeometry.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcelshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCelShader;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLCelShaderOption : unsigned char { csoOutlines, csoTextured, csoNoBuildShadeTexture };

typedef System::Set<TGLCelShaderOption, TGLCelShaderOption::csoOutlines, TGLCelShaderOption::csoNoBuildShadeTexture> TGLCelShaderOptions;

typedef void __fastcall (__closure *TGLCelShaderGetIntensity)(System::TObject* Sender, System::Byte &intensity);

class PASCALIMPLEMENTATION TGLCelShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	float FOutlineWidth;
	TGLCelShaderOptions FCelShaderOptions;
	Glcontext::TGLARBVertexProgramHandle* FVPHandle;
	Gltexture::TGLTexture* FShadeTexture;
	TGLCelShaderGetIntensity FOnGetIntensity;
	bool FOutlinePass;
	bool FUnApplyShadeTexture;
	Glcolor::TGLColor* FOutlineColor;
	
protected:
	void __fastcall SetCelShaderOptions(const TGLCelShaderOptions val);
	void __fastcall SetOutlineWidth(const float val);
	void __fastcall SetOutlineColor(Glcolor::TGLColor* const val);
	void __fastcall BuildShadeTexture();
	virtual void __fastcall Loaded();
	System::UnicodeString __fastcall GenerateVertexProgram();
	
public:
	__fastcall virtual TGLCelShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCelShader();
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property Gltexture::TGLTexture* ShadeTexture = {read=FShadeTexture};
	
__published:
	__property TGLCelShaderOptions CelShaderOptions = {read=FCelShaderOptions, write=SetCelShaderOptions, nodefault};
	__property Glcolor::TGLColor* OutlineColor = {read=FOutlineColor, write=SetOutlineColor};
	__property float OutlineWidth = {read=FOutlineWidth, write=SetOutlineWidth};
	__property TGLCelShaderGetIntensity OnGetIntensity = {read=FOnGetIntensity, write=FOnGetIntensity};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcelshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCELSHADER)
using namespace Glcelshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcelshaderHPP
