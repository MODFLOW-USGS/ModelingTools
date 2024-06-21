// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHiddenLineShader.pas' rev: 36.00 (Windows)

#ifndef GlhiddenlineshaderHPP
#define GlhiddenlineshaderHPP

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
#include <GLColor.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLContext.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glhiddenlineshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLineSettings;
class DELPHICLASS TGLHiddenLineShader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLineSettings : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
private:
	Glcolor::TGLColor* FColor;
	float FWidth;
	Opengltokens::TGLushort FPattern;
	bool FForceMaterial;
	void __fastcall SetPattern(const Opengltokens::TGLushort value);
	void __fastcall SetColor(Glcolor::TGLColor* const v);
	void __fastcall SetWidth(const float Value);
	void __fastcall SetForceMaterial(bool v);
	
public:
	__fastcall virtual TGLLineSettings(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLLineSettings();
	void __fastcall Apply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property float Width = {read=FWidth, write=SetWidth};
	__property Glcolor::TGLColor* Color = {read=FColor, write=SetColor};
	__property Opengltokens::TGLushort Pattern = {read=FPattern, write=SetPattern, default=65535};
	__property bool ForceMaterial = {read=FForceMaterial, write=SetForceMaterial, default=0};
};


class PASCALIMPLEMENTATION TGLHiddenLineShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPassCount;
	bool FLineSmooth;
	bool FSolid;
	Glcolor::TGLColor* FBackGroundColor;
	TGLLineSettings* FFrontLine;
	TGLLineSettings* FBackLine;
	bool FLighting;
	Glscene::TGLShadeModel FShadeModel;
	void __fastcall SetlineSmooth(bool v);
	void __fastcall SetSolid(bool v);
	void __fastcall SetBackgroundColor(Glcolor::TGLColor* AColor);
	void __fastcall SetLighting(bool v);
	void __fastcall SetShadeModel(const Glscene::TGLShadeModel val);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHiddenLineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHiddenLineShader();
	
__published:
	__property TGLLineSettings* FrontLine = {read=FFrontLine, write=FFrontLine};
	__property TGLLineSettings* BackLine = {read=FBackLine, write=FBackLine};
	__property bool LineSmooth = {read=FLineSmooth, write=SetlineSmooth, default=0};
	__property bool Solid = {read=FSolid, write=SetSolid, default=0};
	__property Glcolor::TGLColor* BackgroundColor = {read=FBackGroundColor, write=SetBackgroundColor};
	__property bool SurfaceLit = {read=FLighting, write=SetLighting, default=1};
	__property Glscene::TGLShadeModel ShadeModel = {read=FShadeModel, write=SetShadeModel, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glhiddenlineshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHIDDENLINESHADER)
using namespace Glhiddenlineshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlhiddenlineshaderHPP
