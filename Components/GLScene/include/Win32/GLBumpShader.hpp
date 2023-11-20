// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBumpShader.pas' rev: 35.00 (Windows)

#ifndef GlbumpshaderHPP
#define GlbumpshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLMaterial.hpp>
#include <GLGraphics.hpp>
#include <GLUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLVectorLists.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glbumpshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBumpShader;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBumpMethod : unsigned char { bmDot3TexCombiner, bmBasicARBFP };

enum DECLSPEC_DENUM TBumpSpace : unsigned char { bsObject, bsTangentExternal, bsTangentQuaternion };

enum DECLSPEC_DENUM TBumpOption : unsigned char { boDiffuseTexture2, boSpecularTexture3, boUseSecondaryTexCoords, boLightAttenuation, boParallaxMapping };

typedef System::Set<TBumpOption, TBumpOption::boDiffuseTexture2, TBumpOption::boParallaxMapping> TBumpOptions;

enum DECLSPEC_DENUM TSpecularMode : unsigned char { smOff, smBlinn, smPhong };

class PASCALIMPLEMENTATION TGLBumpShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	Glcontext::TGLARBVertexProgramHandle* FVertexProgramHandle;
	Glcontext::TGLARBFragmentProgramHandle* FFragmentProgramHandle;
	Glvectorlists::TIntegerList* FLightIDs;
	int FLightsEnabled;
	TBumpMethod FBumpMethod;
	TBumpSpace FBumpSpace;
	TBumpOptions FBumpOptions;
	TSpecularMode FSpecularMode;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	bool FDiffusePass;
	System::Classes::TStringList* FVertexProgram;
	System::Classes::TStringList* FFragmentProgram;
	float FParallaxOffset;
	System::UnicodeString __fastcall GenerateVertexProgram();
	System::UnicodeString __fastcall GenerateFragmentProgram();
	void __fastcall DoLightPass(Glrendercontextinfo::TGLRenderContextInfo &rci, unsigned lightID);
	
protected:
	void __fastcall SetBumpMethod(const TBumpMethod Value);
	void __fastcall SetBumpSpace(const TBumpSpace Value);
	void __fastcall SetBumpOptions(const TBumpOptions Value);
	void __fastcall SetSpecularMode(const TSpecularMode Value);
	void __fastcall SetDesignTimeEnabled(const bool Value);
	void __fastcall SetParallaxOffset(const float Value);
	virtual void __fastcall Loaded();
	void __fastcall DeleteVertexPrograms();
	void __fastcall DeleteFragmentPrograms();
	
public:
	__fastcall virtual TGLBumpShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpShader();
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TBumpMethod BumpMethod = {read=FBumpMethod, write=SetBumpMethod, nodefault};
	__property TBumpSpace BumpSpace = {read=FBumpSpace, write=SetBumpSpace, nodefault};
	__property TBumpOptions BumpOptions = {read=FBumpOptions, write=SetBumpOptions, nodefault};
	__property TSpecularMode SpecularMode = {read=FSpecularMode, write=SetSpecularMode, nodefault};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property float ParallaxOffset = {read=FParallaxOffset, write=SetParallaxOffset};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbumpshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBUMPSHADER)
using namespace Glbumpshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbumpshaderHPP
