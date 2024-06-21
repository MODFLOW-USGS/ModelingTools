// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShaderCombiner.pas' rev: 36.00 (Windows)

#ifndef GlshadercombinerHPP
#define GlshadercombinerHPP

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
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLStrings.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glshadercombiner
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomShaderCombiner;
class DELPHICLASS TGLShaderCombiner;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLShaderCombinerType : unsigned char { sctOneSPTwoAP, sctTwoSPOneAP, sctOneMPTwoSP, sctTwoMPOneSP };

class PASCALIMPLEMENTATION TGLCustomShaderCombiner : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FCurrentPass;
	TGLShaderCombinerType FCombinerType;
	Glmaterial::TGLShader* FShaderOne;
	Glmaterial::TGLShader* FShaderTwo;
	void __fastcall SetShaderOne(Glmaterial::TGLShader* const Value);
	void __fastcall SetShaderTwo(Glmaterial::TGLShader* const Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property TGLShaderCombinerType CombinerType = {read=FCombinerType, write=FCombinerType, default=0};
	__property Glmaterial::TGLShader* ShaderOne = {read=FShaderOne, write=SetShaderOne};
	__property Glmaterial::TGLShader* ShaderTwo = {read=FShaderTwo, write=SetShaderTwo};
	__property int CurrentPass = {read=FCurrentPass, stored=false, nodefault};
	
public:
	__fastcall virtual TGLCustomShaderCombiner(System::Classes::TComponent* AOwner);
	virtual bool __fastcall ShaderSupported();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLCustomShaderCombiner() { }
	
};


class PASCALIMPLEMENTATION TGLShaderCombiner : public TGLCustomShaderCombiner
{
	typedef TGLCustomShaderCombiner inherited;
	
__published:
	__property CombinerType = {default=0};
	__property ShaderOne;
	__property ShaderTwo;
	__property ShaderStyle = {default=1};
public:
	/* TGLCustomShaderCombiner.Create */ inline __fastcall virtual TGLShaderCombiner(System::Classes::TComponent* AOwner) : TGLCustomShaderCombiner(AOwner) { }
	
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLShaderCombiner() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glshadercombiner */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADERCOMBINER)
using namespace Glshadercombiner;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadercombinerHPP
