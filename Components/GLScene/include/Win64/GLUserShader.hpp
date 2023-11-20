// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLUserShader.pas' rev: 34.00 (Windows)

#ifndef GlusershaderHPP
#define GlusershaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLMaterial.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glusershader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLUserShader;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnDoApplyEvent)(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);

typedef void __fastcall (__closure *TOnDoUnApplyEvent)(System::TObject* Sender, int Pass, Glrendercontextinfo::TGLRenderContextInfo &rci, bool &Continue);

class PASCALIMPLEMENTATION TGLUserShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPass;
	TOnDoApplyEvent FOnDoApply;
	TOnDoUnApplyEvent FOnDoUnApply;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TOnDoApplyEvent OnDoApply = {read=FOnDoApply, write=FOnDoApply};
	__property TOnDoUnApplyEvent OnDoUnApply = {read=FOnDoUnApply, write=FOnDoUnApply};
	__property ShaderStyle = {default=1};
public:
	/* TGLShader.Create */ inline __fastcall virtual TGLUserShader(System::Classes::TComponent* AOwner) : Glmaterial::TGLShader(AOwner) { }
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLUserShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glusershader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLUSERSHADER)
using namespace Glusershader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlusershaderHPP
