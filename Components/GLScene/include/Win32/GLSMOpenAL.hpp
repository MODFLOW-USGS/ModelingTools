// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSMOpenAL.pas' rev: 36.00 (Windows)

#ifndef GLSMOpenALHPP
#define GLSMOpenALHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <GLScene.hpp>
#include <GLSound.hpp>
#include <GLSoundFileObjects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsmopenal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSMOpenAL;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSMOpenAL : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
private:
	bool FActivated;
	
protected:
	virtual bool __fastcall DoActivate();
	virtual void __fastcall DoDeActivate();
	virtual void __fastcall NotifyMasterVolumeChange();
	virtual void __fastcall Notify3DFactorsChanged();
	virtual void __fastcall NotifyEnvironmentChanged();
	virtual void __fastcall KillSource(Glsound::TGLBaseSoundSource* aSource);
	virtual void __fastcall UpdateSource(Glsound::TGLBaseSoundSource* aSource);
	virtual void __fastcall MuteSource(Glsound::TGLBaseSoundSource* aSource, bool muted);
	virtual void __fastcall PauseSource(Glsound::TGLBaseSoundSource* aSource, bool paused);
	int __fastcall GetDefaultFrequency(Glsound::TGLBaseSoundSource* aSource);
	int __fastcall GetALFormat(Glsoundfileobjects::TGLSoundSampling* sampling);
	
public:
	__fastcall virtual TGLSMOpenAL(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMOpenAL();
	virtual void __fastcall UpdateSources();
	virtual bool __fastcall EAXSupported();
};


typedef System::Sysutils::Exception EOpenALError;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsmopenal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMOPENAL)
using namespace Glsmopenal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLSMOpenALHPP
