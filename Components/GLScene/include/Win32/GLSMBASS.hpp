// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSMBASS.pas' rev: 35.00 (Windows)

#ifndef GlsmbassHPP
#define GlsmbassHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <GLSound.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <Bass.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsmbass
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSMBASS;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBASS3DAlgorithm : unsigned char { algDefault, algOff, algFull, algLight };

class PASCALIMPLEMENTATION TGLSMBASS : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
private:
	bool FActivated;
	TBASS3DAlgorithm FAlgorithm3D;
	
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
	
public:
	__fastcall virtual TGLSMBASS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMBASS();
	virtual void __fastcall UpdateSources();
	virtual float __fastcall CPUUsagePercent();
	virtual bool __fastcall EAXSupported();
	
__published:
	__property TBASS3DAlgorithm Algorithm3D = {read=FAlgorithm3D, write=FAlgorithm3D, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsmbass */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMBASS)
using namespace Glsmbass;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmbassHPP
