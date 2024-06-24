// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glsmfmod.pas' rev: 36.00 (Windows)

#ifndef GlsmfmodHPP
#define GlsmfmodHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <Glsound.hpp>
#include <Glscene.hpp>
#include <Glvectorgeometry.hpp>
#include <Fmod.hpp>
#include <Fmodtypes.hpp>
#include <Fmodpresets.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsmfmod
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSMFMOD;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSMFMOD : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
private:
	bool FActivated;
	bool FEAXCapable;
	
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
	__fastcall virtual TGLSMFMOD(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMFMOD();
	virtual void __fastcall UpdateSources();
	virtual float __fastcall CPUUsagePercent();
	virtual bool __fastcall EAXSupported();
	
__published:
	__property MaxChannels = {default=32};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsmfmod */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMFMOD)
using namespace Glsmfmod;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmfmodHPP
