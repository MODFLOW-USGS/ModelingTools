// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileWAV.pas' rev: 36.00 (Windows)

#ifndef GlfilewavHPP
#define GlfilewavHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.MMSystem.hpp>
#include <System.Classes.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLSoundFileObjects.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilewav
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLWAVFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLWAVFile : public Glsoundfileobjects::TGLSoundFile
{
	typedef Glsoundfileobjects::TGLSoundFile inherited;
	
	
private:
	typedef System::DynamicArray<System::Byte> _TGLWAVFile__1;
	
	
private:
	tWAVEFORMATEX waveFormat;
	int pcmOffset;
	int FPCMDataLength;
	_TGLWAVFile__1 data;
	
public:
	virtual Glapplicationfileio::TGLDataFile* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall PlayOnWaveOut();
	virtual void * __fastcall WAVData();
	virtual int __fastcall WAVDataSize();
	virtual void * __fastcall PCMData();
	virtual int __fastcall LengthInBytes();
public:
	/* TGLSoundFile.Create */ inline __fastcall virtual TGLWAVFile(System::Classes::TPersistent* AOwner) : Glsoundfileobjects::TGLSoundFile(AOwner) { }
	/* TGLSoundFile.Destroy */ inline __fastcall virtual ~TGLWAVFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilewav */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEWAV)
using namespace Glfilewav;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilewavHPP
