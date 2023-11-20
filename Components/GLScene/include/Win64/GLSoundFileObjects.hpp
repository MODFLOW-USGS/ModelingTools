// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSoundFileObjects.pas' rev: 34.00 (Windows)

#ifndef GlsoundfileobjectsHPP
#define GlsoundfileobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.MMSystem.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Consts.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLCrossPlatform.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsoundfileobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSoundSampling;
class DELPHICLASS TGLSoundFile;
struct TGLSoundFileFormat;
class DELPHICLASS TGLSoundFileFormatsList;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSoundSampling : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	int FFrequency;
	int FNbChannels;
	int FBitsPerSample;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TGLSoundSampling(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSoundSampling();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	int __fastcall BytesPerSec();
	int __fastcall BytesPerSample();
	tWAVEFORMATEX __fastcall WaveFormat();
	
__published:
	__property int Frequency = {read=FFrequency, write=FFrequency, default=22050};
	__property int NbChannels = {read=FNbChannels, write=FNbChannels, default=1};
	__property int BitsPerSample = {read=FBitsPerSample, write=FBitsPerSample, default=8};
};


class PASCALIMPLEMENTATION TGLSoundFile : public Glapplicationfileio::TGLDataFile
{
	typedef Glapplicationfileio::TGLDataFile inherited;
	
private:
	TGLSoundSampling* FSampling;
	
protected:
	void __fastcall SetSampling(TGLSoundSampling* const val);
	
public:
	__fastcall virtual TGLSoundFile(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSoundFile();
	virtual void __fastcall PlayOnWaveOut();
	virtual void * __fastcall WAVData() = 0 ;
	virtual int __fastcall WAVDataSize() = 0 ;
	virtual void * __fastcall PCMData() = 0 ;
	virtual int __fastcall LengthInBytes() = 0 ;
	int __fastcall LengthInSamples();
	float __fastcall LengthInSec();
	__property TGLSoundSampling* Sampling = {read=FSampling, write=SetSampling};
};


_DECLARE_METACLASS(System::TMetaClass, TGLSoundFileClass);

struct DECLSPEC_DRECORD TGLSoundFileFormat
{
public:
	TGLSoundFileClass SoundFileClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
};


typedef TGLSoundFileFormat *PSoundFileFormat;

class PASCALIMPLEMENTATION TGLSoundFileFormatsList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TGLSoundFileFormatsList();
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TGLSoundFileClass AClass);
	TGLSoundFileClass __fastcall FindExt(System::UnicodeString Ext);
	HIDESBASE void __fastcall Remove(TGLSoundFileClass AClass);
	void __fastcall BuildFilterStrings(TGLSoundFileClass SoundFileClass, /* out */ System::UnicodeString &Descriptions, /* out */ System::UnicodeString &Filters);
public:
	/* TObject.Create */ inline __fastcall TGLSoundFileFormatsList() : System::Classes::TList() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLSoundFileFormatsList* __fastcall GetGLSoundFileFormats(void);
extern DELPHI_PACKAGE void __fastcall RegisterSoundFileFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TGLSoundFileClass AClass);
extern DELPHI_PACKAGE void __fastcall UnregisterSoundFileClass(TGLSoundFileClass AClass);
}	/* namespace Glsoundfileobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSOUNDFILEOBJECTS)
using namespace Glsoundfileobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsoundfileobjectsHPP
