// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilemd5.pas' rev: 36.00 (Windows)

#ifndef Glfilemd5HPP
#define Glfilemd5HPP

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
#include <Glvectorfileobjects.hpp>
#include <Glpersistentclasses.hpp>
#include <Glutils.hpp>
#include <Glapplicationfileio.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorgeometry.hpp>
#include <Glvectorlists.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilemd5
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMD5VectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMD5VectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
private:
	System::Classes::TStringList* FMD5String;
	System::Classes::TStringList* FTempString;
	System::Classes::TStringList* FBoneNames;
	int FCurrentPos;
	Glvectorfileobjects::TGLSkeletonFrame* FBasePose;
	Glvectorlists::TAffineVectorList* FFramePositions;
	Glvectorlists::TQuaternionList* FFrameQuaternions;
	Glvectorlists::TIntegerList* FJointFlags;
	int FNumFrames;
	int FFirstFrame;
	int FFrameRate;
	int FNumJoints;
	System::UnicodeString __fastcall ReadLine();
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLMD5VectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMD5VectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Classes::TStringList* vMD5TextureExtensions;
}	/* namespace Glfilemd5 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEMD5)
using namespace Glfilemd5;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilemd5HPP
