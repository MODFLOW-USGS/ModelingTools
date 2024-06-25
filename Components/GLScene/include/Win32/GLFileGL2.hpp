// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileGL2.pas' rev: 36.00 (Windows)

#ifndef Glfilegl2HPP
#define Glfilegl2HPP

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
#include <GLPersistentClasses.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLMaterial.hpp>
#include <GLVectorGeometry.hpp>
#include <FileGL2.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilegl2
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMVectorFile;
class DELPHICLASS TGLAVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLMVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMVectorFile() { }
	
};


class PASCALIMPLEMENTATION TGLAVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLAVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLAVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int vGhoul2LevelOfDetail;
extern DELPHI_PACKAGE int vGhoul2MaxBonesPerVertex;
}	/* namespace Glfilegl2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEGL2)
using namespace Glfilegl2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilegl2HPP
