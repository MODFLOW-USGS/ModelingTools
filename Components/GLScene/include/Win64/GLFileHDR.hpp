// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilehdr.pas' rev: 36.00 (Windows)

#ifndef GlfilehdrHPP
#define GlfilehdrHPP

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
#include <Opengltokens.hpp>
#include <Glcontext.hpp>
#include <Glgraphics.hpp>
#include <Gltextureformat.hpp>
#include <Glapplicationfileio.hpp>
#include <Glsrgbe.hpp>
#include <Glvectortypes.hpp>
#include <Glvectorgeometry.hpp>
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilehdr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLHDRImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLHDRImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	System::AnsiString __fastcall GetProgramType();
	void __fastcall SetProgramType(System::AnsiString aval);
	
protected:
	float fGamma;
	float fExposure;
	System::SmallString<16> fProgramType;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
	__property float Gamma = {read=fGamma};
	__property float Exposure = {read=fExposure};
	__property System::AnsiString ProgramType = {read=GetProgramType, write=SetProgramType};
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLHDRImage() : Glgraphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLHDRImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilehdr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEHDR)
using namespace Glfilehdr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilehdrHPP
