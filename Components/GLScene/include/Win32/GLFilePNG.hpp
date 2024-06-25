// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFilePNG.pas' rev: 36.00 (Windows)

#ifndef GlfilepngHPP
#define GlfilepngHPP

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
#include <Vcl.Imaging.pngimage.hpp>
#include <GLStrings.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLGraphics.hpp>
#include <GLTextureFormat.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilepng
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPNGImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPNGImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall SaveToFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* AStream);
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLPNGImage() : Glgraphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLPNGImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilepng */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEPNG)
using namespace Glfilepng;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilepngHPP
