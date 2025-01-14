// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileJPEG.pas' rev: 36.00 (Windows)

#ifndef GlfilejpegHPP
#define GlfilejpegHPP

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
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLGraphics.hpp>
#include <GLTextureFormat.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorGeometry.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilejpeg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLJPEGImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLJPEGImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	bool FAbortLoading;
	System::LongWord FDivScale;
	bool FDither;
	bool FSmoothing;
	bool FProgressiveEncoding;
	void __fastcall SetSmoothing(const bool AValue);
	
public:
	__fastcall virtual TGLJPEGImage();
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall SaveToFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* AStream);
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
	__property System::LongWord DivScale = {read=FDivScale, write=FDivScale, nodefault};
	__property bool Dither = {read=FDither, write=FDither, nodefault};
	__property bool Smoothing = {read=FSmoothing, write=SetSmoothing, nodefault};
	__property bool ProgressiveEncoding = {read=FProgressiveEncoding, nodefault};
public:
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLJPEGImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Jpeg2Bmp(const System::UnicodeString BmpFileName, const System::UnicodeString JpgFileName);
}	/* namespace Glfilejpeg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEJPEG)
using namespace Glfilejpeg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilejpegHPP
