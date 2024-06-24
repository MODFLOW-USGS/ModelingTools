// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glfilebmp.pas' rev: 36.00 (Windows)

#ifndef GlfilebmpHPP
#define GlfilebmpHPP

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
#include <Glbaseclasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfilebmp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBMPImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBMPImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	bool FTopDown;
	System::LongWord RedMask;
	System::LongWord GreenMask;
	System::LongWord BlueMask;
	System::Int8 RedShift;
	System::Int8 GreenShift;
	System::Int8 BlueShift;
	System::Sysutils::PByteArray FLineBuffer;
	int FReadSize;
	int FDeltaX;
	int FDeltaY;
	System::Int8 __fastcall CountBits(System::Byte Value);
	System::Int8 __fastcall ShiftCount(System::LongWord Mask);
	Glgraphics::TPixel32 __fastcall ExpandColor(System::LongWord Value);
	void __fastcall ExpandRLE4ScanLine(int Row, System::Classes::TStream* Stream);
	void __fastcall ExpandRLE8ScanLine(int Row, System::Classes::TStream* Stream);
	int __fastcall Monochrome(int N);
	int __fastcall Quadrochrome(int N);
	int __fastcall Octochrome(int N);
	
public:
	virtual void __fastcall LoadFromFile(const System::UnicodeString filename);
	virtual void __fastcall SaveToFile(const System::UnicodeString filename);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* stream);
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLBMPImage() : Glgraphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLBMPImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilebmp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEBMP)
using namespace Glfilebmp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilebmpHPP
