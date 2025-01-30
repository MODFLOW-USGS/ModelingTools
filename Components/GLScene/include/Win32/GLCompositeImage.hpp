// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCompositeImage.pas' rev: 36.00 (Windows)

#ifndef GLCompositeImageHPP
#define GLCompositeImageHPP

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
#include <GLContext.hpp>
#include <OpenGLTokens.hpp>
#include <GLGraphics.hpp>
#include <GLTexture.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcompositeimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCompositeImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCompositeImage : public Gltexture::TGLTextureImage
{
	typedef Gltexture::TGLTextureImage inherited;
	
private:
	Glgraphics::TGLBitmap32* FBitmap;
	int FWidth;
	int FHeight;
	int FDepth;
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLCompositeImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLCompositeImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glgraphics::TGLBitmap32* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall LoadFromStream(System::Classes::TStream* const AStream);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcompositeimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOMPOSITEIMAGE)
using namespace Glcompositeimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLCompositeImageHPP
