// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultisampleImage.pas' rev: 34.00 (Windows)

#ifndef GlmultisampleimageHPP
#define GlmultisampleimageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <OpenGLTokens.hpp>
#include <GLContext.hpp>
#include <GLTexture.hpp>
#include <GLGraphics.hpp>
#include <GLTextureFormat.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmultisampleimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultisampleImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMultisampleImage : public Gltexture::TGLTextureImage
{
	typedef Gltexture::TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FBitmap;
	int FSamplesCount;
	int FWidth;
	int FHeight;
	int FDepth;
	bool FFixedSamplesLocation;
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	void __fastcall SetSamplesCount(int val);
	void __fastcall SetFixedSamplesLocation(bool val);
	
protected:
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget();
	
public:
	__fastcall virtual TGLMultisampleImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMultisampleImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual bool __fastcall IsSelfLoading();
	virtual void __fastcall LoadTexture(Gltextureformat::TGLInternalFormat AInternalFormat);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=256};
	__property int Height = {read=GetHeight, write=SetHeight, default=256};
	__property int Depth = {read=GetDepth, write=SetDepth, default=0};
	__property int SamplesCount = {read=FSamplesCount, write=SetSamplesCount, default=0};
	__property bool FixedSamplesLocation = {read=FFixedSamplesLocation, write=SetFixedSamplesLocation, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultisampleimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTISAMPLEIMAGE)
using namespace Glmultisampleimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultisampleimageHPP
