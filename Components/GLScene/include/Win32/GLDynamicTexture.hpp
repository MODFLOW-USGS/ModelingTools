// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDynamicTexture.pas' rev: 36.00 (Windows)

#ifndef GldynamictextureHPP
#define GldynamictextureHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLCrossPlatform.hpp>
#include <GLContext.hpp>
#include <GLTexture.hpp>
#include <GLTextureFormat.hpp>
#include <GLGraphics.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gldynamictexture
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDynamicTextureImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDynamicTextureImage : public Gltexture::TGLBlankImage
{
	typedef Gltexture::TGLBlankImage inherited;
	
private:
	int FUpdating;
	int FTexSize;
	void *FBuffer;
	Glcontext::TGLBufferObjectHandle* FPBO;
	void *FData;
	System::Types::TRect FDirtyRect;
	bool FUseBGR;
	bool FUsePBO;
	void __fastcall SetDirtyRectangle(const System::Types::TRect &Value);
	void __fastcall SetUsePBO(const bool Value);
	
protected:
	int __fastcall GetTexSize();
	int __fastcall GetBitsPerPixel();
	int __fastcall GetDataFormat();
	int __fastcall GetTextureFormat();
	void __fastcall FreePBO();
	void __fastcall FreeBuffer();
	__property int BitsPerPixel = {read=GetBitsPerPixel, nodefault};
	__property int DataFormat = {read=GetDataFormat, nodefault};
	__property int TextureFormat = {read=GetTextureFormat, nodefault};
	
public:
	__fastcall virtual TGLDynamicTextureImage(System::Classes::TPersistent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	__property void * Data = {read=FData};
	__property System::Types::TRect DirtyRectangle = {read=FDirtyRect, write=SetDirtyRectangle};
	__property bool UseBGR = {read=FUseBGR, write=FUseBGR, nodefault};
	__property bool UsePBO = {read=FUsePBO, write=SetUsePBO, nodefault};
public:
	/* TGLBlankImage.Destroy */ inline __fastcall virtual ~TGLDynamicTextureImage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gldynamictexture */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDYNAMICTEXTURE)
using namespace Gldynamictexture;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GldynamictextureHPP
