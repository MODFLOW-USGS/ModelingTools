// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFBO.pas' rev: 34.00 (Windows)

#ifndef GlfboHPP
#define GlfboHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLContext.hpp>
#include <GLState.hpp>
#include <GLTexture.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMultisampleImage.hpp>
#include <GLGraphics.hpp>
#include <GLTextureFormat.hpp>
#include <GLVectorTypes.hpp>
#include <GLSLog.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfbo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLRenderbuffer;
class DELPHICLASS TGLDepthRBO;
class DELPHICLASS TGLStencilRBO;
class DELPHICLASS TGLFrameBuffer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLRenderbuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glcontext::TGLRenderbufferHandle* FRenderbufferHandle;
	int FWidth;
	int FHeight;
	bool FStorageValid;
	unsigned __fastcall GetHandle();
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat() = 0 ;
	void __fastcall InvalidateStorage();
	
public:
	__fastcall TGLRenderbuffer();
	__fastcall virtual ~TGLRenderbuffer();
	void __fastcall Bind();
	void __fastcall Unbind();
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
};


class PASCALIMPLEMENTATION TGLDepthRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	Glscene::TGLDepthPrecision FDepthPrecision;
	void __fastcall SetDepthPrecision(const Glscene::TGLDepthPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat();
	
public:
	__fastcall TGLDepthRBO();
	__property Glscene::TGLDepthPrecision DepthPrecision = {read=FDepthPrecision, write=SetDepthPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLDepthRBO() { }
	
};


enum DECLSPEC_DENUM TGLStencilPrecision : unsigned char { spDefault, sp1bit, sp4bits, sp8bits, sp16bits };

class PASCALIMPLEMENTATION TGLStencilRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	TGLStencilPrecision FStencilPrecision;
	void __fastcall SetStencilPrecision(const TGLStencilPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat();
	
public:
	__fastcall TGLStencilRBO();
	__property TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLStencilRBO() { }
	
};


class PASCALIMPLEMENTATION TGLFrameBuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glcontext::TGLFramebufferHandle* FFrameBufferHandle;
	unsigned FTarget;
	int FWidth;
	int FHeight;
	int FLayer;
	int FLevel;
	unsigned FTextureMipmap;
	System::StaticArray<Gltexture::TGLTexture*, 32> FAttachedTexture;
	Gltexture::TGLTexture* FDepthTexture;
	TGLDepthRBO* FDRBO;
	TGLStencilRBO* FSRBO;
	Glcontext::TGLFramebufferStatus __fastcall GetStatus();
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetLayer(const int Value);
	void __fastcall SetLevel(const int Value);
	
protected:
	void __fastcall AttachTexture(const unsigned attachment, const unsigned textarget, const unsigned texture, const int level, const int layer)/* overload */;
	void __fastcall ReattachTextures();
	
public:
	__fastcall TGLFrameBuffer();
	__fastcall virtual ~TGLFrameBuffer();
	void __fastcall AttachDepthBuffer(TGLDepthRBO* DepthBuffer)/* overload */;
	void __fastcall DetachDepthBuffer();
	void __fastcall AttachStencilBuffer(TGLStencilRBO* StencilBuffer)/* overload */;
	void __fastcall DetachStencilBuffer();
	void __fastcall AttachDepthTexture(Gltexture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachDepthTexture();
	void __fastcall AttachTexture(unsigned n, Gltexture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachTexture(unsigned n);
	Glcontext::TGLFramebufferStatus __fastcall GetStringStatus(/* out */ System::UnicodeString &clarification);
	__property Glcontext::TGLFramebufferStatus Status = {read=GetStatus, nodefault};
	void __fastcall Bind();
	void __fastcall Unbind();
	void __fastcall PreRender();
	void __fastcall Render(Glrendercontextinfo::TGLRenderContextInfo &rci, Glscene::TGLBaseSceneObject* baseObject);
	void __fastcall PostRender(const bool PostGenerateMipmap);
	__property Glcontext::TGLFramebufferHandle* Handle = {read=FFrameBufferHandle};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int Layer = {read=FLayer, write=SetLayer, nodefault};
	__property int Level = {read=FLevel, write=SetLevel, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MaxColorAttachments = System::Int8(0x20);
}	/* namespace Glfbo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFBO)
using namespace Glfbo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfboHPP
