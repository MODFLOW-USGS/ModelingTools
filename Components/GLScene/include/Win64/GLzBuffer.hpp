// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLzBuffer.pas' rev: 35.00 (Windows)

#ifndef GlzbufferHPP
#define GlzbufferHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <XOpenGL.hpp>
#include <GLScene.hpp>
#include <GLVectorGeometry.hpp>
#include <GLGraphics.hpp>
#include <GLObjects.hpp>
#include <GLContext.hpp>
#include <GLWin32Viewer.hpp>
#include <GLColor.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLState.hpp>
#include <GLTextureFormat.hpp>
#include <GLVectorTypes.hpp>
#include <GLCoordinates.hpp>
#include <GLPersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glzbuffer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EZBufferException;
class DELPHICLASS TGLzBuffer;
class DELPHICLASS TGLZShadows;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EZBufferException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EZBufferException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EZBufferException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZBufferException() { }
	
};


typedef System::StaticArray<float, 268435456> TZArray;

typedef TZArray *PZArray;

typedef System::DynamicArray<PZArray> TZArrayIdx;

typedef System::StaticArray<System::Byte, 268435456> TAArray;

typedef TAArray *PAArray;

typedef System::DynamicArray<PAArray> TAArrayIdx;

enum DECLSPEC_DENUM TOptimise : unsigned char { opNone, op4in1, op9in1, op16in1 };

class PASCALIMPLEMENTATION TGLzBuffer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TZArray *FData;
	TZArrayIdx FDataIdx;
	TZArrayIdx FDataInvIdx;
	int FWidth;
	int FHeight;
	int FDataSize;
	float ang1;
	float ang2;
	float scal;
	float c1;
	float s1;
	float c2;
	float s2;
	float vw;
	float vh;
	Glvectortypes::TVector3f lt;
	Glvectortypes::TVector3f rt;
	Glvectortypes::TVector3f lb;
	Glvectortypes::TVector3f rb;
	Glvectortypes::TVector3f UpVec;
	Glvectortypes::TVector3f riVec;
	Glvectortypes::TVector3f ltW;
	Glvectortypes::TVector3f rtW;
	Glvectortypes::TVector3f lbW;
	Glvectortypes::TVector3f rbW;
	Glvectortypes::TVector3f UpVecW;
	Glvectortypes::TVector3f riVecW;
	float OrthInvDov;
	float OrthAddX;
	float OrthMulX;
	float OrthAddY;
	float OrthMulY;
	float dov;
	float np;
	float fp;
	float NpFp;
	float OneMinNp_Fp;
	float invOneMinNp_Fp;
	Glscene::TGLCamera* cam;
	void __fastcall DoCalcVectors();
	
protected:
	void __fastcall PrepareBufferMemory();
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	
public:
	Glwin32viewer::TGLSceneViewer* SceneViewer;
	Glscene::TGLMemoryViewer* MemoryViewer;
	Glscene::TGLSceneBuffer* Buffer;
	Glvectortypes::TVector3f Normal;
	__fastcall TGLzBuffer();
	__fastcall virtual ~TGLzBuffer();
	void __fastcall LinkToViewer(Glwin32viewer::TGLSceneViewer* viewer)/* overload */;
	void __fastcall LinkToViewer(Glscene::TGLMemoryViewer* viewer)/* overload */;
	PZArray __fastcall GetDepthBuffer(bool CalcVectors, bool ContextIsActive);
	float __fastcall GetPixelzDepth(int x, int y);
	float __fastcall PixelToDistance_OLD(int x, int y);
	float __fastcall PixelToDistance(int x, int y);
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property PZArray Data = {read=FData};
	__property TZArrayIdx DataIdx = {read=FDataIdx};
	__property TZArrayIdx DataInvIdx = {read=FDataIdx};
	void __fastcall Refresh();
	Glvectortypes::TVector3f __fastcall FastScreenToVector(int x, int y);
	Glvectortypes::TVector3f __fastcall FastVectorToScreen(const Glvectortypes::TVector3f &vec);
	Glvectortypes::TVector3f __fastcall PixelToWorld(const int x, const int y);
	bool __fastcall WorldToPixel(const Glvectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ);
	bool __fastcall WorldToPixelZ(const Glvectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall WorldToPixelZ(const Glvectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall OrthWorldToPixelZ(const Glvectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ);
};


class PASCALIMPLEMENTATION TGLZShadows : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glwin32viewer::TGLSceneViewer* FViewer;
	Glscene::TGLMemoryViewer* FCaster;
	bool FDepthFade;
	bool FFrustShadow;
	bool FSkyShadow;
	TOptimise FOptimise;
	TAArray *FData;
	TAArrayIdx FDataIdx;
	TAArrayIdx FDataInvIdx;
	int FDataSize;
	int FWidth;
	int FHeight;
	int FXRes;
	int FYRes;
	bool Fsoft;
	float FTolerance;
	Glcolor::TGLColor* FColor;
	Glgraphics::TPixel32 SCol;
	bool FTexturePrepared;
	Glcontext::TGLTextureHandle* FTexHandle;
	
protected:
	void __fastcall PrepareAlphaMemory();
	Glwin32viewer::TGLSceneViewer* __fastcall GetViewer();
	void __fastcall SetViewer(Glwin32viewer::TGLSceneViewer* const val);
	Glscene::TGLMemoryViewer* __fastcall GetCaster();
	void __fastcall SetCaster(Glscene::TGLMemoryViewer* const val);
	void __fastcall CalcShadowTexture(Glrendercontextinfo::TGLRenderContextInfo &rci);
	System::Byte __fastcall HardSet(const int x, const int y);
	System::Byte __fastcall SoftTest(const int x, const int y);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetXRes(const int val);
	void __fastcall SetYRes(const int val);
	void __fastcall SetSoft(const bool val);
	void __fastcall BindTexture();
	
public:
	TGLzBuffer* ViewerZBuf;
	TGLzBuffer* CasterZBuf;
	__fastcall virtual TGLZShadows(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLZShadows();
	virtual void __fastcall DoRender(Glrendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property Glwin32viewer::TGLSceneViewer* Viewer = {read=GetViewer, write=SetViewer};
	__property Glscene::TGLMemoryViewer* Caster = {read=GetCaster, write=SetCaster};
	__property bool FrustShadow = {read=FFrustShadow, write=FFrustShadow, nodefault};
	__property bool SkyShadow = {read=FSkyShadow, write=FSkyShadow, nodefault};
	__property TOptimise Optimise = {read=FOptimise, write=FOptimise, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property Glcolor::TGLColor* Color = {read=FColor, write=FColor};
	__property bool Soft = {read=Fsoft, write=SetSoft, nodefault};
	__property float Tolerance = {read=FTolerance, write=FTolerance};
	__property ObjectsSorting = {default=0};
	__property Visible = {default=1};
	__property bool DepthFade = {read=FDepthFade, write=FDepthFade, nodefault};
	bool __fastcall CastShadow();
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLZShadows(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glzbuffer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLZBUFFER)
using namespace Glzbuffer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlzbufferHPP
