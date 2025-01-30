// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGraphics.pas' rev: 36.00 (Windows)

#ifndef GLGraphicsHPP
#define GLGraphicsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Consts.hpp>
#include <GR32.hpp>
#include <OpenGLTokens.hpp>
#include <GLState.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLPersistentClasses.hpp>
#include <GLContext.hpp>
#include <GLImageUtils.hpp>
#include <GLUtils.hpp>
#include <GLColor.hpp>
#include <GLTextureFormat.hpp>
#include <GLVectorGeometry.hpp>
#include <GLStrings.hpp>
#include <GLSLog.hpp>
#include <GLBaseClasses.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgraphics
{
//-- forward type declarations -----------------------------------------------
struct TPixel24;
struct TPixel32;
struct TGLImageLevelDesc;
class DELPHICLASS TGLBaseImage;
class DELPHICLASS TGLImage;
class DELPHICLASS TRasterFileFormat;
class DELPHICLASS TRasterFileFormatsList;
class DELPHICLASS EInvalidRasterFile;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TPixel24
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
};
#pragma pack(pop)


typedef TPixel24 *PPixel24;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TPixel32
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
	System::Byte a;
};
#pragma pack(pop)


typedef TPixel32 *PPixel32;

typedef System::StaticArray<TPixel32, 268435456> TPixel32Array;

typedef TPixel32Array *PPixel32Array;

enum DECLSPEC_DENUM TGLLODStreamingState : unsigned char { ssKeeping, ssLoading, ssLoaded, ssTransfered };

struct DECLSPEC_DRECORD TGLImageLevelDesc
{
public:
	int Width;
	int Height;
	int Depth;
	Glcontext::TGLUnpackPBOHandle* PBO;
	void *MapAddress;
	System::LongWord Offset;
	System::LongWord StreamOffset;
	System::LongWord Size;
	TGLLODStreamingState State;
};


typedef System::Int8 TGLImageLODRange;

typedef System::StaticArray<TGLImageLevelDesc, 16> TGLImagePiramid;

class PASCALIMPLEMENTATION TGLBaseImage : public Glapplicationfileio::TGLDataFile
{
	typedef Glapplicationfileio::TGLDataFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	TGLImageLODRange FStreamLevel;
	Glcontext::TFinishTaskEvent* FFinishEvent;
	
protected:
	PPixel32Array fData;
	TGLImagePiramid FLOD;
	TGLImageLODRange fLevelCount;
	unsigned fColorFormat;
	Gltextureformat::TGLInternalFormat fInternalFormat;
	unsigned fDataType;
	int fElementSize;
	bool fCubeMap;
	bool fTextureArray;
	virtual PPixel32Array __fastcall GetData();
	int __fastcall GetWidth();
	int __fastcall GetHeight();
	int __fastcall GetDepth();
	void * __fastcall GetLevelAddress(System::Byte ALevel)/* overload */;
	void * __fastcall GetLevelAddress(System::Byte ALevel, System::Byte AFace)/* overload */;
	int __fastcall GetLevelWidth(TGLImageLODRange ALOD);
	int __fastcall GetLevelHeight(TGLImageLODRange ALOD);
	int __fastcall GetLevelDepth(TGLImageLODRange ALOD);
	Glcontext::TGLUnpackPBOHandle* __fastcall GetLevelPBO(TGLImageLODRange ALOD);
	int __fastcall GetLevelOffset(TGLImageLODRange ALOD);
	int __fastcall GetLevelSizeInByte(TGLImageLODRange ALOD);
	TGLLODStreamingState __fastcall GetLevelStreamingState(TGLImageLODRange ALOD);
	void __fastcall SetLevelStreamingState(TGLImageLODRange ALOD, TGLLODStreamingState AState);
	void __fastcall SaveHeader();
	void __fastcall LoadHeader();
	void __fastcall StartStreaming();
	void __fastcall DoStreaming();
	
public:
	__fastcall virtual TGLBaseImage();
	__fastcall virtual ~TGLBaseImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget();
	virtual void __fastcall RegisterAsOpenGLTexture(Glcontext::TGLTextureHandle* AHandle, bool aMipmapGen, unsigned aTexFormat, /* out */ int &texWidth, /* out */ int &texHeight, /* out */ int &texDepth);
	virtual bool __fastcall AssignFromTexture(Glcontext::TGLTextureHandle* AHandle, const bool CastToFormat, const Gltextureformat::TGLInternalFormat intFormat = (Gltextureformat::TGLInternalFormat)(0x1f), const unsigned colorFormat = (unsigned)(0x0), const unsigned dataType = (unsigned)(0x0));
	bool __fastcall ConvertCrossToCubeMap();
	bool __fastcall ConvertToVolume(const int col, const int row, const bool MakeArray);
	unsigned __fastcall DataSize();
	bool __fastcall IsEmpty();
	bool __fastcall IsCompressed();
	bool __fastcall IsVolume();
	void __fastcall Narrow();
	virtual void __fastcall GenerateMipmap(Glimageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap();
	__property PPixel32Array Data = {read=GetData};
	void __fastcall SetErrorImage();
	void __fastcall UpdateLevelsInfo();
	__property int LevelWidth[TGLImageLODRange ALOD] = {read=GetLevelWidth};
	__property int LevelHeight[TGLImageLODRange ALOD] = {read=GetLevelHeight};
	__property int LevelDepth[TGLImageLODRange ALOD] = {read=GetLevelDepth};
	__property Glcontext::TGLUnpackPBOHandle* LevelPixelBuffer[TGLImageLODRange ALOD] = {read=GetLevelPBO};
	__property int LevelOffset[TGLImageLODRange ALOD] = {read=GetLevelOffset};
	__property int LevelSizeInByte[TGLImageLODRange ALOD] = {read=GetLevelSizeInByte};
	__property TGLLODStreamingState LevelStreamingState[TGLImageLODRange ALOD] = {read=GetLevelStreamingState, write=SetLevelStreamingState};
	__property TGLImageLODRange LevelCount = {read=fLevelCount, nodefault};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, nodefault};
};


typedef System::TMetaClass* TGLBaseImageClass;

class PASCALIMPLEMENTATION TGLImage : public TGLBaseImage
{
	typedef TGLBaseImage inherited;
	
private:
	bool FVerticalReverseOnAssignFromBitmap;
	bool FBlank;
	unsigned fOldColorFormat;
	unsigned fOldDataType;
	void __fastcall DataConvertTask();
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetDepth(const int val);
	void __fastcall SetBlank(const bool Value);
	void __fastcall SetCubeMap(const bool val);
	void __fastcall SetArray(const bool val);
	PPixel32Array __fastcall GetScanLine(int index);
	void __fastcall AssignFrom24BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFrom32BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromBitmap32(Gr32::TBitmap32* aBitmap32);
	void __fastcall AssignFromPngImage(Vcl::Imaging::Pngimage::TPngImage* aPngImage);
	
public:
	__fastcall virtual TGLImage();
	__fastcall virtual ~TGLImage();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AssignFromBitmap24WithoutRGBSwap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromTexture2D(unsigned textureHandle)/* overload */;
	void __fastcall AssignFromTexture2D(Glcontext::TGLTextureHandle* textureHandle)/* overload */;
	Vcl::Graphics::TBitmap* __fastcall Create32BitsBitmap();
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, write=fInternalFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, write=SetCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, write=SetArray, nodefault};
	__property PPixel32Array ScanLine[int index] = {read=GetScanLine};
	__property bool VerticalReverseOnAssignFromBitmap = {read=FVerticalReverseOnAssignFromBitmap, write=FVerticalReverseOnAssignFromBitmap, nodefault};
	__property bool Blank = {read=FBlank, write=SetBlank, nodefault};
	void __fastcall SetColorFormatDataType(const unsigned AColorFormat, const unsigned ADataType);
	void __fastcall SetAlphaFromIntensity();
	void __fastcall SetAlphaTransparentForColor(const System::Uitypes::TColor aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TPixel32 aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TPixel24 aColor)/* overload */;
	void __fastcall SetAlphaToValue(const System::Byte aValue);
	void __fastcall SetAlphaToFloatValue(const float aValue);
	void __fastcall InvertAlpha();
	void __fastcall SqrtAlpha();
	void __fastcall BrightnessCorrection(const float factor);
	void __fastcall GammaCorrection(const float gamma);
	void __fastcall DownSampleByFactor2();
	void __fastcall ReadPixels(const Gr32::TRect &area);
	void __fastcall DrawPixels(const float x, const float y);
	void __fastcall GrayScaleToNormalMap(const float scale, bool wrapX = true, bool wrapY = true);
	void __fastcall NormalizeNormalMap();
	void __fastcall AssignToBitmap(Vcl::Graphics::TBitmap* aBitmap);
	virtual void __fastcall GenerateMipmap(Glimageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap();
};


typedef TGLImage TGLBitmap32;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TRasterFileFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TGLBaseImageClass BaseImageClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
public:
	/* TObject.Create */ inline __fastcall TRasterFileFormat() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRasterFileFormat() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TRasterFileFormatsList : public Glpersistentclasses::TPersistentObjectList
{
	typedef Glpersistentclasses::TPersistentObjectList inherited;
	
public:
	__fastcall virtual ~TRasterFileFormatsList();
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TGLBaseImageClass AClass);
	TGLBaseImageClass __fastcall FindExt(System::UnicodeString ext);
	TGLBaseImageClass __fastcall FindFromFileName(const System::UnicodeString fileName);
	TGLBaseImageClass __fastcall FindFromStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall Remove(TGLBaseImageClass AClass);
	void __fastcall BuildFilterStrings(TGLBaseImageClass imageFileClass, System::UnicodeString &descriptions, System::UnicodeString &filters, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
	System::UnicodeString __fastcall FindExtByIndex(int index, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TRasterFileFormatsList() : Glpersistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TRasterFileFormatsList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidRasterFile : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidRasterFile() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vVerticalFlipDDS;
extern DELPHI_PACKAGE TRasterFileFormatsList* __fastcall GetRasterFileFormats(void);
extern DELPHI_PACKAGE void __fastcall RegisterRasterFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TGLBaseImageClass AClass);
extern DELPHI_PACKAGE void __fastcall UnregisterRasterFormat(TGLBaseImageClass AClass);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RasterFileFormatExtensionByIndex(int index);
extern DELPHI_PACKAGE void __fastcall Div2(int &Value);
extern DELPHI_PACKAGE int __fastcall GetImageLodNumber(int w, int h, int d, bool IsVolume);
extern DELPHI_PACKAGE void __fastcall GammaCorrectRGBArray(void * base, int pixelCount, float gamma);
extern DELPHI_PACKAGE void __fastcall BrightenRGBArray(void * base, int pixelCount, float factor);
extern DELPHI_PACKAGE void __fastcall BGR24ToRGB24(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall BGR24ToRGBA32(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall RGB24ToRGBA32(void * src, void * dest, int pixelCount);
extern DELPHI_PACKAGE void __fastcall BGRA32ToRGBA32(void * src, void * dest, int pixelCount);
}	/* namespace Glgraphics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGRAPHICS)
using namespace Glgraphics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLGraphicsHPP
