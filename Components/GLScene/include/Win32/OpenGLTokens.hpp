// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OpenGLTokens.pas' rev: 36.00 (Windows)

#ifndef OpengltokensHPP
#define OpengltokensHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <GLVectorTypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Opengltokens
{
//-- forward type declarations -----------------------------------------------
struct DECLSPEC_DRECORD _cl_context
{
};


struct DECLSPEC_DRECORD _cl_event
{
};


struct _WGLSWAP;
struct TGPUDEVICE;
struct DECLSPEC_DRECORD TGLUNurbs
{
};


struct DECLSPEC_DRECORD TGLUQuadric
{
};


struct DECLSPEC_DRECORD TGLUTesselator
{
};


//-- type declarations -------------------------------------------------------
typedef System::ByteBool TGLboolean;

typedef System::ByteBool *PGLboolean;

typedef unsigned TGLbitfield;

typedef unsigned *PGLbitfield;

typedef System::Int8 TGLbyte;

typedef System::Int8 *PGLbyte;

typedef short TGLshort;

typedef short *PGLshort;

typedef int TGLint;

typedef System::PInteger PGLint;

typedef int TGLsizei;

typedef System::PInteger PGLsizei;

typedef __int64 TGLint64;

typedef System::PInt64 PGLint64;

typedef __int64 TGLint64EXT;

typedef System::PInt64 PGLint64EXT;

typedef unsigned __int64 TGLuint64;

typedef System::PUInt64 PGLuint64;

typedef unsigned __int64 TGLuint64EXT;

typedef System::PUInt64 PGLuint64EXT;

typedef System::Byte TGLubyte;

typedef System::PByte PGLubyte;

typedef System::Word TGLushort;

typedef System::PWord PGLushort;

typedef unsigned TGLuint;

typedef System::PCardinal PGLuint;

typedef float TGLfloat;

typedef System::PSingle PGLfloat;

typedef System::PSingle PGLclampf;

typedef double TGLdouble;

typedef System::PDouble PGLdouble;

typedef double TGLclampd;

typedef System::PDouble PGLclampd;

typedef unsigned GLhandleARB;

typedef unsigned *PGLhandleARB;

typedef char * *PGLPCharArray;

typedef void * PGLvoid;

typedef void * *PGLPointer;

typedef _cl_context *p_cl_context;

typedef _cl_event *p_cl_event;

typedef System::NativeInt GLintptr;

typedef System::NativeInt TGLintptr;

typedef System::NativeInt GLsizeiptr;

typedef System::NativeInt TGLsizeiptr;

typedef System::NativeInt GLsync;

typedef System::NativeInt TGLsync;

typedef _WGLSWAP *PWGLswap;

#pragma pack(push,1)
struct DECLSPEC_DRECORD _WGLSWAP
{
public:
	HDC hdc;
	unsigned uiFlags;
};
#pragma pack(pop)


typedef _WGLSWAP TWGLswap;

typedef _WGLSWAP WGLSWAP;

typedef int HPBUFFERARB;

typedef unsigned *PHGPUNV;

typedef Winapi::Windows::THandle HGPUNV;

typedef Winapi::Windows::THandle HVIDEOINPUTDEVICENV;

typedef unsigned *PHVIDEOINPUTDEVICENV;

typedef TGPUDEVICE *PGPUDEVICE;

struct DECLSPEC_DRECORD TGPUDEVICE
{
public:
	unsigned cb;
	System::StaticArray<char, 32> DeviceName;
	System::StaticArray<char, 128> DeviceString;
	unsigned Flags;
	Winapi::Windows::TRect rcVirtualScreen;
};


typedef void __stdcall (*TDebugProc)(unsigned source, unsigned type_, unsigned id, unsigned severity, TGLsizei length, const char * message, void * userParam);

typedef TDebugProc TGLDEBUGPROCARB;

typedef void __stdcall (*TDebugProcAMD)(unsigned id, unsigned category, unsigned severity, TGLsizei length, char * message, void * userParam);

typedef GLintptr TGLvdpauSurfaceNV;

typedef int *PGLvdpauSurfaceNV;

typedef TGLUNurbs *PGLUNurbs;

typedef TGLUQuadric *PGLUQuadric;

typedef TGLUTesselator *PGLUTesselator;

typedef TGLUNurbs TGLUNurbsObj;

typedef TGLUQuadric TGLUQuadricObj;

typedef TGLUTesselator TGLUTesselatorObj;

typedef TGLUTesselator TGLUTriangulatorObj;

typedef PGLUNurbs PGLUNurbsObj;

typedef PGLUQuadric PGLUQuadricObj;

typedef PGLUTesselator PGLUTesselatorObj;

typedef PGLUTesselator PGLUTriangulatorObj;

typedef void __stdcall (*TGLUQuadricErrorProc)(unsigned errorCode);

typedef void __stdcall (*TGLUTessBeginProc)(unsigned AType);

typedef void __stdcall (*TGLUTessEdgeFlagProc)(TGLboolean Flag);

typedef void __stdcall (*TGLUTessVertexProc)(void * VertexData);

typedef void __stdcall (*TGLUTessEndProc)(void);

typedef void __stdcall (*TGLUTessErrorProc)(unsigned ErrNo);

typedef void __stdcall (*TGLUTessCombineProc)(const Glvectortypes::TVector3d &Coords, const Glvectortypes::TVector4p &VertexData, const Glvectortypes::TVector4f &Weight, PGLPointer OutData);

typedef void __stdcall (*TGLUTessBeginDataProc)(unsigned AType, void * UserData);

typedef void __stdcall (*TGLUTessEdgeFlagDataProc)(TGLboolean Flag, void * UserData);

typedef void __stdcall (*TGLUTessVertexDataProc)(void * VertexData, void * UserData);

typedef void __stdcall (*TGLUTessEndDataProc)(void * UserData);

typedef void __stdcall (*TGLUTessErrorDataProc)(unsigned ErrNo, void * UserData);

typedef void __stdcall (*TGLUTessCombineDataProc)(const Glvectortypes::TVector3d &Coords, const Glvectortypes::TVector4p &VertexData, const Glvectortypes::TVector4f &Weight, PGLPointer OutData, void * UserData);

typedef void __stdcall (*TGLUNurbsErrorProc)(unsigned ErrorCode);

typedef void __stdcall (*PFNGLBLENDCOLORPROC)(float red, float green, float blue, float alpha);

typedef void __stdcall (*PFNGLBLENDEQUATIONPROC)(unsigned mode);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSPROC)(unsigned mode, unsigned Astart, unsigned Aend, TGLsizei count, unsigned Atype, void * indices);

typedef void __stdcall (*PFNGLTEXIMAGE3DPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE3DPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE3DPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLCOLORTABLEPROC)(unsigned target, unsigned internalformat, TGLsizei width, unsigned format, unsigned Atype, void * table);

typedef void __stdcall (*PFNGLCOLORTABLEPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLCOLORTABLEPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLCOPYCOLORTABLEPROC)(unsigned target, unsigned internalformat, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLGETCOLORTABLEPROC)(unsigned target, unsigned format, unsigned Atype, void * table);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLCOLORSUBTABLEPROC)(unsigned target, TGLsizei start, TGLsizei count, unsigned format, unsigned Atype, void * data);

typedef void __stdcall (*PFNGLCOPYCOLORSUBTABLEPROC)(unsigned target, TGLsizei start, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLCONVOLUTIONFILTER1DPROC)(unsigned target, unsigned internalformat, TGLsizei width, unsigned format, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLCONVOLUTIONFILTER2DPROC)(unsigned target, unsigned internalformat, TGLsizei width, TGLsizei height, unsigned format, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERFPROC)(unsigned target, unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERIPROC)(unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLCOPYCONVOLUTIONFILTER1DPROC)(unsigned target, unsigned internalformat, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLCOPYCONVOLUTIONFILTER2DPROC)(unsigned target, unsigned internalformat, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETCONVOLUTIONFILTERPROC)(unsigned target, unsigned internalformat, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLGETCONVOLUTIONPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETCONVOLUTIONPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETSEPARABLEFILTERPROC)(unsigned target, unsigned format, unsigned Atype, void * row, void * column, void * span);

typedef void __stdcall (*PFNGLSEPARABLEFILTER2DPROC)(unsigned target, unsigned internalformat, TGLsizei width, TGLsizei height, unsigned format, unsigned Atype, void * row, void * column);

typedef void __stdcall (*PFNGLGETHISTOGRAMPROC)(unsigned target, TGLboolean reset, unsigned format, unsigned Atype, void * values);

typedef void __stdcall (*PFNGLGETHISTOGRAMPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETHISTOGRAMPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETMINMAXPROC)(unsigned target, TGLboolean reset, unsigned format, unsigned Atype, void * values);

typedef void __stdcall (*PFNGLGETMINMAXPARAMETERFVPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETMINMAXPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLHISTOGRAMPROC)(unsigned target, TGLsizei width, unsigned internalformat, TGLboolean sink);

typedef void __stdcall (*PFNGLMINMAXPROC)(unsigned target, unsigned internalformat, TGLboolean sink);

typedef void __stdcall (*PFNGLRESETHISTOGRAMPROC)(unsigned target);

typedef void __stdcall (*PFNGLRESETMINMAXPROC)(unsigned target);

typedef void __stdcall (*PFNGLACTIVETEXTUREPROC)(unsigned texture);

typedef void __stdcall (*PFNGLSAMPLECOVERAGEPROC)(float Value, TGLboolean invert);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE3DPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLsizei Height, TGLsizei depth, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE2DPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLsizei Height, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE1DPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC)(unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXIMAGEPROC)(unsigned target, TGLint level, void * img);

typedef void __stdcall (*PFNGLCLIENTACTIVETEXTUREPROC)(unsigned texture);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DPROC)(unsigned target, TGLdouble s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DVPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FPROC)(unsigned target, TGLfloat s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FVPROC)(unsigned target, TGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IPROC)(unsigned target, TGLint s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IVPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SPROC)(unsigned target, TGLshort s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SVPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DPROC)(unsigned target, TGLdouble s, TGLdouble t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DVPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FPROC)(unsigned target, TGLfloat s, TGLfloat t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FVPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IPROC)(unsigned target, TGLint s, TGLint t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IVPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SPROC)(unsigned target, TGLshort s, TGLshort t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SVPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DPROC)(unsigned target, TGLdouble s, TGLdouble t, TGLdouble r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DVPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FPROC)(unsigned target, TGLfloat s, TGLfloat t, TGLfloat r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FVPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IPROC)(unsigned target, TGLint s, TGLint t, TGLint r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IVPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SPROC)(unsigned target, TGLshort s, TGLshort t, TGLshort r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SVPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DPROC)(unsigned target, TGLdouble s, TGLdouble t, TGLdouble r, TGLdouble q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DVPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FPROC)(unsigned target, TGLfloat s, TGLfloat t, TGLfloat r, TGLfloat q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FVPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IPROC)(unsigned target, TGLint s, TGLint t, TGLint r, TGLint q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IVPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SPROC)(unsigned target, TGLshort s, TGLshort t, TGLshort r, TGLshort q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SVPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXFPROC)(PGLfloat m);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXDPROC)(PGLdouble m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXFPROC)(PGLfloat m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXDPROC)(PGLdouble m);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEPROC)(unsigned sfactorRGB, unsigned dfactorRGB, unsigned sfactorAlpha, unsigned dfactorAlpha);

typedef void __stdcall (*PFNGLMULTIDRAWARRAYSPROC)(unsigned mode, PGLint First, PGLsizei Count, TGLsizei primcount);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSPROC)(unsigned mode, PGLsizei Count, unsigned AType, void *indices, TGLsizei primcount);

typedef void __stdcall (*PFNGLPOINTPARAMETERFPROC)(unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLPOINTPARAMETERFVPROC)(unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLPOINTPARAMETERIPROC)(unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLPOINTPARAMETERIVPROC)(unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLFOGCOORDFPROC)(TGLfloat coord);

typedef void __stdcall (*PFNGLFOGCOORDFVPROC)(PGLfloat coord);

typedef void __stdcall (*PFNGLFOGCOORDDPROC)(TGLdouble coord);

typedef void __stdcall (*PFNGLFOGCOORDDVPROC)(PGLdouble coord);

typedef void __stdcall (*PFNGLFOGCOORDPOINTERPROC)(unsigned AType, TGLsizei stride, void * p);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BPROC)(TGLbyte red, TGLbyte green, TGLbyte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BVPROC)(PGLbyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DPROC)(TGLdouble red, TGLdouble green, TGLdouble blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DVPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FPROC)(TGLfloat red, TGLfloat green, TGLfloat blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FVPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IPROC)(TGLint red, TGLint green, TGLint blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IVPROC)(PGLint v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SPROC)(TGLshort red, TGLshort green, TGLshort blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SVPROC)(PGLshort v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBPROC)(TGLubyte red, TGLubyte green, TGLubyte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBVPROC)(PGLubyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIPROC)(unsigned red, unsigned green, unsigned blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIVPROC)(PGLuint v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USPROC)(TGLushort red, TGLushort green, TGLushort blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USVPROC)(PGLushort v);

typedef void __stdcall (*PFNGLSECONDARYCOLORPOINTERPROC)(TGLint Size, unsigned Atype, TGLsizei stride, void * p);

typedef void __stdcall (*PFNGLWINDOWPOS2DPROC)(TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLWINDOWPOS2DVPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLWINDOWPOS2FPROC)(TGLfloat x, TGLfloat y);

typedef void __stdcall (*PFNGLWINDOWPOS2FVPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLWINDOWPOS2IPROC)(TGLint x, TGLint y);

typedef void __stdcall (*PFNGLWINDOWPOS2IVPROC)(PGLint v);

typedef void __stdcall (*PFNGLWINDOWPOS2SPROC)(TGLshort x, TGLshort y);

typedef void __stdcall (*PFNGLWINDOWPOS2SVPROC)(PGLshort v);

typedef void __stdcall (*PFNGLWINDOWPOS3DPROC)(TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLWINDOWPOS3DVPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLWINDOWPOS3FPROC)(TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLWINDOWPOS3FVPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLWINDOWPOS3IPROC)(TGLint x, TGLint y, TGLint z);

typedef void __stdcall (*PFNGLWINDOWPOS3IVPROC)(PGLint v);

typedef void __stdcall (*PFNGLWINDOWPOS3SPROC)(TGLshort x, TGLshort y, TGLshort z);

typedef void __stdcall (*PFNGLWINDOWPOS3SVPROC)(PGLshort v);

typedef void __stdcall (*PFNGLGENQUERIESPROC)(TGLsizei n, PGLuint ids);

typedef void __stdcall (*PFNGLDELETEQUERIESPROC)(TGLsizei n, const PGLuint ids);

typedef TGLboolean __stdcall (*PFNGLISQUERYPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINQUERYPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETQUERYIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTIVPROC)(unsigned id, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUIVPROC)(unsigned id, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLBINDBUFFERPROC)(unsigned target, unsigned buffer);

typedef void __stdcall (*PFNGLDELETEBUFFERSPROC)(TGLsizei n, const PGLuint buffers);

typedef void __stdcall (*PFNGLGENBUFFERSPROC)(TGLsizei n, PGLuint buffers);

typedef TGLboolean __stdcall (*PFNGLISBUFFERPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLBUFFERDATAPROC)(unsigned target, TGLsizei size, const void * data, unsigned usage);

typedef void __stdcall (*PFNGLBUFFERSUBDATAPROC)(unsigned target, unsigned offset, TGLsizei size, const void * data);

typedef void __stdcall (*PFNGLGETBUFFERSUBDATAPROC)(unsigned target, unsigned offset, TGLsizei size, void * data);

typedef void * __stdcall (*PFNGLMAPBUFFERPROC)(unsigned target, unsigned access);

typedef TGLboolean __stdcall (*PFNGLUNMAPBUFFERPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETBUFFERPOINTERVPROC)(unsigned target, unsigned pname, void * params);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEPROC)(unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLDRAWBUFFERSPROC)(TGLsizei n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLSTENCILOPSEPARATEPROC)(unsigned face, unsigned sfail, unsigned dpfail, unsigned dppass);

typedef void __stdcall (*PFNGLSTENCILFUNCSEPARATEPROC)(unsigned face, unsigned func, TGLint ref, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILMASKSEPARATEPROC)(unsigned face, unsigned mask);

typedef void __stdcall (*PFNGLATTACHSHADERPROC)(unsigned _program, unsigned shader);

typedef void __stdcall (*PFNGLBINDATTRIBLOCATIONPROC)(unsigned _program, unsigned index, const char * name);

typedef void __stdcall (*PFNGLCOMPILESHADERPROC)(unsigned shader);

typedef unsigned __stdcall (*PFNGLCREATEPROGRAMPROC)(void);

typedef unsigned __stdcall (*PFNGLCREATESHADERPROC)(unsigned _type);

typedef void __stdcall (*PFNGLDELETEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLDELETESHADERPROC)(unsigned shader);

typedef void __stdcall (*PFNGLDETACHSHADERPROC)(unsigned _program, unsigned shader);

typedef void __stdcall (*PFNGLDISABLEVERTEXATTRIBARRAYPROC)(unsigned index);

typedef void __stdcall (*PFNGLENABLEVERTEXATTRIBARRAYPROC)(unsigned index);

typedef void __stdcall (*PFNGLGETACTIVEATTRIBPROC)(unsigned _program, unsigned index, TGLsizei bufSize, PGLsizei length, PGLint size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMPROC)(unsigned _program, unsigned index, TGLsizei bufSize, PGLsizei length, PGLint size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETATTACHEDSHADERSPROC)(unsigned _program, TGLsizei maxCount, PGLsizei count, PGLuint obj);

typedef TGLint __stdcall (*PFNGLGETATTRIBLOCATIONPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGETPROGRAMIVPROC)(unsigned _program, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETPROGRAMINFOLOGPROC)(unsigned _program, TGLsizei bufSize, PGLsizei length, char * infoLog);

typedef void __stdcall (*PFNGLGETSHADERIVPROC)(unsigned shader, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETSHADERINFOLOGPROC)(unsigned shader, TGLsizei bufSize, PGLsizei length, char * infoLog);

typedef void __stdcall (*PFNGLGETSHADERSOURCEPROC)(unsigned shader, TGLsizei bufSize, PGLsizei length, char * source);

typedef TGLint __stdcall (*PFNGLGETUNIFORMLOCATIONPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGETUNIFORMFVPROC)(unsigned _program, TGLint location, PGLfloat params);

typedef void __stdcall (*PFNGLGETUNIFORMIVPROC)(unsigned _program, TGLint location, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVPROC)(unsigned index, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVPROC)(unsigned index, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVPROC)(unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVPROC)(unsigned index, unsigned pname, void * _pointer);

typedef TGLboolean __stdcall (*PFNGLISPROGRAMPROC)(unsigned _program);

typedef TGLboolean __stdcall (*PFNGLISSHADERPROC)(unsigned shader);

typedef void __stdcall (*PFNGLLINKPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLSHADERSOURCEPROC)(unsigned shader, TGLsizei count, const PGLPCharArray _string, const PGLint length);

typedef void __stdcall (*PFNGLUSEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLUNIFORM1FPROC)(TGLint location, TGLfloat v0);

typedef void __stdcall (*PFNGLUNIFORM2FPROC)(TGLint location, TGLfloat v0, TGLfloat v1);

typedef void __stdcall (*PFNGLUNIFORM3FPROC)(TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2);

typedef void __stdcall (*PFNGLUNIFORM4FPROC)(TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2, TGLfloat v3);

typedef void __stdcall (*PFNGLUNIFORM1IPROC)(TGLint location, TGLint v0);

typedef void __stdcall (*PFNGLUNIFORM2IPROC)(TGLint location, TGLint v0, TGLint v1);

typedef void __stdcall (*PFNGLUNIFORM3IPROC)(TGLint location, TGLint v0, TGLint v1, TGLint v2);

typedef void __stdcall (*PFNGLUNIFORM4IPROC)(TGLint location, TGLint v0, TGLint v1, TGLint v2, TGLint v3);

typedef void __stdcall (*PFNGLUNIFORM1FVPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM2FVPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM3FVPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM4FVPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM1IVPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM2IVPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM3IVPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM4IVPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DPROC)(unsigned index, TGLdouble x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FPROC)(unsigned index, TGLfloat x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SPROC)(unsigned index, TGLshort x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DPROC)(unsigned index, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FPROC)(unsigned index, TGLfloat x, TGLfloat y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SPROC)(unsigned index, TGLshort x, TGLshort y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SPROC)(unsigned index, TGLshort x, TGLshort y, TGLshort z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NBVPROC)(unsigned index, PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NIVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NSVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBPROC)(unsigned index, TGLubyte x, TGLubyte y, TGLubyte z, TGLubyte w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBVPROC)(unsigned index, PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUSVPROC)(unsigned index, PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4BVPROC)(unsigned index, PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4IVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SPROC)(unsigned index, TGLshort x, TGLshort y, TGLshort z, TGLshort w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVPROC)(unsigned index, PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4USVPROC)(unsigned index, PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERPROC)(unsigned index, TGLint size, unsigned _type, TGLboolean normalized, TGLsizei stride, void * _pointer);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X3FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X2FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X4FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X2FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X4FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X3FVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IPROC)(unsigned index, TGLint x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IPROC)(unsigned index, TGLint x, TGLint y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IPROC)(unsigned index, TGLint x, TGLint y, TGLint z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IPROC)(unsigned index, TGLint x, TGLint y, TGLint z, TGLint w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIPROC)(unsigned index, unsigned x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIPROC)(unsigned index, unsigned x, unsigned y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIPROC)(unsigned index, unsigned x, unsigned y, unsigned z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIPROC)(unsigned index, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIVPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4BVPROC)(unsigned index, PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4SVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UBVPROC)(unsigned index, PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4USVPROC)(unsigned index, PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBIPOINTERPROC)(unsigned index, TGLint size, unsigned _type, TGLsizei stride, void * _pointer);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIIVPROC)(unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIUIVPROC)(unsigned index, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLUNIFORM1UIPROC)(TGLint location, unsigned v0);

typedef void __stdcall (*PFNGLUNIFORM2UIPROC)(TGLint location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLUNIFORM3UIPROC)(TGLint location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLUNIFORM4UIPROC)(TGLint location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLUNIFORM1UIVPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM2UIVPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM3UIVPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM4UIVPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLGETUNIFORMUIVPROC)(unsigned _program, TGLint location, PGLuint params);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONPROC)(unsigned _program, unsigned colorNumber, char * name);

typedef TGLint __stdcall (*PFNGLGETFRAGDATALOCATIONPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLBEGINCONDITIONALRENDERPROC)(unsigned id, unsigned mode);

typedef void __stdcall (*PFNGLENDCONDITIONALRENDERPROC)(void);

typedef void __stdcall (*PFNGLCLAMPCOLORPROC)(unsigned target, unsigned clamp);

typedef void __stdcall (*PFNGLTEXPARAMETERIIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLTEXPARAMETERIUIVPROC)(unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIUIVPROC)(unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLCOLORMASKIPROC)(unsigned index, TGLboolean r, TGLboolean g, TGLboolean b, TGLboolean a);

typedef void __stdcall (*PFNGLGETBOOLEANI_VPROC)(unsigned target, unsigned index, PGLboolean data);

typedef void __stdcall (*PFNGLGETINTEGERI_VPROC)(unsigned target, unsigned index, PGLint data);

typedef void __stdcall (*PFNGLENABLEIPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLDISABLEIPROC)(unsigned target, unsigned index);

typedef TGLboolean __stdcall (*PFNGLISENABLEDIPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLBINDBUFFERRANGEPROC)(unsigned target, unsigned index, unsigned buffer, TGLintptr offset, TGLsizeiptr size);

typedef void __stdcall (*PFNGLBINDBUFFERBASEPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSPROC)(unsigned _program, TGLsizei count, const PGLPCharArray varyings, unsigned bufferMode);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGPROC)(unsigned _program, unsigned index, TGLsizei bufSize, PGLsizei length, PGLsizei size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLCLEARBUFFERIVPROC)(unsigned buffer, TGLint drawbuffer, PGLint value);

typedef void __stdcall (*PFNGLCLEARBUFFERUIVPROC)(unsigned buffer, TGLint drawbuffer, PGLuint value);

typedef void __stdcall (*PFNGLCLEARBUFFERFVPROC)(unsigned buffer, TGLint drawbuffer, PGLfloat value);

typedef void __stdcall (*PFNGLCLEARBUFFERFIPROC)(unsigned buffer, TGLint drawbuffer, TGLfloat depth, TGLint stencil);

typedef char * __stdcall (*PFNGLGETSTRINGIPROC)(unsigned name, unsigned index);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDPROC)(unsigned mode, TGLint first, TGLsizei count, TGLsizei primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDPROC)(unsigned mode, TGLsizei count, unsigned _type, PGLvoid indices, TGLsizei primcount);

typedef void __stdcall (*PFNGLTEXBUFFERPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTINDEXPROC)(unsigned index);

typedef void __stdcall (*PFNGLGETINTEGER64I_VPROC)(unsigned target, unsigned index, PGLint64 data);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERI64VPROC)(unsigned target, unsigned pname, PGLint64 params);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLVERTEXATTRIBDIVISORPROC)(unsigned index, unsigned divisor);

typedef void __stdcall (*PFNGLBLENDEQUATIONIPROC)(unsigned buf, unsigned mode);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEIPROC)(unsigned buf, unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLBLENDFUNCIPROC)(unsigned buf, unsigned src, unsigned dst);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEIPROC)(unsigned buf, unsigned srcRGB, unsigned dstRGB, unsigned srcAlpha, unsigned dstAlpha);

typedef void __stdcall (*PFNGLMINSAMPLESHADINGPROC)(float value);

typedef void __stdcall (*PFNGLUNURBSCALLBACKDATAEXTPROC)(PGLUNurbs nurb, void * userData);

typedef PGLUNurbs __stdcall (*PFNGLUNEWNURBSTESSELLATOREXTPROC)(void);

typedef void __stdcall (*PFNGLUDELETENURBSTESSELLATOREXTPROC)(PGLUNurbs nurb);

typedef int __stdcall (*PFNWGLCREATEBUFFERREGIONARBPROC)(HDC DC, int iLayerPlane, unsigned uType);

typedef void __stdcall (*PFNWGLDELETEBUFFERREGIONARBPROC)(int hRegion);

typedef System::LongBool __stdcall (*PFNWGLSAVEBUFFERREGIONARBPROC)(int hRegion, int x, int y, int width, int height);

typedef System::LongBool __stdcall (*PFNWGLRESTOREBUFFERREGIONARBPROC)(int hRegion, int x, int y, int width, int height, int xSrc, int ySrc);

typedef char * __stdcall (*PFNWGLGETEXTENSIONSSTRINGARBPROC)(HDC DC);

typedef System::LongBool __stdcall (*PFNWGLGETPIXELFORMATATTRIBIVARBPROC)(HDC DC, int iPixelFormat, int iLayerPlane, unsigned nAttributes, const PGLint piAttributes, PGLint piValues);

typedef System::LongBool __stdcall (*PFNWGLGETPIXELFORMATATTRIBFVARBPROC)(HDC DC, int iPixelFormat, int iLayerPlane, unsigned nAttributes, const PGLint piAttributes, PGLfloat piValues);

typedef System::LongBool __stdcall (*PFNWGLCHOOSEPIXELFORMATARBPROC)(HDC DC, const PGLint piAttribIList, const PGLfloat pfAttribFList, unsigned nMaxFormats, PGLint piFormats, System::PCardinal nNumFormats);

typedef System::LongBool __stdcall (*PFNWGLMAKECONTEXTCURRENTARBPROC)(HDC hDrawDC, HDC hReadDC, HGLRC _hglrc);

typedef HDC __stdcall (*PFNWGLGETCURRENTREADDCARBPROC)(void);

typedef HPBUFFERARB __stdcall (*PFNWGLCREATEPBUFFERARBPROC)(HDC DC, TGLint iPixelFormat, TGLint iWidth, TGLint iHeight, const PGLint piAttribList);

typedef HDC __stdcall (*PFNWGLGETPBUFFERDCARBPROC)(HPBUFFERARB hPbuffer);

typedef int __stdcall (*PFNWGLRELEASEPBUFFERDCARBPROC)(HPBUFFERARB hPbuffer, HDC DC);

typedef System::LongBool __stdcall (*PFNWGLDESTROYPBUFFERARBPROC)(HPBUFFERARB hPbuffer);

typedef System::LongBool __stdcall (*PFNWGLQUERYPBUFFERARBPROC)(HPBUFFERARB hPbuffer, int iAttribute, PGLint piValue);

typedef System::LongBool __stdcall (*PFNWGLBINDTEXIMAGEARBPROC)(HPBUFFERARB hPbuffer, int iBuffer);

typedef System::LongBool __stdcall (*PFNWGLRELEASETEXIMAGEARBPROC)(HPBUFFERARB hpBuffer, int iBuffer);

typedef System::LongBool __stdcall (*PFNWGLSETPBUFFERATTRIBARBPROC)(HPBUFFERARB hpBuffer, const PGLint piAttribList);

typedef HGLRC __stdcall (*PFNWGLCREATECONTEXTATTRIBSARBPROC)(HDC DC, HGLRC hShareContext, PGLint attribList);

typedef System::LongBool __stdcall (*PFNWGLSWAPINTERVALEXTPROC)(int interval);

typedef int __stdcall (*PFNWGLGETSWAPINTERVALEXTPROC)(void);

typedef bool __stdcall (*PFNWGLENUMGPUSNVPROC)(unsigned iGpuIndex, HGPUNV &hGpu);

typedef bool __stdcall (*PFNWGLENUMGPUDEVICESNVPROC)(HGPUNV hGpu, unsigned iDeviceIndex, PGPUDEVICE lpGpuDevice);

typedef HDC __stdcall (*PFNWGLCREATEAFFINITYDCNVPROC)(PHGPUNV hGpuList);

typedef bool __stdcall (*PFNWGLENUMGPUSFROMAFFINITYDCNVPROC)(HDC hAffinityDC, unsigned iGpuIndex, HGPUNV &hGpu);

typedef bool __stdcall (*PFNWGLDELETEDCNVPROC)(HDC hdc);

typedef System::LongBool __stdcall (*PFNWGLDXSETRESOURCESHAREHANDLEPROC)(void * dxObject, Winapi::Windows::THandle shareHandle);

typedef Winapi::Windows::THandle __stdcall (*PFNWGLDXOPENDEVICEPROC)(void * dxDevice);

typedef System::LongBool __stdcall (*PFNWGLDXCLOSEDEVICEPROC)(Winapi::Windows::THandle hDevice);

typedef Winapi::Windows::THandle __stdcall (*PFNWGLDXREGISTEROBJECTPROC)(Winapi::Windows::THandle hDevice, void * dxObject, unsigned name, unsigned atype, unsigned access);

typedef System::LongBool __stdcall (*PFNWGLDXUNREGISTEROBJECTPROC)(Winapi::Windows::THandle hDevice, Winapi::Windows::THandle hObject);

typedef System::LongBool __stdcall (*PFNWGLDXOBJECTACCESSPROC)(Winapi::Windows::THandle hObject, unsigned access);

typedef System::LongBool __stdcall (*PFNWGLDXLOCKOBJECTSPROC)(Winapi::Windows::THandle hDevice, TGLint count, Winapi::Windows::PHandle hObjects);

typedef System::LongBool __stdcall (*PFNWGLDXUNLOCKOBJECTSNVPROC)(Winapi::Windows::THandle hDevice, TGLint count, Winapi::Windows::PHandle hObjects);

typedef void __stdcall (*PFNGLSAMPLEPASSARBPROC)(unsigned pass);

typedef void __stdcall (*PFNGLACTIVETEXTUREARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLCLIENTACTIVETEXTUREARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DARBPROC)(unsigned target, TGLdouble s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DVARBPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FARBPROC)(unsigned target, TGLfloat s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FVARBPROC)(unsigned target, TGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IARBPROC)(unsigned target, TGLint s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IVARBPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SARBPROC)(unsigned target, TGLshort s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SVARBPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DARBPROC)(unsigned target, TGLdouble s, TGLdouble t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DVARBPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FARBPROC)(unsigned target, TGLfloat s, TGLfloat t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FVARBPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IARBPROC)(unsigned target, TGLint s, TGLint t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IVARBPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SARBPROC)(unsigned target, TGLshort s, TGLshort t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SVARBPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DARBPROC)(unsigned target, TGLdouble s, TGLdouble t, TGLdouble r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DVARBPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FARBPROC)(unsigned target, TGLfloat s, TGLfloat t, TGLfloat r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FVARBPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IARBPROC)(unsigned target, TGLint s, TGLint t, TGLint r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IVARBPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SARBPROC)(unsigned target, TGLshort s, TGLshort t, TGLshort r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SVARBPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DARBPROC)(unsigned target, TGLdouble s, TGLdouble t, TGLdouble r, TGLdouble q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DVARBPROC)(unsigned target, PGLdouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FARBPROC)(unsigned target, TGLfloat s, TGLfloat t, TGLfloat r, TGLfloat q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FVARBPROC)(unsigned target, PGLfloat v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IARBPROC)(unsigned target, TGLint s, TGLint t, TGLint r, TGLint q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IVARBPROC)(unsigned target, PGLint v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SARBPROC)(unsigned target, TGLshort s, TGLshort t, TGLshort r, TGLshort q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SVARBPROC)(unsigned target, PGLshort v);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXFARBPROC)(PGLfloat m);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXDARBPROC)(PGLdouble m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXFARBPROC)(PGLfloat m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXDARBPROC)(PGLdouble m);

typedef void __stdcall (*PFNGLSAMPLECOVERAGEARBPROC)(float Value, TGLboolean invert);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE3DARBPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLsizei Height, TGLsizei depth, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE2DARBPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLsizei Height, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE1DARBPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei Width, TGLint border, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC)(unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned Format, TGLsizei imageSize, void * data);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXIMAGEARBPROC)(unsigned target, TGLint level, void * img);

typedef void __stdcall (*PFNGLPOINTPARAMETERFARBPROC)(unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLPOINTPARAMETERFVARBPROC)(unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLWEIGHTBVARBPROC)(TGLint size, PGLbyte weights);

typedef void __stdcall (*PFNGLWEIGHTSVARBPROC)(TGLint size, PGLshort weights);

typedef void __stdcall (*PFNGLWEIGHTIVARBPROC)(TGLint size, PGLint weights);

typedef void __stdcall (*PFNGLWEIGHTFVARBPROC)(TGLint size, PGLfloat weights);

typedef void __stdcall (*PFNGLWEIGHTDVARBPROC)(TGLint size, PGLdouble weights);

typedef void __stdcall (*PFNGLWEIGHTUBVARBPROC)(TGLint size, PGLubyte weights);

typedef void __stdcall (*PFNGLWEIGHTUSVARBPROC)(TGLint size, PGLushort weights);

typedef void __stdcall (*PFNGLWEIGHTUIVARBPROC)(TGLint size, PGLuint weights);

typedef void __stdcall (*PFNGLWEIGHTPOINTERARBPROC)(TGLint size, unsigned _type, TGLsizei stride, void * _pointer);

typedef void __stdcall (*PFNGLVERTEXBLENDARBPROC)(TGLint count);

typedef void __stdcall (*PFNGLCURRENTPALETTEMATRIXARBPROC)(TGLint index);

typedef void __stdcall (*PFNGLMATRIXINDEXUBVARBPROC)(TGLint size, PGLubyte indices);

typedef void __stdcall (*PFNGLMATRIXINDEXUSVARBPROC)(TGLint size, PGLushort indices);

typedef void __stdcall (*PFNGLMATRIXINDEXUIVARBPROC)(TGLint size, PGLuint indices);

typedef void __stdcall (*PFNGLMATRIXINDEXPOINTERARBPROC)(TGLint size, unsigned _type, TGLsizei stride, void * _pointer);

typedef void __stdcall (*PFNGLWINDOWPOS2DARBPROC)(TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLWINDOWPOS2DVARBPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLWINDOWPOS2FARBPROC)(TGLfloat x, TGLfloat y);

typedef void __stdcall (*PFNGLWINDOWPOS2FVARBPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLWINDOWPOS2IARBPROC)(TGLint x, TGLint y);

typedef void __stdcall (*PFNGLWINDOWPOS2IVARBPROC)(PGLint v);

typedef void __stdcall (*PFNGLWINDOWPOS2SARBPROC)(TGLshort x, TGLshort y);

typedef void __stdcall (*PFNGLWINDOWPOS2SVARBPROC)(PGLshort v);

typedef void __stdcall (*PFNGLWINDOWPOS3DARBPROC)(TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLWINDOWPOS3DVARBPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLWINDOWPOS3FARBPROC)(TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLWINDOWPOS3FVARBPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLWINDOWPOS3IARBPROC)(TGLint x, TGLint y, TGLint z);

typedef void __stdcall (*PFNGLWINDOWPOS3IVARBPROC)(PGLint v);

typedef void __stdcall (*PFNGLWINDOWPOS3SARBPROC)(TGLshort x, TGLshort y, TGLshort z);

typedef void __stdcall (*PFNGLWINDOWPOS3SVARBPROC)(PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DARBPROC)(unsigned index, TGLdouble x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVARBPROC)(unsigned index, const PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FARBPROC)(unsigned index, TGLfloat x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVARBPROC)(unsigned index, const PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SARBPROC)(unsigned index, TGLshort x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVARBPROC)(unsigned index, const PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DARBPROC)(unsigned index, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVARBPROC)(unsigned index, const PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FARBPROC)(unsigned index, TGLfloat x, TGLfloat y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVARBPROC)(unsigned index, const PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SARBPROC)(unsigned index, TGLshort x, TGLshort y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVARBPROC)(unsigned index, const PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DARBPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVARBPROC)(unsigned index, const PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FARBPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVARBPROC)(unsigned index, const PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SARBPROC)(unsigned index, TGLshort x, TGLshort y, TGLshort z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVARBPROC)(unsigned index, const PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NBVARBPROC)(unsigned index, const PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NIVARBPROC)(unsigned index, const PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NSVARBPROC)(unsigned index, const PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBARBPROC)(unsigned index, TGLubyte x, TGLubyte y, TGLubyte z, TGLubyte w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBVARBPROC)(unsigned index, const PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUIVARBPROC)(unsigned index, const PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUSVARBPROC)(unsigned index, const PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4BVARBPROC)(unsigned index, const PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DARBPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVARBPROC)(unsigned index, const PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FARBPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVARBPROC)(unsigned index, const PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4IVARBPROC)(unsigned index, const PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SARBPROC)(unsigned index, TGLshort x, TGLshort y, TGLshort z, TGLshort w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVARBPROC)(unsigned index, const PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVARBPROC)(unsigned index, const PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UIVARBPROC)(unsigned index, const PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4USVARBPROC)(unsigned index, const PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERARBPROC)(unsigned index, TGLint size, unsigned _type, TGLboolean normalized, TGLsizei stride, const void * _pointer);

typedef void __stdcall (*PFNGLENABLEVERTEXATTRIBARRAYARBPROC)(unsigned index);

typedef void __stdcall (*PFNGLDISABLEVERTEXATTRIBARRAYARBPROC)(unsigned index);

typedef void __stdcall (*PFNGLPROGRAMSTRINGARBPROC)(unsigned target, unsigned format, TGLsizei len, const void * _string);

typedef void __stdcall (*PFNGLBINDPROGRAMARBPROC)(unsigned target, unsigned _program);

typedef void __stdcall (*PFNGLDELETEPROGRAMSARBPROC)(TGLsizei n, const PGLuint programs);

typedef void __stdcall (*PFNGLGENPROGRAMSARBPROC)(TGLsizei n, PGLuint programs);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4DARBPROC)(unsigned target, unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4DVARBPROC)(unsigned target, unsigned index, const PGLdouble params);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4FARBPROC)(unsigned target, unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4FVARBPROC)(unsigned target, unsigned index, const PGLfloat params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4DARBPROC)(unsigned target, unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4DVARBPROC)(unsigned target, unsigned index, const PGLdouble params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4FARBPROC)(unsigned target, unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4FVARBPROC)(unsigned target, unsigned index, const PGLfloat params);

typedef void __stdcall (*PFNGLGETPROGRAMENVPARAMETERDVARBPROC)(unsigned target, unsigned index, PGLdouble params);

typedef void __stdcall (*PFNGLGETPROGRAMENVPARAMETERFVARBPROC)(unsigned target, unsigned index, PGLfloat params);

typedef void __stdcall (*PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC)(unsigned target, unsigned index, PGLdouble params);

typedef void __stdcall (*PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC)(unsigned target, unsigned index, PGLfloat params);

typedef void __stdcall (*PFNGLGETPROGRAMIVARBPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETPROGRAMSTRINGARBPROC)(unsigned target, unsigned pname, void * _string);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVARBPROC)(unsigned index, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVARBPROC)(unsigned index, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVARBPROC)(unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVARBPROC)(unsigned index, unsigned pname, void * _pointer);

typedef TGLboolean __stdcall (*PFNGLISPROGRAMARBPROC)(unsigned _program);

typedef void __stdcall (*PFNGLBINDBUFFERARBPROC)(unsigned target, unsigned buffer);

typedef void __stdcall (*PFNGLDELETEBUFFERSARBPROC)(TGLsizei n, const PGLuint buffers);

typedef void __stdcall (*PFNGLGENBUFFERSARBPROC)(TGLsizei n, PGLuint buffers);

typedef TGLboolean __stdcall (*PFNGLISBUFFERARBPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLBUFFERDATAARBPROC)(unsigned target, TGLsizei size, const void * data, unsigned usage);

typedef void __stdcall (*PFNGLBUFFERSUBDATAARBPROC)(unsigned target, unsigned offset, TGLsizei size, const void * data);

typedef void __stdcall (*PFNGLGETBUFFERSUBDATAARBPROC)(unsigned target, unsigned offset, TGLsizei size, void * data);

typedef void * __stdcall (*PFNGLMAPBUFFERARBPROC)(unsigned target, unsigned access);

typedef TGLboolean __stdcall (*PFNGLUNMAPBUFFERARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERIVARBPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETBUFFERPOINTERVARBPROC)(unsigned target, unsigned pname, void * params);

typedef void __stdcall (*PFNGLGENQUERIESARBPROC)(TGLsizei n, PGLuint ids);

typedef void __stdcall (*PFNGLDELETEQUERIESARBPROC)(TGLsizei n, const PGLuint ids);

typedef TGLboolean __stdcall (*PFNGLISQUERYARBPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINQUERYARBPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETQUERYIVARBPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTIVARBPROC)(unsigned id, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUIVARBPROC)(unsigned id, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLDELETEOBJECTARBPROC)(GLhandleARB obj);

typedef GLhandleARB __stdcall (*PFNGLGETHANDLEARBPROC)(unsigned pname);

typedef void __stdcall (*PFNGLDETACHOBJECTARBPROC)(GLhandleARB containerObj, GLhandleARB attachedObj);

typedef GLhandleARB __stdcall (*PFNGLCREATESHADEROBJECTARBPROC)(unsigned shaderType);

typedef void __stdcall (*PFNGLSHADERSOURCEARBPROC)(GLhandleARB shaderObj, TGLsizei count, const PGLPCharArray _string, const PGLint length);

typedef void __stdcall (*PFNGLCOMPILESHADERARBPROC)(GLhandleARB shaderObj);

typedef GLhandleARB __stdcall (*PFNGLCREATEPROGRAMOBJECTARBPROC)(void);

typedef void __stdcall (*PFNGLATTACHOBJECTARBPROC)(GLhandleARB containerObj, GLhandleARB obj);

typedef void __stdcall (*PFNGLLINKPROGRAMARBPROC)(GLhandleARB programObj);

typedef void __stdcall (*PFNGLUSEPROGRAMOBJECTARBPROC)(GLhandleARB programObj);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMARBPROC)(GLhandleARB programObj);

typedef void __stdcall (*PFNGLUNIFORM1FARBPROC)(TGLint location, TGLfloat v0);

typedef void __stdcall (*PFNGLUNIFORM2FARBPROC)(TGLint location, TGLfloat v0, TGLfloat v1);

typedef void __stdcall (*PFNGLUNIFORM3FARBPROC)(TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2);

typedef void __stdcall (*PFNGLUNIFORM4FARBPROC)(TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2, TGLfloat v3);

typedef void __stdcall (*PFNGLUNIFORM1IARBPROC)(TGLint location, TGLint v0);

typedef void __stdcall (*PFNGLUNIFORM2IARBPROC)(TGLint location, TGLint v0, TGLint v1);

typedef void __stdcall (*PFNGLUNIFORM3IARBPROC)(TGLint location, TGLint v0, TGLint v1, TGLint v2);

typedef void __stdcall (*PFNGLUNIFORM4IARBPROC)(TGLint location, TGLint v0, TGLint v1, TGLint v2, TGLint v3);

typedef void __stdcall (*PFNGLUNIFORM1FVARBPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM2FVARBPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM3FVARBPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM4FVARBPROC)(TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORM1IVARBPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM2IVARBPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM3IVARBPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORM4IVARBPROC)(TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2FVARBPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3FVARBPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4FVARBPROC)(TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLGETOBJECTPARAMETERFVARBPROC)(GLhandleARB obj, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETOBJECTPARAMETERIVARBPROC)(GLhandleARB obj, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETINFOLOGARBPROC)(GLhandleARB obj, TGLsizei maxLength, PGLsizei length, char * infoLog);

typedef void __stdcall (*PFNGLGETATTACHEDOBJECTSARBPROC)(GLhandleARB containerObj, TGLsizei maxCount, PGLsizei count, PGLhandleARB obj);

typedef TGLint __stdcall (*PFNGLGETUNIFORMLOCATIONARBPROC)(GLhandleARB programObj, const char * name);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMARBPROC)(GLhandleARB programObj, unsigned index, TGLsizei maxLength, PGLsizei length, PGLint size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETUNIFORMFVARBPROC)(GLhandleARB programObj, TGLint location, PGLfloat params);

typedef void __stdcall (*PFNGLGETUNIFORMIVARBPROC)(GLhandleARB programObj, TGLint location, PGLint params);

typedef void __stdcall (*PFNGLGETSHADERSOURCEARBPROC)(GLhandleARB obj, TGLsizei maxLength, PGLsizei length, char * source);

typedef void __stdcall (*PFNGLBINDATTRIBLOCATIONARBPROC)(GLhandleARB programObj, unsigned index, const char * name);

typedef void __stdcall (*PFNGLGETACTIVEATTRIBARBPROC)(GLhandleARB programObj, unsigned index, TGLsizei maxLength, PGLsizei length, PGLint size, System::PCardinal _type, char * name);

typedef TGLint __stdcall (*PFNGLGETATTRIBLOCATIONARBPROC)(GLhandleARB programObj, const char * name);

typedef void __stdcall (*PFNGLDRAWBUFFERSARBPROC)(TGLsizei n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLCLAMPCOLORARBPROC)(unsigned target, unsigned clamp);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDARBPROC)(unsigned mode, TGLint first, TGLsizei count, TGLsizei primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDARBPROC)(unsigned mode, TGLsizei count, unsigned _type, PGLvoid indices, TGLsizei primcount);

typedef TGLboolean __stdcall (*PFNGLISRENDERBUFFERPROC)(unsigned renderbuffer);

typedef void __stdcall (*PFNGLBINDRENDERBUFFERPROC)(unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLDELETERENDERBUFFERSPROC)(TGLsizei n, PGLuint renderbuffers);

typedef void __stdcall (*PFNGLGENRENDERBUFFERSPROC)(TGLsizei n, PGLuint renderbuffers);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEPROC)(unsigned target, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC)(unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETRENDERBUFFERPARAMETERIVPROC)(unsigned target, unsigned pname, PGLint params);

typedef TGLboolean __stdcall (*PFNGLISFRAMEBUFFERPROC)(unsigned framebuffer);

typedef void __stdcall (*PFNGLBINDFRAMEBUFFERPROC)(unsigned target, unsigned framebuffer);

typedef void __stdcall (*PFNGLDELETEFRAMEBUFFERSPROC)(TGLsizei n, PGLuint framebuffers);

typedef void __stdcall (*PFNGLGENFRAMEBUFFERSPROC)(TGLsizei n, PGLuint framebuffers);

typedef unsigned __stdcall (*PFNGLCHECKFRAMEBUFFERSTATUSPROC)(unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE1DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE2DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE3DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level, TGLint layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYERPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level, TGLint layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERRENDERBUFFERPROC)(unsigned target, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC)(unsigned target, unsigned attachment, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLBLITFRAMEBUFFERPROC)(TGLint srcX0, TGLint srcY0, TGLint srcX1, TGLint srcY1, TGLint dstX0, TGLint dstY0, TGLint dstX1, TGLint dstY1, TGLbitfield mask, unsigned filter);

typedef void __stdcall (*PFNGLGENERATEMIPMAPPROC)(unsigned target);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIARBPROC)(unsigned _program, unsigned pname, TGLint value);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREARBPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYERARBPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level, TGLint layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREFACEARBPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level, unsigned face);

typedef void __stdcall (*PFNGLVERTEXATTRIBDIVISORARBPROC)(unsigned index, unsigned divisor);

typedef void * __stdcall (*PFNGLMAPBUFFERRANGEPROC)(unsigned target, TGLint offset, TGLsizei length, TGLbitfield access);

typedef void __stdcall (*PFNGLFLUSHMAPPEDBUFFERRANGEPROC)(unsigned target, TGLint offset, TGLsizei length);

typedef void __stdcall (*PFNGLTEXBUFFERARBPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLBINDVERTEXARRAYPROC)(unsigned _array);

typedef void __stdcall (*PFNGLDELETEVERTEXARRAYSPROC)(TGLsizei n, PGLuint arrays);

typedef void __stdcall (*PFNGLGENVERTEXARRAYSPROC)(TGLsizei n, PGLuint arrays);

typedef TGLboolean __stdcall (*PFNGLISVERTEXARRAYPROC)(unsigned _array);

typedef void __stdcall (*PFNGLGETUNIFORMINDICESPROC)(unsigned _program, TGLsizei uniformCount, PGLPCharArray uniformNames, PGLuint uniformIndices);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMSIVPROC)(unsigned _program, TGLsizei uniformCount, PGLuint uniformIndices, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMNAMEPROC)(unsigned _program, unsigned uniformIndex, TGLsizei bufSize, PGLsizei length, char * uniformName);

typedef unsigned __stdcall (*PFNGLGETUNIFORMBLOCKINDEXPROC)(unsigned _program, char * uniformBlockName);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMBLOCKIVPROC)(unsigned _program, unsigned uniformBlockIndex, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC)(unsigned _program, unsigned uniformBlockIndex, TGLsizei bufSize, PGLsizei length, char * uniformBlockName);

typedef void __stdcall (*PFNGLUNIFORMBLOCKBINDINGPROC)(unsigned _program, unsigned uniformBlockIndex, unsigned uniformBlockBinding);

typedef void __stdcall (*PFNGLCOPYBUFFERSUBDATAPROC)(unsigned readTarget, unsigned writeTarget, TGLintptr readOffset, TGLintptr writeOffset, TGLsizeiptr size);

typedef void __stdcall (*PFNGLDRAWELEMENTSBASEVERTEXPROC)(unsigned mode, TGLsizei count, unsigned _type, PGLvoid indices, TGLint basevertex);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC)(unsigned mode, unsigned start, unsigned _end, TGLsizei count, unsigned _type, PGLvoid indices, TGLint basevertex);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC)(unsigned mode, TGLsizei count, unsigned _type, PGLvoid indices, TGLsizei primcount, TGLint basevertex);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC)(unsigned mode, PGLsizei count, unsigned _type, void *indices, TGLsizei primcount, PGLint basevertex);

typedef void __stdcall (*PFNGLPROVOKINGVERTEXPROC)(unsigned mode);

typedef TGLsync __stdcall (*PFNGLFENCESYNCPROC)(unsigned condition, TGLbitfield flags);

typedef TGLboolean __stdcall (*PFNGLISSYNCPROC)(TGLsync sync);

typedef void __stdcall (*PFNGLDELETESYNCPROC)(TGLsync sync);

typedef unsigned __stdcall (*PFNGLCLIENTWAITSYNCPROC)(TGLsync sync, TGLbitfield flags, TGLuint64 timeout);

typedef void __stdcall (*PFNGLWAITSYNCPROC)(TGLsync sync, TGLbitfield flags, TGLuint64 timeout);

typedef void __stdcall (*PFNGLGETINTEGER64VPROC)(unsigned pname, PGLint64 params);

typedef void __stdcall (*PFNGLGETSYNCIVPROC)(TGLsync sync, unsigned pname, TGLsizei bufSize, PGLsizei length, PGLint values);

typedef void __stdcall (*PFNGLTEXIMAGE2DMULTISAMPLEPROC)(unsigned target, TGLsizei samples, TGLint internalformat, TGLsizei width, TGLsizei height, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLTEXIMAGE3DMULTISAMPLEPROC)(unsigned target, TGLsizei samples, TGLint internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLGETMULTISAMPLEFVPROC)(unsigned pname, unsigned index, PGLfloat val);

typedef void __stdcall (*PFNGLSAMPLEMASKIPROC)(unsigned index, TGLbitfield mask);

typedef void __stdcall (*PFNGLBLENDEQUATIONIARBPROC)(unsigned buf, unsigned mode);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEIARBPROC)(unsigned buf, unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLBLENDFUNCIARBPROC)(unsigned buf, unsigned src, unsigned dst);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEIARBPROC)(unsigned buf, unsigned srcRGB, unsigned dstRGB, unsigned srcAlpha, unsigned dstAlpha);

typedef void __stdcall (*PFNGLMINSAMPLESHADINGARBPROC)(float value);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONINDEXEDPROC)(unsigned _program, unsigned colorNumber, unsigned index, const char * name);

typedef TGLint __stdcall (*PFNGLGETFRAGDATAINDEXPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGENSAMPLERSPROC)(TGLsizei count, PGLuint samplers);

typedef void __stdcall (*PFNGLDELETESAMPLERSPROC)(TGLsizei count, const PGLuint samplers);

typedef TGLboolean __stdcall (*PFNGLISSAMPLERPROC)(unsigned sampler);

typedef void __stdcall (*PFNGLBINDSAMPLERPROC)(unsigned _unit, unsigned sampler);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIPROC)(unsigned sampler, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIVPROC)(unsigned sampler, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERFPROC)(unsigned sampler, unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERFVPROC)(unsigned sampler, unsigned pname, const PGLfloat params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIIVPROC)(unsigned sampler, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIUIVPROC)(unsigned sampler, unsigned pname, const PGLuint params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIVPROC)(unsigned sampler, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIIVPROC)(unsigned sampler, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERFVPROC)(unsigned sampler, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIFVPROC)(unsigned sampler, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLQUERYCOUNTERPROC)(unsigned id, unsigned target);

typedef void __stdcall (*PFNGLGETQUERYOBJECTI64VPROC)(unsigned id, unsigned pname, PGLint64 params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUI64VPROC)(unsigned id, unsigned pname, PGLuint64 params);

typedef void __stdcall (*PFNGLVERTEXP2UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP2UIVPROC)(unsigned _type, const PGLuint value);

typedef void __stdcall (*PFNGLVERTEXP3UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP3UIVPROC)(unsigned _type, const PGLuint value);

typedef void __stdcall (*PFNGLVERTEXP4UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP4UIVPROC)(unsigned _type, const PGLuint value);

typedef void __stdcall (*PFNGLTEXCOORDP1UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP1UIVPROC)(unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLTEXCOORDP2UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP2UIVPROC)(unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLTEXCOORDP3UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP3UIVPROC)(unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLTEXCOORDP4UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP4UIVPROC)(unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP1UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP1UIVPROC)(unsigned texture, unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP2UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP2UIVPROC)(unsigned texture, unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP3UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP3UIVPROC)(unsigned texture, unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP4UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP4UIVPROC)(unsigned texture, unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLNORMALP3UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLNORMALP3UIVPROC)(unsigned _type, const PGLuint coords);

typedef void __stdcall (*PFNGLCOLORP3UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLCOLORP3UIVPROC)(unsigned _type, const PGLuint color);

typedef void __stdcall (*PFNGLCOLORP4UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLCOLORP4UIVPROC)(unsigned _type, const PGLuint color);

typedef void __stdcall (*PFNGLSECONDARYCOLORP3UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLSECONDARYCOLORP3UIVPROC)(unsigned _type, const PGLuint color);

typedef void __stdcall (*PFNGLVERTEXATTRIBP1UIPROC)(unsigned index, unsigned _type, TGLboolean normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP1UIVPROC)(unsigned index, unsigned _type, TGLboolean normalized, const PGLuint value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP2UIPROC)(unsigned index, unsigned _type, TGLboolean normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP2UIVPROC)(unsigned index, unsigned _type, TGLboolean normalized, const PGLuint value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP3UIPROC)(unsigned index, unsigned _type, TGLboolean normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP3UIVPROC)(unsigned index, unsigned _type, TGLboolean normalized, const PGLuint value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP4UIPROC)(unsigned index, unsigned _type, TGLboolean normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP4UIVPROC)(unsigned index, unsigned _type, TGLboolean normalized, const PGLuint value);

typedef void __stdcall (*PFNGLDRAWARRAYSINDIRECTPROC)(unsigned mode, const PGLvoid indirect);

typedef void __stdcall (*PFNGLDRAWELEMENTSINDIRECTPROC)(unsigned mode, unsigned _type, const PGLvoid indirect);

typedef void __stdcall (*PFNGLUNIFORM1DPROC)(TGLint location, TGLdouble x);

typedef void __stdcall (*PFNGLUNIFORM2DPROC)(TGLint location, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLUNIFORM3DPROC)(TGLint location, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLUNIFORM4DPROC)(TGLint location, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLUNIFORM1DVPROC)(TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORM2DVPROC)(TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORM3DVPROC)(TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORM4DVPROC)(TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X3DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X4DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X2DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X4DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X2DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X3DVPROC)(TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLGETUNIFORMDVPROC)(unsigned _program, TGLint location, PGLdouble params);

typedef void __stdcall (*PFNGLCLIENTATTRIBDEFAULTEXTPROC)(TGLbitfield mask);

typedef void __stdcall (*PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC)(TGLbitfield mask);

typedef void __stdcall (*PFNGLMATRIXLOADFEXTPROC)(unsigned mode, const PGLfloat m);

typedef void __stdcall (*PFNGLMATRIXLOADDEXTPROC)(unsigned mode, const PGLdouble m);

typedef void __stdcall (*PFNGLMATRIXMULTFEXTPROC)(unsigned mode, const PGLfloat m);

typedef void __stdcall (*PFNGLMATRIXMULTDEXTPROC)(unsigned mode, const PGLdouble m);

typedef void __stdcall (*PFNGLMATRIXLOADIDENTITYEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXROTATEFEXTPROC)(unsigned mode, TGLfloat angle, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLMATRIXROTATEDEXTPROC)(unsigned mode, TGLdouble angle, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLMATRIXSCALEFEXTPROC)(unsigned mode, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLMATRIXSCALEDEXTPROC)(unsigned mode, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLMATRIXTRANSLATEFEXTPROC)(unsigned mode, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLMATRIXTRANSLATEDEXTPROC)(unsigned mode, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLMATRIXFRUSTUMEXTPROC)(unsigned mode, TGLdouble left, TGLdouble right, TGLdouble bottom, TGLdouble top, TGLdouble zNear, TGLdouble zFar);

typedef void __stdcall (*PFNGLMATRIXORTHOEXTPROC)(unsigned mode, TGLdouble left, TGLdouble right, TGLdouble bottom, TGLdouble top, TGLdouble zNear, TGLdouble zFar);

typedef void __stdcall (*PFNGLMATRIXPOPEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXPUSHEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXLOADTRANSPOSEFEXTPROC)(unsigned mode, const PGLfloat m);

typedef void __stdcall (*PFNGLMATRIXLOADTRANSPOSEDEXTPROC)(unsigned mode, const PGLdouble m);

typedef void __stdcall (*PFNGLMATRIXMULTTRANSPOSEFEXTPROC)(unsigned mode, const PGLfloat m);

typedef void __stdcall (*PFNGLMATRIXMULTTRANSPOSEDEXTPROC)(unsigned mode, const PGLdouble m);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERFVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLfloat params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIEXTPROC)(unsigned texture, unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLCOPYTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLint x, TGLint y, TGLsizei width, TGLint border);

typedef void __stdcall (*PFNGLCOPYTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLint x, TGLint y, TGLsizei width, TGLsizei height, TGLint border);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETTEXTUREIMAGEEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned format, unsigned type_, PGLvoid pixels);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERFVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned pname, TGLint params);

typedef void __stdcall (*PFNGLTEXTUREIMAGE3DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLfloat params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLCOPYMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLint x, TGLint y, TGLsizei width, TGLint border);

typedef void __stdcall (*PFNGLCOPYMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLint x, TGLint y, TGLsizei width, TGLsizei height, TGLint border);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETMULTITEXIMAGEEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned format, unsigned type_, PGLvoid pixels);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLMULTITEXIMAGE3DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, unsigned type_, const PGLvoid pixels);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLBINDMULTITEXTUREEXTPROC)(unsigned texunit, unsigned target, unsigned texture);

typedef void __stdcall (*PFNGLENABLECLIENTSTATEINDEXEDEXTPROC)(unsigned array_, unsigned index_);

typedef void __stdcall (*PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC)(unsigned array_, unsigned index_);

typedef void __stdcall (*PFNGLMULTITEXCOORDPOINTEREXTPROC)(unsigned texunit, TGLint size, unsigned type_, TGLsizei stride, const PGLvoid pointer);

typedef void __stdcall (*PFNGLMULTITEXENVFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLMULTITEXENVFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLfloat params);

typedef void __stdcall (*PFNGLMULTITEXENVIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLMULTITEXENVIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLMULTITEXGENDEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLdouble param);

typedef void __stdcall (*PFNGLMULTITEXGENDVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLdouble params);

typedef void __stdcall (*PFNGLMULTITEXGENFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLMULTITEXGENFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLfloat params);

typedef void __stdcall (*PFNGLMULTITEXGENIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLMULTITEXGENIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLGETMULTITEXENVFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETMULTITEXENVIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETMULTITEXGENDVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLGETMULTITEXGENFVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETMULTITEXGENIVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETFLOATINDEXEDVEXTPROC)(unsigned target, unsigned index_, PGLfloat data);

typedef void __stdcall (*PFNGLGETDOUBLEINDEXEDVEXTPROC)(unsigned target, unsigned index_, PGLdouble data);

typedef void __stdcall (*PFNGLGETPOINTERINDEXEDVEXTPROC)(unsigned target, unsigned index_, PGLvoid data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC)(unsigned texture, unsigned target, TGLint lod, PGLvoid img);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLint border, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned format, TGLsizei imageSize, const PGLvoid bits);

typedef void __stdcall (*PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC)(unsigned texunit, unsigned target, TGLint lod, PGLvoid img);

typedef void __stdcall (*PFNGLNAMEDPROGRAMSTRINGEXTPROC)(unsigned program_, unsigned target, unsigned format, TGLsizei len, const PGLvoid string_);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const PGLdouble params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const PGLfloat params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC)(unsigned program_, unsigned target, unsigned index_, PGLdouble params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC)(unsigned program_, unsigned target, unsigned index_, PGLfloat params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMIVEXTPROC)(unsigned program_, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMSTRINGEXTPROC)(unsigned program_, unsigned target, unsigned pname, PGLvoid string_);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLsizei count, const PGLfloat params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLint x, TGLint y, TGLint z, TGLint w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const PGLint params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLsizei count, const PGLint params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC)(unsigned program_, unsigned target, unsigned index_, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const PGLuint params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, TGLsizei count, const PGLuint params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, PGLint params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, PGLuint params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLuint params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLint params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const PGLuint params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLNAMEDBUFFERDATAEXTPROC)(unsigned buffer, TGLsizei size, const PGLvoid data, unsigned usage);

typedef void __stdcall (*PFNGLNAMEDBUFFERSUBDATAEXTPROC)(unsigned buffer, GLintptr offset, GLsizeiptr size, const PGLvoid data);

typedef PGLvoid __stdcall (*PFNGLMAPNAMEDBUFFEREXTPROC)(unsigned buffer, unsigned access);

typedef TGLboolean __stdcall (*PFNGLUNMAPNAMEDBUFFEREXTPROC)(unsigned buffer);

typedef PGLvoid __stdcall (*PFNGLMAPNAMEDBUFFERRANGEEXTPROC)(unsigned buffer, GLintptr offset, GLsizeiptr length, TGLbitfield access);

typedef void __stdcall (*PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC)(unsigned buffer, GLintptr offset, GLsizeiptr length);

typedef void __stdcall (*PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC)(unsigned readBuffer, unsigned writeBuffer, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC)(unsigned buffer, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPOINTERVEXTPROC)(unsigned buffer, unsigned pname, PGLvoid params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERSUBDATAEXTPROC)(unsigned buffer, GLintptr offset, GLsizeiptr size, PGLvoid data);

typedef void __stdcall (*PFNGLTEXTUREBUFFEREXTPROC)(unsigned texture, unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLMULTITEXBUFFEREXTPROC)(unsigned texunit, unsigned target, unsigned interformat, unsigned buffer);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC)(unsigned renderbuffer, unsigned interformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC)(unsigned renderbuffer, unsigned pname, PGLint params);

typedef unsigned __stdcall (*PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC)(unsigned framebuffer, unsigned target);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, TGLint level, TGLint zoffset);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC)(unsigned framebuffer, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGENERATETEXTUREMIPMAPEXTPROC)(unsigned texture, unsigned target);

typedef void __stdcall (*PFNGLGENERATEMULTITEXMIPMAPEXTPROC)(unsigned texunit, unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC)(unsigned framebuffer, unsigned mode);

typedef void __stdcall (*PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC)(unsigned framebuffer, TGLsizei n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLFRAMEBUFFERREADBUFFEREXTPROC)(unsigned framebuffer, unsigned mode);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC)(unsigned framebuffer, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC)(unsigned renderbuffer, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC)(unsigned renderbuffer, TGLsizei coverageSamples, TGLsizei colorSamples, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, TGLint level, TGLint layer);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, TGLint level, unsigned face);

typedef void __stdcall (*PFNGLTEXTURERENDERBUFFEREXTPROC)(unsigned texture, unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLMULTITEXRENDERBUFFEREXTPROC)(unsigned texunit, unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DEXTPROC)(unsigned _program, TGLint location, TGLdouble x);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DEXTPROC)(unsigned _program, TGLint location, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DEXTPROC)(unsigned _program, TGLint location, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DEXTPROC)(unsigned _program, TGLint location, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, const PGLdouble value);

typedef TGLint __stdcall (*PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC)(unsigned _program, unsigned shadertype, const char * name);

typedef unsigned __stdcall (*PFNGLGETSUBROUTINEINDEXPROC)(unsigned _program, unsigned shadertype, const char * name);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC)(unsigned _program, unsigned shadertype, unsigned index, unsigned pname, PGLint values);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC)(unsigned _program, unsigned shadertype, unsigned index, TGLsizei bufsize, PGLsizei length, char * name);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINENAMEPROC)(unsigned _program, unsigned shadertype, unsigned index, TGLsizei bufsize, PGLsizei length, char * name);

typedef void __stdcall (*PFNGLUNIFORMSUBROUTINESUIVPROC)(unsigned shadertype, TGLsizei count, const PGLuint indices);

typedef void __stdcall (*PFNGLGETUNIFORMSUBROUTINEUIVPROC)(unsigned shadertype, TGLint location, PGLuint params);

typedef void __stdcall (*PFNGLGETPROGRAMSTAGEIVPROC)(unsigned _program, unsigned shadertype, unsigned pname, PGLint values);

typedef void __stdcall (*PFNGLPATCHPARAMETERIPROC)(unsigned pname, TGLint value);

typedef void __stdcall (*PFNGLPATCHPARAMETERFVPROC)(unsigned pname, const PGLfloat values);

typedef void __stdcall (*PFNGLBINDTRANSFORMFEEDBACKPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLDELETETRANSFORMFEEDBACKSPROC)(TGLsizei n, const PGLuint ids);

typedef void __stdcall (*PFNGLGENTRANSFORMFEEDBACKSPROC)(TGLsizei n, PGLuint ids);

typedef TGLboolean __stdcall (*PFNGLISTRANSFORMFEEDBACKPROC)(unsigned id);

typedef void __stdcall (*PFNGLPAUSETRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLRESUMETRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLDRAWTRANSFORMFEEDBACKPROC)(unsigned mode, unsigned id);

typedef void __stdcall (*PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC)(unsigned mode, unsigned id, unsigned stream);

typedef void __stdcall (*PFNGLBEGINQUERYINDEXEDPROC)(unsigned target, unsigned index, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYINDEXEDPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLGETQUERYINDEXEDIVPROC)(unsigned target, unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLRELEASESHADERCOMPILERPROC)(void);

typedef void __stdcall (*PFNGLSHADERBINARYPROC)(TGLsizei count, PGLuint shaders, unsigned binaryformat, void * binary, TGLsizei length);

typedef void __stdcall (*PFNGLGETSHADERPRECISIONFORMATPROC)(unsigned shadertype, unsigned precisiontype, PGLint range, PGLint precision);

typedef void __stdcall (*PFNGLDEPTHRANGEFPROC)(float n, float f);

typedef void __stdcall (*PFNGLCLEARDEPTHFPROC)(TGLclampd depth);

typedef void __stdcall (*PFNGLGETPROGRAMBINARYPROC)(unsigned _program, TGLsizei bufSize, PGLsizei length, System::PCardinal binaryFormat, void * binary);

typedef void __stdcall (*PFNGLPROGRAMBINARYPROC)(unsigned _program, unsigned binaryFormat, void * binary, TGLsizei length);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIPROC)(unsigned _program, unsigned pname, TGLint value);

typedef void __stdcall (*PFNGLUSEPROGRAMSTAGESPROC)(unsigned pipeline, TGLbitfield stages, unsigned _program);

typedef void __stdcall (*PFNGLACTIVESHADERPROGRAMPROC)(unsigned pipeline, unsigned _program);

typedef unsigned __stdcall (*PFNGLCREATESHADERPROGRAMVPROC)(unsigned _type, TGLsizei count, const PGLPCharArray strings);

typedef void __stdcall (*PFNGLBINDPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLDELETEPROGRAMPIPELINESPROC)(TGLsizei n, PGLuint pipelines);

typedef void __stdcall (*PFNGLGENPROGRAMPIPELINESPROC)(TGLsizei n, PGLuint pipelines);

typedef TGLboolean __stdcall (*PFNGLISPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLGETPROGRAMPIPELINEIVPROC)(unsigned pipeline, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1IPROC)(unsigned _program, TGLint location, TGLint v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1IVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1FPROC)(unsigned _program, TGLint location, TGLfloat v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1FVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DPROC)(unsigned _program, TGLint location, TGLdouble v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1UIPROC)(unsigned _program, TGLint location, unsigned v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1UIVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2IPROC)(unsigned _program, TGLint location, TGLint v0, TGLint v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2IVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2FPROC)(unsigned _program, TGLint location, TGLfloat v0, TGLfloat v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2FVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DPROC)(unsigned _program, TGLint location, TGLdouble v0, TGLdouble v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2UIPROC)(unsigned _program, TGLint location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2UIVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3IPROC)(unsigned _program, TGLint location, TGLint v0, TGLint v1, TGLint v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3IVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3FPROC)(unsigned _program, TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3FVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DPROC)(unsigned _program, TGLint location, TGLdouble v0, TGLdouble v1, TGLdouble v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3UIPROC)(unsigned _program, TGLint location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3UIVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4IPROC)(unsigned _program, TGLint location, TGLint v0, TGLint v1, TGLint v2, TGLint v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4IVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4FPROC)(unsigned _program, TGLint location, TGLfloat v0, TGLfloat v1, TGLfloat v2, TGLfloat v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4FVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DPROC)(unsigned _program, TGLint location, TGLdouble v0, TGLdouble v1, TGLdouble v2, TGLdouble v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4UIPROC)(unsigned _program, TGLint location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4UIVPROC)(unsigned _program, TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLfloat value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC)(unsigned _program, TGLint location, TGLsizei count, TGLboolean transpose, PGLdouble value);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLGETPROGRAMPIPELINEINFOLOGPROC)(unsigned pipeline, TGLsizei bufSize, PGLsizei length, char * infoLog);

typedef void __stdcall (*PFNGLVERTEXATTRIBL1DPROC)(unsigned index, TGLdouble x);

typedef void __stdcall (*PFNGLVERTEXATTRIBL2DPROC)(unsigned index, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLVERTEXATTRIBL3DPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLVERTEXATTRIBL4DPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLVERTEXATTRIBL1DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL2DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL3DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL4DVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBLPOINTERPROC)(unsigned index, TGLint size, unsigned _type, TGLsizei stride, void * ptr);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBLDVPROC)(unsigned index, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC)(unsigned vaobj, unsigned buffer, unsigned index, TGLint size, unsigned _type, TGLsizei stride, TGLintptr offset);

typedef void __stdcall (*PFNGLVIEWPORTARRAYVPROC)(unsigned first, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLVIEWPORTINDEXEDFPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat w, TGLfloat h);

typedef void __stdcall (*PFNGLVIEWPORTINDEXEDFVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLSCISSORARRAYVPROC)(unsigned first, TGLsizei count, PGLint v);

typedef void __stdcall (*PFNGLSCISSORINDEXEDPROC)(unsigned index, TGLint left, TGLint bottom, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLSCISSORINDEXEDVPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLDEPTHRANGEARRAYVPROC)(unsigned first, TGLsizei count, PGLclampd v);

typedef void __stdcall (*PFNGLDEPTHRANGEINDEXEDPROC)(unsigned index, TGLclampd n, TGLclampd f);

typedef void __stdcall (*PFNGLGETFLOATI_VPROC)(unsigned target, unsigned index, PGLfloat data);

typedef void __stdcall (*PFNGLGETDOUBLEI_VPROC)(unsigned target, unsigned index, PGLdouble data);

typedef void __stdcall (*PFNGLDEBUGMESSAGECONTROLARBPROC)(unsigned source, unsigned _type, unsigned severity, TGLsizei count, PGLuint ids, TGLboolean enabled);

typedef void __stdcall (*PFNGLDEBUGMESSAGEINSERTARBPROC)(unsigned source, unsigned _type, unsigned id, unsigned severity, TGLsizei length, char * buf);

typedef void __stdcall (*PFNGLDEBUGMESSAGECALLBACKARBPROC)(TGLDEBUGPROCARB callback, void * userParam);

typedef unsigned __stdcall (*PFNGLGETDEBUGMESSAGELOGARBPROC)(unsigned count, TGLsizei bufsize, System::PCardinal sources, System::PCardinal types, PGLuint ids, System::PCardinal severities, PGLsizei lengths, char * messageLog);

typedef unsigned __stdcall (*PFNGLGETGRAPHICSRESETSTATUSARBPROC)(void);

typedef void __stdcall (*PFNGLGETNMAPDVARBPROC)(unsigned target, unsigned query, TGLsizei bufSize, PGLdouble v);

typedef void __stdcall (*PFNGLGETNMAPFVARBPROC)(unsigned target, unsigned query, TGLsizei bufSize, PGLfloat v);

typedef void __stdcall (*PFNGLGETNMAPIVARBPROC)(unsigned target, unsigned query, TGLsizei bufSize, PGLint v);

typedef void __stdcall (*PFNGLGETNPIXELMAPFVARBPROC)(unsigned map, TGLsizei bufSize, PGLfloat values);

typedef void __stdcall (*PFNGLGETNPIXELMAPUIVARBPROC)(unsigned map, TGLsizei bufSize, PGLuint values);

typedef void __stdcall (*PFNGLGETNPIXELMAPUSVARBPROC)(unsigned map, TGLsizei bufSize, PGLushort values);

typedef void __stdcall (*PFNGLGETNPOLYGONSTIPPLEARBPROC)(TGLsizei bufSize, PGLubyte pattern);

typedef void __stdcall (*PFNGLGETNCOLORTABLEARBPROC)(unsigned target, unsigned format, unsigned _type, TGLsizei bufSize, void * table);

typedef void __stdcall (*PFNGLGETNCONVOLUTIONFILTERARBPROC)(unsigned target, unsigned format, unsigned _type, TGLsizei bufSize, void * image);

typedef void __stdcall (*PFNGLGETNSEPARABLEFILTERARBPROC)(unsigned target, unsigned format, unsigned _type, TGLsizei rowBufSize, void * row, TGLsizei columnBufSize, void * column, void * span);

typedef void __stdcall (*PFNGLGETNHISTOGRAMARBPROC)(unsigned target, TGLboolean reset, unsigned format, unsigned _type, TGLsizei bufSize, void * values);

typedef void __stdcall (*PFNGLGETNMINMAXARBPROC)(unsigned target, TGLboolean reset, unsigned format, unsigned _type, TGLsizei bufSize, void * values);

typedef void __stdcall (*PFNGLGETNTEXIMAGEARBPROC)(unsigned target, TGLint level, unsigned format, unsigned _type, TGLsizei bufSize, void * img);

typedef void __stdcall (*PFNGLREADNPIXELSARBPROC)(TGLint x, TGLint y, TGLsizei width, TGLsizei height, unsigned format, unsigned _type, TGLsizei bufSize, void * data);

typedef void __stdcall (*PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC)(unsigned target, TGLint lod, TGLsizei bufSize, void * img);

typedef void __stdcall (*PFNGLGETNUNIFORMFVARBPROC)(unsigned _program, TGLint location, TGLsizei bufSize, PGLfloat params);

typedef void __stdcall (*PFNGLGETNUNIFORMIVARBPROC)(unsigned _program, TGLint location, TGLsizei bufSize, PGLint params);

typedef void __stdcall (*PFNGLGETNUNIFORMUIVARBPROC)(unsigned _program, TGLint location, TGLsizei bufSize, PGLuint params);

typedef void __stdcall (*PFNGLGETNUNIFORMDVARBPROC)(unsigned _program, TGLint location, TGLsizei bufSize, PGLdouble params);

typedef void __stdcall (*PFNGLPushDebugGroup)(unsigned source, unsigned id, TGLsizei length, const char * message_);

typedef void __stdcall (*PFNGLPopDebugGroup)(void);

typedef void __stdcall (*PFNGLObjectLabel)(unsigned identifier, unsigned name, TGLsizei length, const char * label_);

typedef void __stdcall (*PFNGLGetObjectLabel)(unsigned identifier, unsigned name, TGLsizei bufsize, PGLsizei length, char * label_);

typedef void __stdcall (*PFNGLObjectPtrLabel)(const void * ptr, TGLsizei length, const char * label_);

typedef void __stdcall (*PFNGLGetObjectPtrLabel)(const void * ptr, TGLsizei bufSize, PGLsizei length, char * label_);

typedef void __stdcall (*PFNGLClearBufferData)(unsigned target, unsigned internalformat, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearBufferSubData)(unsigned target, unsigned internalformat, GLintptr offset, GLsizeiptr size, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearNamedBufferData)(unsigned buffer, unsigned internalformat, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearNamedBufferSubData)(unsigned buffer, unsigned internalformat, unsigned format, unsigned type_, GLsizeiptr offset, GLsizeiptr size, const void * data);

typedef void __stdcall (*PFNGLDispatchCompute)(unsigned num_groups_x, unsigned num_groups_y, unsigned num_groups_z);

typedef void __stdcall (*PFNGLDispatchComputeIndirect)(GLintptr indirect);

typedef void __stdcall (*PFNGLCopyImageSubData)(unsigned srcName, unsigned srcTarget, TGLint srcLevel, TGLint srcX, TGLint srcY, TGLint srcZ, unsigned dstName, unsigned dstTarget, TGLint dstLevel, TGLint dstX, TGLint dstY, TGLint dstZ, TGLsizei srcWidth, TGLsizei srcHeight, TGLsizei srcDepth);

typedef void __stdcall (*PFNGLFramebufferParameteri)(unsigned target, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLGetFramebufferParameteriv)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLNamedFramebufferParameteri)(unsigned framebuffer, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLGetNamedFramebufferParameteriv)(unsigned framebuffer, unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLGetInternalformati64v)(unsigned target, unsigned internalformat, unsigned pname, TGLsizei bufSize, PGLint64 params);

typedef void __stdcall (*PFNGLInvalidateTexSubImage)(unsigned texture, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth);

typedef void __stdcall (*PFNGLInvalidateTexImage)(unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLInvalidateBufferSubData)(unsigned buffer, GLintptr offset, GLsizeiptr length);

typedef void __stdcall (*PFNGLInvalidateBufferData)(unsigned buffer);

typedef void __stdcall (*PFNGLInvalidateFramebuffer)(unsigned target, TGLsizei numAttachments, const System::PCardinal attachments);

typedef void __stdcall (*PFNGLInvalidateSubFramebuffer)(unsigned target, TGLsizei numAttachments, const System::PCardinal attachments, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLMultiDrawArraysIndirect)(unsigned mode, const void * indirect, TGLsizei drawcount, TGLsizei stride);

typedef void __stdcall (*PFNGLMultiDrawElementsIndirect)(unsigned mode, unsigned type_, const void * indirect, TGLsizei drawcount, TGLsizei stride);

typedef void __stdcall (*PFNGLGetProgramInterfaceiv)(unsigned program_, unsigned programInterface, unsigned pname, PGLint params);

typedef unsigned __stdcall (*PFNGLGetProgramResourceIndex)(unsigned program_, unsigned programInterface, const char * name);

typedef void __stdcall (*PFNGLGetProgramResourceName)(unsigned program_, unsigned programInterface, unsigned index, TGLsizei bufSize, PGLsizei length, char * name);

typedef void __stdcall (*PFNGLGetProgramResourceiv)(unsigned program_, unsigned programInterface, unsigned index, TGLsizei propCount, const System::PCardinal props, TGLsizei bufSize, PGLsizei length, PGLint params);

typedef TGLint __stdcall (*PFNGLGetProgramResourceLocation)(unsigned program_, unsigned programInterface, const char * name);

typedef TGLint __stdcall (*PFNGLGetProgramResourceLocationIndex)(unsigned program_, unsigned programInterface, const char * name);

typedef void __stdcall (*PFNGLShaderStorageBlockBinding)(unsigned program_, unsigned storageBlockIndex, unsigned storageBlockBinding);

typedef void __stdcall (*PFNGLTexBufferRange)(unsigned target, unsigned internalformat, unsigned buffer, GLintptr offset, GLsizeiptr size);

typedef void __stdcall (*PFNGLTextureBufferRange)(unsigned texture, unsigned target, unsigned internalformat, unsigned buffer, GLintptr offset, GLsizeiptr size);

typedef void __stdcall (*PFNGLTexStorage2DMultisample)(unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLTexStorage3DMultisample)(unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLTextureStorage2DMultisample)(unsigned texture, unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLTextureStorage3DMultisample)(unsigned texture, unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLboolean fixedsamplelocations);

typedef void __stdcall (*PFNGLBufferStorage)(unsigned target, GLsizeiptr size, const void * data, TGLbitfield flags);

typedef void __stdcall (*PFNGLClearTexImage)(unsigned texture, TGLint level, unsigned format, unsigned _type, const void * data);

typedef void __stdcall (*PFNGLClearTexSubImage)(unsigned texture, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, unsigned _type, const void * Data);

typedef void __stdcall (*PFNGLBindBuffersBase)(unsigned target, unsigned first, TGLsizei count, const PGLuint buffers);

typedef void __stdcall (*PFNGLBindBuffersRange)(unsigned target, unsigned first, TGLsizei count, const PGLuint buffers, const GLintptr offsets, const GLsizeiptr sizes);

typedef void __stdcall (*PFNGLBindTextures)(unsigned first, TGLsizei count, const PGLuint textures);

typedef void __stdcall (*PFNGLBindSamplers)(unsigned first, TGLsizei count, const PGLuint samplers);

typedef void __stdcall (*PFNGLBindImageTextures)(unsigned first, TGLsizei count, const PGLuint textures);

typedef void __stdcall (*PFNGLBindVertexBuffers)(unsigned first, TGLsizei count, const unsigned buffers, const GLintptr offsets, const PGLsizei strides);

typedef void __stdcall (*PFNGLTextureView)(unsigned texture, unsigned target, unsigned origtexture, unsigned internalformat, unsigned minlevel, unsigned numlevels, unsigned minlayer, unsigned numlayers);

typedef void __stdcall (*PFNGLBindVertexBuffer)(unsigned bindingindex, unsigned buffer, GLintptr offset, TGLsizei stride);

typedef void __stdcall (*PFNGLVertexAttribFormat)(unsigned attribindex, TGLint size, unsigned type_, TGLboolean normalized, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribIFormat)(unsigned attribindex, TGLint size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribLFormat)(unsigned attribindex, TGLint size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribBinding)(unsigned attribindex, unsigned bindingindex);

typedef void __stdcall (*PFNGLVertexBindingDivisor)(unsigned bindingindex, unsigned divisor);

typedef void __stdcall (*PFNGLVertexArrayBindVertexBuffer)(unsigned vaobj, unsigned bindingindex, unsigned buffer, GLintptr offset, TGLsizei stride);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribFormat)(unsigned vaobj, unsigned attribindex, TGLint size, unsigned type_, TGLboolean normalized, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribIFormat)(unsigned vaobj, unsigned attribindex, TGLint size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribLFormat)(unsigned vaobj, unsigned attribindex, TGLint size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribBinding)(unsigned vaobj, unsigned attribindex, unsigned bindingindex);

typedef void __stdcall (*PFNGLVertexArrayVertexBindingDivisor)(unsigned vaobj, unsigned bindingindex, unsigned divisor);

typedef GLsync __stdcall (*PFNGLCreateSyncFromCLevent)(p_cl_context context, p_cl_event event, TGLbitfield flags);

typedef void __stdcall (*PFNGLARRAYELEMENTARRAYEXTPROC)(unsigned mode, TGLsizei count, void * pi);

typedef void __stdcall (*PFNGLADDSWAPHINTRECTWINPROC)(TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLBLENDCOLOREXTPROC)(float red, float green, float blue, float alpha);

typedef void __stdcall (*PFNGLPOLYGONOFFSETEXTPROC)(TGLfloat factor, TGLfloat bias);

typedef void __stdcall (*PFNGLTEXIMAGE3DEXTPROC)(unsigned target, TGLint level, unsigned internalformat, TGLsizei width, TGLsizei height, TGLsizei depth, TGLint border, unsigned Format, unsigned AType, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE1DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLsizei width, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE2DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLsizei width, TGLsizei height, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE3DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLsizei width, TGLsizei height, TGLsizei depth, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXIMAGE1DEXTPROC)(unsigned target, TGLint level, unsigned internalFormat, TGLint x, TGLint y, TGLsizei width, TGLint border);

typedef void __stdcall (*PFNGLCOPYTEXIMAGE2DEXTPROC)(unsigned target, TGLint level, unsigned internalFormat, TGLint x, TGLint y, TGLsizei width, TGLsizei height, TGLint border);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE1DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint x, TGLint y, TGLsizei width);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE2DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE3DEXTPROC)(unsigned target, TGLint level, TGLint xoffset, TGLint yoffset, TGLint zoffset, TGLint x, TGLint y, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGENTEXTURESEXTPROC)(TGLsizei n, PGLuint textures);

typedef void __stdcall (*PFNGLDELETETEXTURESEXTPROC)(TGLsizei n, PGLuint textures);

typedef void __stdcall (*PFNGLBINDTEXTUREEXTPROC)(unsigned target, unsigned texture);

typedef void __stdcall (*PFNGLPRIORITIZETEXTURESEXTPROC)(TGLsizei n, PGLuint textures, Winapi::Windows::PSingle priorities);

typedef TGLboolean __stdcall (*PFNGLARETEXTURESRESIDENTEXTPROC)(TGLsizei n, PGLuint textures, PGLboolean residences);

typedef TGLboolean __stdcall (*PFNGLISTEXTUREEXTPROC)(unsigned texture);

typedef void __stdcall (*PFNGLSAMPLEMASKSGISPROC)(float Value, TGLboolean invert);

typedef void __stdcall (*PFNGLSAMPLEPATTERNSGISPROC)(unsigned pattern);

typedef void __stdcall (*PFNGLBLENDEQUATIONEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLCOLORTABLEEXTPROC)(unsigned target, unsigned internalFormat, TGLsizei width, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLCOLORSUBTABLEEXTPROC)(unsigned target, TGLsizei start, TGLsizei count, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLGETCOLORTABLEEXTPROC)(unsigned target, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERFVEXTPROC)(unsigned target, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERIVEXTPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLINDEXMATERIALEXTPROC)(unsigned face, unsigned mode);

typedef void __stdcall (*PFNGLINDEXFUNCEXTPROC)(unsigned func, TGLfloat ref);

typedef void __stdcall (*PFNGLLOCKARRAYSEXTPROC)(TGLint first, TGLsizei count);

typedef void __stdcall (*PFNGLUNLOCKARRAYSEXTPROC)(void);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSEXTPROC)(unsigned mode, unsigned start, unsigned Aend, TGLsizei Count, unsigned Atype, void * indices);

typedef void __stdcall (*PFNGLBEGINSCENEEXTPROC)(void);

typedef void __stdcall (*PFNGLENDSCENEEXTPROC)(void);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BEXTPROC)(TGLbyte red, TGLbyte green, TGLbyte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BVEXTPROC)(PGLbyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DEXTPROC)(TGLdouble red, TGLdouble green, TGLdouble blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DVEXTPROC)(PGLdouble v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FEXTPROC)(TGLfloat red, TGLfloat green, TGLfloat blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FVEXTPROC)(PGLfloat v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IEXTPROC)(TGLint red, TGLint green, TGLint blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IVEXTPROC)(PGLint v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SEXTPROC)(TGLshort red, TGLshort green, TGLshort blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SVEXTPROC)(PGLshort v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBEXTPROC)(TGLubyte red, TGLubyte green, TGLubyte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBVEXTPROC)(PGLubyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIEXTPROC)(unsigned red, unsigned green, unsigned blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIVEXTPROC)(PGLuint v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USEXTPROC)(TGLushort red, TGLushort green, TGLushort blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USVEXTPROC)(PGLushort v);

typedef void __stdcall (*PFNGLSECONDARYCOLORPOINTEREXTPROC)(TGLint Size, unsigned Atype, TGLsizei stride, void * p);

typedef void __stdcall (*PFNGLMULTIDRAWARRAYSEXTPROC)(unsigned mode, PGLint First, PGLsizei Count, TGLsizei primcount);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSEXTPROC)(unsigned mode, PGLsizei Count, unsigned AType, void *indices, TGLsizei primcount);

typedef void __stdcall (*PFNGLFOGCOORDFEXTPROC)(TGLfloat coord);

typedef void __stdcall (*PFNGLFOGCOORDFVEXTPROC)(PGLfloat coord);

typedef void __stdcall (*PFNGLFOGCOORDDEXTPROC)(TGLdouble coord);

typedef void __stdcall (*PFNGLFOGCOORDDVEXTPROC)(PGLdouble coord);

typedef void __stdcall (*PFNGLFOGCOORDPOINTEREXTPROC)(unsigned AType, TGLsizei stride, void * p);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEEXTPROC)(unsigned sfactorRGB, unsigned dfactorRGB, unsigned sfactorAlpha, unsigned dfactorAlpha);

typedef void __stdcall (*PFNGLFLUSHVERTEXARRAYRANGENVPROC)(void);

typedef void __stdcall (*PFNGLVERTEXARRAYRANGENVPROC)(TGLsizei Size, void * p);

typedef void * __stdcall (*PFNWGLALLOCATEMEMORYNVPROC)(TGLsizei size, float readFrequency, float writeFrequency, float priority);

typedef void __stdcall (*PFNWGLFREEMEMORYNVPROC)(void * ptr);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERFVNVPROC)(unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERFNVPROC)(unsigned pname, TGLfloat param);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERIVNVPROC)(unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERINVPROC)(unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLCOMBINERINPUTNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned input, unsigned mapping, unsigned componentUsage);

typedef void __stdcall (*PFNGLCOMBINEROUTPUTNVPROC)(unsigned stage, unsigned portion, unsigned abOutput, unsigned cdOutput, unsigned sumOutput, unsigned scale, unsigned bias, TGLboolean abDotProduct, TGLboolean cdDotProduct, TGLboolean muxSum);

typedef void __stdcall (*PFNGLFINALCOMBINERINPUTNVPROC)(unsigned variable, unsigned input, unsigned mapping, unsigned componentUsage);

typedef void __stdcall (*PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC)(unsigned stage, unsigned portion, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC)(unsigned stage, unsigned portion, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC)(unsigned variable, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC)(unsigned variable, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLRESIZEBUFFERSMESAPROC)(void);

typedef void __stdcall (*PFNGLTBUFFERMASK3DFXPROC)(unsigned mask);

typedef void __stdcall (*PFNGLSAMPLEMASKEXTPROC)(float Value, TGLboolean invert);

typedef void __stdcall (*PFNGLSAMPLEPATTERNEXTPROC)(unsigned pattern);

typedef void __stdcall (*PFNGLTEXTURECOLORMASKSGISPROC)(TGLboolean red, TGLboolean green, TGLboolean blue, TGLboolean alpha);

typedef void __stdcall (*PFNGLGENFENCESNVPROC)(TGLsizei n, PGLuint fences);

typedef void __stdcall (*PFNGLDELETEFENCESNVPROC)(TGLsizei n, PGLuint fences);

typedef void __stdcall (*PFNGLSETFENCENVPROC)(unsigned fence, unsigned condition);

typedef TGLboolean __stdcall (*PFNGLTESTFENCENVPROC)(unsigned fence);

typedef void __stdcall (*PFNGLFINISHFENCENVPROC)(unsigned fence);

typedef TGLboolean __stdcall (*PFNGLISFENCENVPROC)(unsigned fence);

typedef void __stdcall (*PFNGLGETFENCEIVNVPROC)(unsigned fence, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLAREPROGRAMSRESIDENTNVPROC)(TGLsizei n, PGLuint programs, PGLboolean residences);

typedef void __stdcall (*PFNGLBINDPROGRAMNVPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLDELETEPROGRAMSNVPROC)(TGLsizei n, PGLuint programs);

typedef void __stdcall (*PFNGLEXECUTEPROGRAMNVPROC)(unsigned target, unsigned id, PGLfloat params);

typedef void __stdcall (*PFNGLGENPROGRAMSNVPROC)(TGLsizei n, PGLuint programs);

typedef void __stdcall (*PFNGLGETPROGRAMPARAMETERDVNVPROC)(unsigned target, unsigned index, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLGETPROGRAMPARAMETERFVNVPROC)(unsigned target, unsigned index, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETPROGRAMIVNVPROC)(unsigned id, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETPROGRAMSTRINGNVPROC)(unsigned id, unsigned pname, PGLubyte programIdx);

typedef void __stdcall (*PFNGLGETTRACKMATRIXIVNVPROC)(unsigned target, unsigned address, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVNVPROC)(unsigned index, unsigned pname, PGLdouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVNVPROC)(unsigned index, unsigned pname, PGLfloat params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVNVPROC)(unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVNVPROC)(unsigned index, unsigned pname, PGLPointer pointer);

typedef TGLboolean __stdcall (*PFNGLISPROGRAMNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLLOADPROGRAMNVPROC)(unsigned target, unsigned id, TGLsizei len, PGLubyte programIdx);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4DNVPROC)(unsigned target, unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4DVNVPROC)(unsigned target, unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4FNVPROC)(unsigned target, unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4FVNVPROC)(unsigned target, unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERS4DVNVPROC)(unsigned target, unsigned index, TGLsizei count, PGLdouble v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERS4FVNVPROC)(unsigned target, unsigned index, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLREQUESTRESIDENTPROGRAMSNVPROC)(TGLsizei n, PGLuint programs);

typedef void __stdcall (*PFNGLTRACKMATRIXNVPROC)(unsigned target, unsigned address, unsigned matrix, unsigned transform);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERNVPROC)(unsigned index, TGLint fsize, unsigned vertextype, TGLsizei stride, void * pointer);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DNVPROC)(unsigned index, TGLdouble x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVNVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FNVPROC)(unsigned index, TGLfloat x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVNVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SNVPROC)(unsigned index, TGLshort x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVNVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DNVPROC)(unsigned index, TGLdouble x, TGLdouble y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVNVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FNVPROC)(unsigned index, TGLfloat x, TGLfloat y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVNVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SNVPROC)(unsigned index, TGLshort x, TGLshort y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVNVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DNVPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVNVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FNVPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVNVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SNVPROC)(unsigned index, TGLshort x, TGLshort y, TGLshort z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVNVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DNVPROC)(unsigned index, TGLdouble x, TGLdouble y, TGLdouble z, TGLdouble w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVNVPROC)(unsigned index, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FNVPROC)(unsigned index, TGLfloat x, TGLfloat y, TGLfloat z, TGLfloat w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVNVPROC)(unsigned index, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SNVPROC)(unsigned index, TGLshort x, TGLshort y, TGLdouble z, TGLshort w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVNVPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVNVPROC)(unsigned index, PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1DVNVPROC)(unsigned index, TGLsizei count, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1FVNVPROC)(unsigned index, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1SVNVPROC)(unsigned index, TGLsizei count, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2DVNVPROC)(unsigned index, TGLsizei count, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2FVNVPROC)(unsigned index, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2SVNVPROC)(unsigned index, TGLsizei count, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3DVNVPROC)(unsigned index, TGLsizei count, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3FVNVPROC)(unsigned index, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3SVNVPROC)(unsigned index, TGLsizei count, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4DVNVPROC)(unsigned index, TGLsizei count, PGLdouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4FVNVPROC)(unsigned index, TGLsizei count, PGLfloat v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4SVNVPROC)(unsigned index, TGLsizei count, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4UBVNVPROC)(unsigned index, TGLsizei count, PGLubyte v);

typedef void __stdcall (*PFNGLGENOCCLUSIONQUERIESNVPROC)(TGLsizei n, PGLuint ids);

typedef void __stdcall (*PFNGLDELETEOCCLUSIONQUERIESNVPROC)(TGLsizei n, const PGLuint ids);

typedef TGLboolean __stdcall (*PFNGLISOCCLUSIONQUERYNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINOCCLUSIONQUERYNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLENDOCCLUSIONQUERYNVPROC)(void);

typedef void __stdcall (*PFNGLGETOCCLUSIONQUERYIVNVPROC)(unsigned id, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETOCCLUSIONQUERYUIVNVPROC)(unsigned id, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLPOINTPARAMETERINVPROC)(unsigned pname, TGLint param);

typedef void __stdcall (*PFNGLPOINTPARAMETERIVNVPROC)(unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLACTIVESTENCILFACEEXTPROC)(unsigned face);

typedef void __stdcall (*PFNGLDRAWBUFFERSATIPROC)(TGLsizei n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTNVPROC)(void);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTINDEXNVPROC)(unsigned index);

typedef void __stdcall (*PFNGLDEPTHBOUNDSEXTPROC)(TGLclampd zmin, TGLclampd zmax);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEEXTPROC)(unsigned modeRGB, unsigned modeAlpha);

typedef TGLboolean __stdcall (*PFNGLISRENDERBUFFEREXTPROC)(unsigned renderbuffer);

typedef void __stdcall (*PFNGLBINDRENDERBUFFEREXTPROC)(unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLDELETERENDERBUFFERSEXTPROC)(TGLsizei n, PGLuint renderbuffers);

typedef void __stdcall (*PFNGLGENRENDERBUFFERSEXTPROC)(TGLsizei n, PGLuint renderbuffers);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEEXTPROC)(unsigned target, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC)(unsigned target, unsigned pname, PGLint params);

typedef TGLboolean __stdcall (*PFNGLISFRAMEBUFFEREXTPROC)(unsigned framebuffer);

typedef void __stdcall (*PFNGLBINDFRAMEBUFFEREXTPROC)(unsigned target, unsigned framebuffer);

typedef void __stdcall (*PFNGLDELETEFRAMEBUFFERSEXTPROC)(TGLsizei n, PGLuint framebuffers);

typedef void __stdcall (*PFNGLGENFRAMEBUFFERSEXTPROC)(TGLsizei n, PGLuint framebuffers);

typedef unsigned __stdcall (*PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC)(unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE1DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE2DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE3DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, TGLint level, TGLint zoffset);

typedef void __stdcall (*PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC)(unsigned target, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC)(unsigned target, unsigned attachment, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGENERATEMIPMAPEXTPROC)(unsigned target);

typedef void __stdcall (*PFNGLSTRINGMARKERGREMEDYPROC)(TGLsizei len, char * str);

typedef void __stdcall (*PFNGLSTENCILCLEARTAGEXTPROC)(TGLsizei stencilTagBits, unsigned stencilClearTag);

typedef void __stdcall (*PFNGLBLITFRAMEBUFFEREXTPROC)(TGLint srcX0, TGLint srcY0, TGLint srcX1, TGLint srcY1, TGLint dstX0, TGLint dstY0, TGLint dstX1, TGLint dstY1, TGLbitfield mask, unsigned filter);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC)(unsigned target, TGLsizei samples, unsigned internalformat, TGLsizei width, TGLsizei height);

typedef void __stdcall (*PFNGLGETQUERYOBJECTI64VEXTPROC)(unsigned id, unsigned pname, PGLint64EXT params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUI64VEXTPROC)(unsigned id, unsigned pname, PGLuint64EXT params);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETERS4FVEXTPROC)(unsigned target, unsigned index, TGLsizei count, const PGLfloat params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC)(unsigned target, unsigned index, TGLsizei count, const PGLfloat params);

typedef void __stdcall (*PFNGLPROGRAMVERTEXLIMITNVPROC)(unsigned target, TGLint limit);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIEXTPROC)(unsigned _program, unsigned pname, TGLint value);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREEXTPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level, TGLint layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC)(unsigned target, unsigned attachment, unsigned texture, TGLint level, unsigned face);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IEXTPROC)(unsigned index, TGLint x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IEXTPROC)(unsigned index, TGLint x, TGLint y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IEXTPROC)(unsigned index, TGLint x, TGLint y, TGLint z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IEXTPROC)(unsigned index, TGLint x, TGLint y, TGLint z, TGLint w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIEXTPROC)(unsigned index, unsigned x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIEXTPROC)(unsigned index, unsigned x, unsigned y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIEXTPROC)(unsigned index, unsigned x, unsigned y, unsigned z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIEXTPROC)(unsigned index, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IVEXTPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IVEXTPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IVEXTPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IVEXTPROC)(unsigned index, PGLint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIVEXTPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIVEXTPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIVEXTPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIVEXTPROC)(unsigned index, PGLuint v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4BVEXTPROC)(unsigned index, PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4SVEXTPROC)(unsigned index, PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UBVEXTPROC)(unsigned index, PGLubyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4USVEXTPROC)(unsigned index, PGLushort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBIPOINTEREXTPROC)(unsigned index, TGLint size, unsigned _type, TGLsizei stride, void * _pointer);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIIVEXTPROC)(unsigned index, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIUIVEXTPROC)(unsigned index, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLUNIFORM1UIEXTPROC)(TGLint location, unsigned v0);

typedef void __stdcall (*PFNGLUNIFORM2UIEXTPROC)(TGLint location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLUNIFORM3UIEXTPROC)(TGLint location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLUNIFORM4UIEXTPROC)(TGLint location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLUNIFORM1UIVEXTPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM2UIVEXTPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM3UIVEXTPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLUNIFORM4UIVEXTPROC)(TGLint location, TGLsizei count, PGLuint value);

typedef void __stdcall (*PFNGLGETUNIFORMUIVEXTPROC)(unsigned _program, TGLint location, PGLuint params);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONEXTPROC)(unsigned _program, unsigned colorNumber, char * name);

typedef TGLint __stdcall (*PFNGLGETFRAGDATALOCATIONEXTPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDEXTPROC)(unsigned mode, TGLint first, TGLsizei count, TGLsizei primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDEXTPROC)(unsigned mode, TGLsizei count, unsigned _type, PGLvoid indices, TGLsizei primcount);

typedef void __stdcall (*PFNGLTEXBUFFEREXTPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLCOLORMASKINDEXEDEXTPROC)(unsigned buf, TGLboolean r, TGLboolean g, TGLboolean b, TGLboolean a);

typedef void __stdcall (*PFNGLGETBOOLEANINDEXEDVEXTPROC)(unsigned value, unsigned index, PGLboolean data);

typedef void __stdcall (*PFNGLGETINTEGERINDEXEDVEXTPROC)(unsigned value, unsigned index, PGLint data);

typedef void __stdcall (*PFNGLENABLEINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLDISABLEINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef TGLboolean __stdcall (*PFNGLISENABLEDINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLBINDBUFFERRANGENVPROC)(unsigned target, unsigned index, unsigned buffer, TGLintptr offset, TGLsizeiptr size);

typedef void __stdcall (*PFNGLBINDBUFFEROFFSETNVPROC)(unsigned target, unsigned index, unsigned buffer, TGLintptr offset);

typedef void __stdcall (*PFNGLBINDBUFFERBASENVPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC)(TGLsizei count, PGLint attribs, unsigned bufferMode);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC)(unsigned _program, TGLsizei count, PGLint locations, unsigned bufferMode);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKNVPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKNVPROC)(void);

typedef TGLint __stdcall (*PFNGLGETVARYINGLOCATIONNVPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLGETACTIVEVARYINGNVPROC)(unsigned _program, unsigned index, TGLsizei bufSize, PGLsizei length, PGLsizei size, unsigned _type, char * name);

typedef void __stdcall (*PFNGLACTIVEVARYINGNVPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC)(unsigned _program, unsigned index, PGLint location);

typedef void __stdcall (*PFNGLUNIFORMBUFFEREXTPROC)(unsigned _program, TGLint location, unsigned buffer);

typedef TGLint __stdcall (*PFNGLGETUNIFORMBUFFERSIZEEXTPROC)(unsigned _program, TGLint location);

typedef PGLint __stdcall (*PFNGLGETUNIFORMOFFSETEXTPROC)(unsigned _program, TGLint location);

typedef void __stdcall (*PFNGLCLEARCOLORIIEXTPROC)(TGLint r, TGLint g, TGLint b, TGLint a);

typedef void __stdcall (*PFNGLCLEARCOLORIUIEXTPROC)(unsigned r, unsigned g, unsigned b, unsigned a);

typedef void __stdcall (*PFNGLTEXPARAMETERIIVEXTPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLTEXPARAMETERIUIVEXTPROC)(unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIIVEXTPROC)(unsigned target, unsigned pname, PGLint params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIUIVEXTPROC)(unsigned target, unsigned pname, PGLuint params);

typedef void __stdcall (*PFNGLFRAMETERMINATORGREMEDYPROC)(void);

typedef void __stdcall (*PFNGLBEGINCONDITIONALRENDERNVPROC)(unsigned id, unsigned mode);

typedef void __stdcall (*PFNGLENDCONDITIONALRENDERNVPROC)(void);

typedef void __stdcall (*PFNGLBINDBUFFERRANGEEXTPROC)(unsigned target, unsigned index, unsigned buffer, TGLintptr offset, TGLsizeiptr size);

typedef void __stdcall (*PFNGLBINDBUFFEROFFSETEXTPROC)(unsigned target, unsigned index, unsigned buffer, TGLintptr offset);

typedef void __stdcall (*PFNGLBINDBUFFERBASEEXTPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKEXTPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKEXTPROC)(void);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC)(unsigned _program, TGLsizei count, const PGLPCharArray varyings, unsigned bufferMode);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC)(unsigned _program, unsigned index, TGLsizei bufSize, PGLsizei length, PGLsizei size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLTESSELLATIONFACTORAMDPROC)(TGLfloat factor);

typedef void __stdcall (*PFNGLTESSELLATIONMODEAMDPROC)(unsigned mode);

typedef void __stdcall (*PFNGLCOPYIMAGESUBDATANVPROC)(unsigned srcName, unsigned srcTarget, TGLint srcLevel, TGLint srcX, TGLint srcY, TGLint srcZ, unsigned dstName, unsigned dstTarget, TGLint dstLevel, TGLint dstX, TGLint dstY, TGLint dstZ, TGLsizei width, TGLsizei height, TGLsizei depth);

typedef void __stdcall (*PFNGLMAKEBUFFERRESIDENTNVPROC)(unsigned target, unsigned access);

typedef void __stdcall (*PFNGLMAKEBUFFERNONRESIDENTNVPROC)(unsigned target);

typedef TGLboolean __stdcall (*PFNGLISBUFFERRESIDENTNVPROC)(unsigned target);

typedef void __stdcall (*PFNGLMAKENAMEDBUFFERRESIDENTNVPROC)(unsigned buffer, unsigned access);

typedef void __stdcall (*PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC)(unsigned buffer);

typedef TGLboolean __stdcall (*PFNGLISNAMEDBUFFERRESIDENTNVPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERUI64VNVPROC)(unsigned target, unsigned pname, PGLuint64EXT params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC)(unsigned buffer, unsigned pname, PGLuint64EXT params);

typedef void __stdcall (*PFNGLGETINTEGERUI64VNVPROC)(unsigned value, PGLuint64EXT result);

typedef void __stdcall (*PFNGLUNIFORMUI64NVPROC)(TGLint location, TGLuint64EXT value);

typedef void __stdcall (*PFNGLUNIFORMUI64VNVPROC)(TGLint location, TGLsizei count, const PGLuint64EXT value);

typedef void __stdcall (*PFNGLGETUNIFORMUI64VNVPROC)(unsigned _program, TGLint location, PGLuint64EXT params);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMUI64NVPROC)(unsigned _program, TGLint location, TGLuint64EXT value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMUI64VNVPROC)(unsigned _program, TGLint location, TGLsizei count, const PGLuint64EXT value);

typedef void __stdcall (*PFNGLBUFFERADDRESSRANGENVPROC)(unsigned pname, unsigned index, TGLuint64EXT address, TGLsizeiptr length);

typedef void __stdcall (*PFNGLVERTEXFORMATNVPROC)(TGLint size, unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLNORMALFORMATNVPROC)(unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLCOLORFORMATNVPROC)(TGLint size, unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLINDEXFORMATNVPROC)(unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLTEXCOORDFORMATNVPROC)(TGLint size, unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLEDGEFLAGFORMATNVPROC)(TGLsizei stride);

typedef void __stdcall (*PFNGLSECONDARYCOLORFORMATNVPROC)(TGLint size, unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLFOGCOORDFORMATNVPROC)(unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLVERTEXATTRIBFORMATNVPROC)(unsigned index, TGLint size, unsigned _type, TGLboolean normalized, TGLsizei stride);

typedef void __stdcall (*PFNGLVERTEXATTRIBIFORMATNVPROC)(unsigned index, TGLint size, unsigned _type, TGLsizei stride);

typedef void __stdcall (*PFNGLGETINTEGERUI64I_VNVPROC)(unsigned value, unsigned index, PGLuint64EXT result);

typedef void __stdcall (*PGNGLGETBUFFERPARAMETERUI64VNV)(unsigned value, unsigned index, PGLuint64EXT result);

typedef unsigned __stdcall (*PFNGLGENPATHSNVPROC)(TGLsizei range);

typedef void __stdcall (*PFNGLDELETEPATHSNVPROC)(unsigned path, TGLsizei range);

typedef TGLboolean __stdcall (*PFNGLISPATHNVPROC)(unsigned path);

typedef void __stdcall (*PFNGLPATHCOMMANDSNVPROC)(unsigned path, TGLsizei numCommands, PGLubyte commands, TGLsizei numCoords, unsigned coordType, PGLvoid coords);

typedef void __stdcall (*PFNGLPATHCOORDSNVPROC)(unsigned path, TGLsizei numCoords, unsigned coordType, PGLvoid coords);

typedef void __stdcall (*PFNGLPATHSUBCOMMANDSNVPROC)(unsigned path, TGLsizei commandStart, TGLsizei commandsToDelete, TGLsizei numCommands, PGLubyte commands, TGLsizei numCoords, unsigned coordType, PGLvoid coords);

typedef void __stdcall (*PFNGLPATHSUBCOORDSNVPROC)(unsigned path, TGLsizei coordStart, TGLsizei numCoords, unsigned coordType, PGLvoid coords);

typedef void __stdcall (*PFNGLPATHSTRINGNVPROC)(unsigned path, unsigned format, TGLsizei length, PGLvoid pathString);

typedef void __stdcall (*PFNGLPATHGLYPHSNVPROC)(unsigned firstPathName, unsigned fontTarget, PGLvoid fontName, TGLbitfield fontStyle, TGLsizei numGlyphs, unsigned _type, PGLvoid charcodes, unsigned handleMissingGlyphs, unsigned pathParameterTemplate, TGLfloat emScale);

typedef void __stdcall (*PFNGLPATHGLYPHRANGENVPROC)(unsigned firstPathName, unsigned fontTarget, char * fontName, TGLbitfield fontStyle, unsigned firstGlyph, TGLsizei numGlyphs, unsigned handleMissingGlyphs, unsigned pathParameterTemplate, TGLfloat emScale);

typedef void __stdcall (*PFNGLWEIGHTPATHSNVPROC)(unsigned resultPath, TGLsizei numPaths, PGLuint paths, PGLfloat weights);

typedef void __stdcall (*PFNGLCOPYPATHNVPROC)(unsigned resultPath, unsigned srcPath);

typedef void __stdcall (*PFNGLINTERPOLATEPATHSNVPROC)(unsigned resultPath, unsigned pathA, unsigned pathB, TGLfloat weight);

typedef void __stdcall (*PFNGLTRANSFORMPATHNVPROC)(unsigned resultPath, unsigned srcPath, unsigned transformType, PGLfloat transformValues);

typedef void __stdcall (*PFNGLPATHPARAMETERIVNVPROC)(unsigned path, unsigned pname, PGLint value);

typedef void __stdcall (*PFNGLPATHPARAMETERINVPROC)(unsigned path, unsigned pname, TGLint value);

typedef void __stdcall (*PFNGLPATHPARAMETERFVNVPROC)(unsigned path, unsigned pname, PGLfloat value);

typedef void __stdcall (*PFNGLPATHPARAMETERFNVPROC)(unsigned path, unsigned pname, TGLfloat value);

typedef void __stdcall (*PFNGLPATHDASHARRAYNVPROC)(unsigned path, TGLsizei dashCount, PGLfloat dashArray);

typedef void __stdcall (*PFNGLPATHSTENCILFUNCNVPROC)(unsigned func, TGLint ref, unsigned mask);

typedef void __stdcall (*PFNGLPATHSTENCILDEPTHOFFSETNVPROC)(TGLfloat factor, TGLfloat units);

typedef void __stdcall (*PFNGLSTENCILFILLPATHNVPROC)(unsigned path, unsigned fillMode, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILSTROKEPATHNVPROC)(unsigned path, TGLint reference, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILFILLPATHINSTANCEDNVPROC)(TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, unsigned fillMode, unsigned mask, unsigned transformType, PGLfloat transformValues);

typedef void __stdcall (*PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC)(TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, TGLint reference, unsigned mask, unsigned transformType, PGLfloat transformValues);

typedef void __stdcall (*PFNGLPATHCOVERDEPTHFUNCNVPROC)(unsigned func);

typedef void __stdcall (*PFNGLPATHCOLORGENNVPROC)(unsigned color, unsigned genMode, unsigned colorFormat, PGLfloat coeffs);

typedef void __stdcall (*PFNGLPATHTEXGENNVPROC)(unsigned texCoordSet, unsigned genMode, TGLint components, PGLfloat coeffs);

typedef void __stdcall (*PFNGLPATHFOGGENNVPROC)(unsigned genMode);

typedef void __stdcall (*PFNGLCOVERFILLPATHNVPROC)(unsigned path, unsigned coverMode);

typedef void __stdcall (*PFNGLCOVERSTROKEPATHNVPROC)(unsigned path, unsigned coverMode);

typedef void __stdcall (*PFNGLCOVERFILLPATHINSTANCEDNVPROC)(TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, unsigned coverMode, unsigned transformType, PGLfloat transformValues);

typedef void __stdcall (*PFNGLCOVERSTROKEPATHINSTANCEDNVPROC)(TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, unsigned coverMode, unsigned transformType, PGLfloat transformValues);

typedef void __stdcall (*PFNGLGETPATHPARAMETERIVNVPROC)(unsigned path, unsigned pname, PGLint value);

typedef void __stdcall (*PFNGLGETPATHPARAMETERFVNVPROC)(unsigned path, unsigned pname, PGLfloat value);

typedef void __stdcall (*PFNGLGETPATHCOMMANDSNVPROC)(unsigned path, PGLubyte commands);

typedef void __stdcall (*PFNGLGETPATHCOORDSNVPROC)(unsigned path, PGLfloat coords);

typedef void __stdcall (*PFNGLGETPATHDASHARRAYNVPROC)(unsigned path, PGLfloat dashArray);

typedef void __stdcall (*PFNGLGETPATHMETRICSNVPROC)(TGLbitfield metricQueryMask, TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, TGLsizei stride, PGLfloat metrics);

typedef void __stdcall (*PFNGLGETPATHMETRICRANGENVPROC)(TGLbitfield metricQueryMask, unsigned firstPathName, TGLsizei numPaths, TGLsizei stride, PGLfloat metrics);

typedef void __stdcall (*PFNGLGETPATHSPACINGNVPROC)(unsigned pathListMode, TGLsizei numPaths, unsigned pathNameType, PGLvoid paths, unsigned pathBase, TGLfloat advanceScale, TGLfloat kerningScale, unsigned transformType, PGLfloat returnedSpacing);

typedef void __stdcall (*PFNGLGETPATHCOLORGENIVNVPROC)(unsigned color, unsigned pname, PGLint value);

typedef void __stdcall (*PFNGLGETPATHCOLORGENFVNVPROC)(unsigned color, unsigned pname, PGLfloat value);

typedef void __stdcall (*PFNGLGETPATHTEXGENIVNVPROC)(unsigned texCoordSet, unsigned pname, PGLint value);

typedef void __stdcall (*PFNGLGETPATHTEXGENFVNVPROC)(unsigned texCoordSet, unsigned pname, PGLfloat value);

typedef TGLboolean __stdcall (*PFNGLISPOINTINFILLPATHNVPROC)(unsigned path, unsigned mask, TGLfloat x, TGLfloat y);

typedef TGLboolean __stdcall (*PFNGLISPOINTINSTROKEPATHNVPROC)(unsigned path, TGLfloat x, TGLfloat y);

typedef TGLfloat __stdcall (*PFNGLGETPATHLENGTHNVPROC)(unsigned path, TGLsizei startSegment, TGLsizei numSegments);

typedef TGLboolean __stdcall (*PFNGLPOINTALONGPATHNVPROC)(unsigned path, TGLsizei startSegment, TGLsizei numSegments, TGLfloat distance, PGLfloat x, PGLfloat y, PGLfloat tangentX, PGLfloat tangentY);

//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Word GL_DEPTH_BUFFER_BIT = System::Word(0x100);
static _DELPHI_CONST System::Word GL_STENCIL_BUFFER_BIT = System::Word(0x400);
static _DELPHI_CONST System::Word GL_COLOR_BUFFER_BIT = System::Word(0x4000);
static _DELPHI_CONST System::Int8 GL_FALSE = System::Int8(0x0);
static _DELPHI_CONST System::Int8 GL_TRUE = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_POINTS = System::Int8(0x0);
static _DELPHI_CONST System::Int8 GL_LINES = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_LINE_LOOP = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_LINE_STRIP = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GL_TRIANGLES = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_TRIANGLE_STRIP = System::Int8(0x5);
static _DELPHI_CONST System::Int8 GL_TRIANGLE_FAN = System::Int8(0x6);
static _DELPHI_CONST System::Word GL_NEVER = System::Word(0x200);
static _DELPHI_CONST System::Word GL_LESS = System::Word(0x201);
static _DELPHI_CONST System::Word GL_EQUAL = System::Word(0x202);
static _DELPHI_CONST System::Word GL_LEQUAL = System::Word(0x203);
static _DELPHI_CONST System::Word GL_GREATER = System::Word(0x204);
static _DELPHI_CONST System::Word GL_NOTEQUAL = System::Word(0x205);
static _DELPHI_CONST System::Word GL_GEQUAL = System::Word(0x206);
static _DELPHI_CONST System::Word GL_ALWAYS = System::Word(0x207);
static _DELPHI_CONST System::Int8 GL_ZERO = System::Int8(0x0);
static _DELPHI_CONST System::Int8 GL_ONE = System::Int8(0x1);
static _DELPHI_CONST System::Word GL_SRC_COLOR = System::Word(0x300);
static _DELPHI_CONST System::Word GL_ONE_MINUS_SRC_COLOR = System::Word(0x301);
static _DELPHI_CONST System::Word GL_SRC_ALPHA = System::Word(0x302);
static _DELPHI_CONST System::Word GL_ONE_MINUS_SRC_ALPHA = System::Word(0x303);
static _DELPHI_CONST System::Word GL_DST_ALPHA = System::Word(0x304);
static _DELPHI_CONST System::Word GL_ONE_MINUS_DST_ALPHA = System::Word(0x305);
static _DELPHI_CONST System::Word GL_DST_COLOR = System::Word(0x306);
static _DELPHI_CONST System::Word GL_ONE_MINUS_DST_COLOR = System::Word(0x307);
static _DELPHI_CONST System::Word GL_SRC_ALPHA_SATURATE = System::Word(0x308);
static _DELPHI_CONST System::Int8 GL_NONE = System::Int8(0x0);
static _DELPHI_CONST System::Word GL_FRONT_LEFT = System::Word(0x400);
static _DELPHI_CONST System::Word GL_FRONT_RIGHT = System::Word(0x401);
static _DELPHI_CONST System::Word GL_BACK_LEFT = System::Word(0x402);
static _DELPHI_CONST System::Word GL_BACK_RIGHT = System::Word(0x403);
static _DELPHI_CONST System::Word GL_FRONT = System::Word(0x404);
static _DELPHI_CONST System::Word GL_BACK = System::Word(0x405);
static _DELPHI_CONST System::Word GL_LEFT = System::Word(0x406);
static _DELPHI_CONST System::Word GL_RIGHT = System::Word(0x407);
static _DELPHI_CONST System::Word GL_FRONT_AND_BACK = System::Word(0x408);
static _DELPHI_CONST System::Int8 GL_NO_ERROR = System::Int8(0x0);
static _DELPHI_CONST System::Word GL_INVALID_ENUM = System::Word(0x500);
static _DELPHI_CONST System::Word GL_INVALID_VALUE = System::Word(0x501);
static _DELPHI_CONST System::Word GL_INVALID_OPERATION = System::Word(0x502);
static _DELPHI_CONST System::Word GL_OUT_OF_MEMORY = System::Word(0x505);
static _DELPHI_CONST System::Word GL_CW = System::Word(0x900);
static _DELPHI_CONST System::Word GL_CCW = System::Word(0x901);
static _DELPHI_CONST System::Word GL_POINT_SIZE = System::Word(0xb11);
static _DELPHI_CONST System::Word GL_POINT_SIZE_RANGE = System::Word(0xb12);
static _DELPHI_CONST System::Word GL_POINT_SIZE_GRANULARITY = System::Word(0xb13);
static _DELPHI_CONST System::Word GL_LINE_SMOOTH = System::Word(0xb20);
static _DELPHI_CONST System::Word GL_LINE_WIDTH = System::Word(0xb21);
static _DELPHI_CONST System::Word GL_LINE_WIDTH_RANGE = System::Word(0xb22);
static _DELPHI_CONST System::Word GL_LINE_WIDTH_GRANULARITY = System::Word(0xb23);
static _DELPHI_CONST System::Word GL_POLYGON_SMOOTH = System::Word(0xb41);
static _DELPHI_CONST System::Word GL_CULL_FACE = System::Word(0xb44);
static _DELPHI_CONST System::Word GL_CULL_FACE_MODE = System::Word(0xb45);
static _DELPHI_CONST System::Word GL_FRONT_FACE = System::Word(0xb46);
static _DELPHI_CONST System::Word GL_DEPTH_RANGE = System::Word(0xb70);
static _DELPHI_CONST System::Word GL_DEPTH_TEST = System::Word(0xb71);
static _DELPHI_CONST System::Word GL_DEPTH_WRITEMASK = System::Word(0xb72);
static _DELPHI_CONST System::Word GL_DEPTH_CLEAR_VALUE = System::Word(0xb73);
static _DELPHI_CONST System::Word GL_DEPTH_FUNC = System::Word(0xb74);
static _DELPHI_CONST System::Word GL_STENCIL_TEST = System::Word(0xb90);
static _DELPHI_CONST System::Word GL_STENCIL_CLEAR_VALUE = System::Word(0xb91);
static _DELPHI_CONST System::Word GL_STENCIL_FUNC = System::Word(0xb92);
static _DELPHI_CONST System::Word GL_STENCIL_VALUE_MASK = System::Word(0xb93);
static _DELPHI_CONST System::Word GL_STENCIL_FAIL = System::Word(0xb94);
static _DELPHI_CONST System::Word GL_STENCIL_PASS_DEPTH_FAIL = System::Word(0xb95);
static _DELPHI_CONST System::Word GL_STENCIL_PASS_DEPTH_PASS = System::Word(0xb96);
static _DELPHI_CONST System::Word GL_STENCIL_REF = System::Word(0xb97);
static _DELPHI_CONST System::Word GL_STENCIL_WRITEMASK = System::Word(0xb98);
static _DELPHI_CONST System::Word GL_MATRIX_MODE = System::Word(0xba0);
static _DELPHI_CONST System::Word GL_VIEWPORT = System::Word(0xba2);
static _DELPHI_CONST System::Word GL_DITHER = System::Word(0xbd0);
static _DELPHI_CONST System::Word GL_BLEND_DST = System::Word(0xbe0);
static _DELPHI_CONST System::Word GL_BLEND_SRC = System::Word(0xbe1);
static _DELPHI_CONST System::Word GL_BLEND = System::Word(0xbe2);
static _DELPHI_CONST System::Word GL_LOGIC_OP_MODE = System::Word(0xbf0);
static _DELPHI_CONST System::Word GL_COLOR_LOGIC_OP = System::Word(0xbf2);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER = System::Word(0xc01);
static _DELPHI_CONST System::Word GL_READ_BUFFER = System::Word(0xc02);
static _DELPHI_CONST System::Word GL_SCISSOR_BOX = System::Word(0xc10);
static _DELPHI_CONST System::Word GL_SCISSOR_TEST = System::Word(0xc11);
static _DELPHI_CONST System::Word GL_COLOR_CLEAR_VALUE = System::Word(0xc22);
static _DELPHI_CONST System::Word GL_COLOR_WRITEMASK = System::Word(0xc23);
static _DELPHI_CONST System::Word GL_DOUBLEBUFFER = System::Word(0xc32);
static _DELPHI_CONST System::Word GL_STEREO = System::Word(0xc33);
static _DELPHI_CONST System::Word GL_LINE_SMOOTH_HINT = System::Word(0xc52);
static _DELPHI_CONST System::Word GL_POLYGON_SMOOTH_HINT = System::Word(0xc53);
static _DELPHI_CONST System::Word GL_UNPACK_SWAP_BYTES = System::Word(0xcf0);
static _DELPHI_CONST System::Word GL_UNPACK_LSB_FIRST = System::Word(0xcf1);
static _DELPHI_CONST System::Word GL_UNPACK_ROW_LENGTH = System::Word(0xcf2);
static _DELPHI_CONST System::Word GL_UNPACK_SKIP_ROWS = System::Word(0xcf3);
static _DELPHI_CONST System::Word GL_UNPACK_SKIP_PIXELS = System::Word(0xcf4);
static _DELPHI_CONST System::Word GL_UNPACK_ALIGNMENT = System::Word(0xcf5);
static _DELPHI_CONST System::Word GL_PACK_SWAP_BYTES = System::Word(0xd00);
static _DELPHI_CONST System::Word GL_PACK_LSB_FIRST = System::Word(0xd01);
static _DELPHI_CONST System::Word GL_PACK_ROW_LENGTH = System::Word(0xd02);
static _DELPHI_CONST System::Word GL_PACK_SKIP_ROWS = System::Word(0xd03);
static _DELPHI_CONST System::Word GL_PACK_SKIP_PIXELS = System::Word(0xd04);
static _DELPHI_CONST System::Word GL_PACK_ALIGNMENT = System::Word(0xd05);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_SIZE = System::Word(0xd33);
static _DELPHI_CONST System::Word GL_MAX_VIEWPORT_DIMS = System::Word(0xd3a);
static _DELPHI_CONST System::Word GL_SUBPIXEL_BITS = System::Word(0xd50);
static _DELPHI_CONST System::Word GL_TEXTURE_1D = System::Word(0xde0);
static _DELPHI_CONST System::Word GL_TEXTURE_2D = System::Word(0xde1);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_UNITS = System::Word(0x2a00);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_POINT = System::Word(0x2a01);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_LINE = System::Word(0x2a02);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_FILL = System::Word(0x8037);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_FACTOR = System::Word(0x8038);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_1D = System::Word(0x8068);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_2D = System::Word(0x8069);
static _DELPHI_CONST System::Word GL_TEXTURE_WIDTH = System::Word(0x1000);
static _DELPHI_CONST System::Word GL_TEXTURE_HEIGHT = System::Word(0x1001);
static _DELPHI_CONST System::Word GL_TEXTURE_INTERNAL_FORMAT = System::Word(0x1003);
static _DELPHI_CONST System::Word GL_TEXTURE_BORDER_COLOR = System::Word(0x1004);
static _DELPHI_CONST System::Word GL_TEXTURE_BORDER = System::Word(0x1005);
static _DELPHI_CONST System::Word GL_TEXTURE_RED_SIZE = System::Word(0x805c);
static _DELPHI_CONST System::Word GL_TEXTURE_GREEN_SIZE = System::Word(0x805d);
static _DELPHI_CONST System::Word GL_TEXTURE_BLUE_SIZE = System::Word(0x805e);
static _DELPHI_CONST System::Word GL_TEXTURE_ALPHA_SIZE = System::Word(0x805f);
static _DELPHI_CONST System::Word GL_DONT_CARE = System::Word(0x1100);
static _DELPHI_CONST System::Word GL_FASTEST = System::Word(0x1101);
static _DELPHI_CONST System::Word GL_NICEST = System::Word(0x1102);
static _DELPHI_CONST System::Word GL_BYTE = System::Word(0x1400);
static _DELPHI_CONST System::Word GL_UNSIGNED_BYTE = System::Word(0x1401);
static _DELPHI_CONST System::Word GL_SHORT = System::Word(0x1402);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT = System::Word(0x1403);
static _DELPHI_CONST System::Word GL_INT = System::Word(0x1404);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT = System::Word(0x1405);
static _DELPHI_CONST System::Word GL_FLOAT = System::Word(0x1406);
static _DELPHI_CONST System::Word GL_DOUBLE = System::Word(0x140a);
static _DELPHI_CONST System::Word GL_CLEAR = System::Word(0x1500);
static _DELPHI_CONST System::Word GL_AND = System::Word(0x1501);
static _DELPHI_CONST System::Word GL_AND_REVERSE = System::Word(0x1502);
static _DELPHI_CONST System::Word GL_COPY = System::Word(0x1503);
static _DELPHI_CONST System::Word GL_AND_INVERTED = System::Word(0x1504);
static _DELPHI_CONST System::Word GL_NOOP = System::Word(0x1505);
static _DELPHI_CONST System::Word GL_XOR = System::Word(0x1506);
static _DELPHI_CONST System::Word GL_OR = System::Word(0x1507);
static _DELPHI_CONST System::Word GL_NOR = System::Word(0x1508);
static _DELPHI_CONST System::Word GL_EQUIV = System::Word(0x1509);
static _DELPHI_CONST System::Word GL_INVERT = System::Word(0x150a);
static _DELPHI_CONST System::Word GL_OR_REVERSE = System::Word(0x150b);
static _DELPHI_CONST System::Word GL_COPY_INVERTED = System::Word(0x150c);
static _DELPHI_CONST System::Word GL_OR_INVERTED = System::Word(0x150d);
static _DELPHI_CONST System::Word GL_NAND = System::Word(0x150e);
static _DELPHI_CONST System::Word GL_SET = System::Word(0x150f);
static _DELPHI_CONST System::Word GL_TEXTURE = System::Word(0x1702);
static _DELPHI_CONST System::Word GL_COLOR = System::Word(0x1800);
static _DELPHI_CONST System::Word GL_DEPTH = System::Word(0x1801);
static _DELPHI_CONST System::Word GL_STENCIL = System::Word(0x1802);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX = System::Word(0x1901);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT = System::Word(0x1902);
static _DELPHI_CONST System::Word GL_RED = System::Word(0x1903);
static _DELPHI_CONST System::Word GL_GREEN = System::Word(0x1904);
static _DELPHI_CONST System::Word GL_BLUE = System::Word(0x1905);
static _DELPHI_CONST System::Word GL_ALPHA = System::Word(0x1906);
static _DELPHI_CONST System::Word GL_RGB = System::Word(0x1907);
static _DELPHI_CONST System::Word GL_RGBA = System::Word(0x1908);
static _DELPHI_CONST System::Word GL_POINT = System::Word(0x1b00);
static _DELPHI_CONST System::Word GL_LINE = System::Word(0x1b01);
static _DELPHI_CONST System::Word GL_FILL = System::Word(0x1b02);
static _DELPHI_CONST System::Word GL_KEEP = System::Word(0x1e00);
static _DELPHI_CONST System::Word GL_REPLACE = System::Word(0x1e01);
static _DELPHI_CONST System::Word GL_INCR = System::Word(0x1e02);
static _DELPHI_CONST System::Word GL_DECR = System::Word(0x1e03);
static _DELPHI_CONST System::Word GL_VENDOR = System::Word(0x1f00);
static _DELPHI_CONST System::Word GL_RENDERER = System::Word(0x1f01);
static _DELPHI_CONST System::Word GL_VERSION = System::Word(0x1f02);
static _DELPHI_CONST System::Word GL_EXTENSIONS = System::Word(0x1f03);
static _DELPHI_CONST System::Word GL_NEAREST = System::Word(0x2600);
static _DELPHI_CONST System::Word GL_LINEAR = System::Word(0x2601);
static _DELPHI_CONST System::Word GL_NEAREST_MIPMAP_NEAREST = System::Word(0x2700);
static _DELPHI_CONST System::Word GL_LINEAR_MIPMAP_NEAREST = System::Word(0x2701);
static _DELPHI_CONST System::Word GL_NEAREST_MIPMAP_LINEAR = System::Word(0x2702);
static _DELPHI_CONST System::Word GL_LINEAR_MIPMAP_LINEAR = System::Word(0x2703);
static _DELPHI_CONST System::Word GL_TEXTURE_MAG_FILTER = System::Word(0x2800);
static _DELPHI_CONST System::Word GL_TEXTURE_MIN_FILTER = System::Word(0x2801);
static _DELPHI_CONST System::Word GL_TEXTURE_WRAP_S = System::Word(0x2802);
static _DELPHI_CONST System::Word GL_TEXTURE_WRAP_T = System::Word(0x2803);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_1D = System::Word(0x8063);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D = System::Word(0x8064);
static _DELPHI_CONST System::Word GL_REPEAT = System::Word(0x2901);
static _DELPHI_CONST System::Word GL_R3_G3_B2 = System::Word(0x2a10);
static _DELPHI_CONST System::Word GL_RGB4 = System::Word(0x804f);
static _DELPHI_CONST System::Word GL_RGB5 = System::Word(0x8050);
static _DELPHI_CONST System::Word GL_RGB8 = System::Word(0x8051);
static _DELPHI_CONST System::Word GL_RGB10 = System::Word(0x8052);
static _DELPHI_CONST System::Word GL_RGB12 = System::Word(0x8053);
static _DELPHI_CONST System::Word GL_RGB16 = System::Word(0x8054);
static _DELPHI_CONST System::Word GL_RGBA2 = System::Word(0x8055);
static _DELPHI_CONST System::Word GL_RGBA4 = System::Word(0x8056);
static _DELPHI_CONST System::Word GL_RGB5_A1 = System::Word(0x8057);
static _DELPHI_CONST System::Word GL_RGBA8 = System::Word(0x8058);
static _DELPHI_CONST System::Word GL_RGB10_A2 = System::Word(0x8059);
static _DELPHI_CONST System::Word GL_RGBA12 = System::Word(0x805a);
static _DELPHI_CONST System::Word GL_RGBA16 = System::Word(0x805b);
static _DELPHI_CONST System::Int8 GL_CURRENT_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_POINT_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_LINE_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_POLYGON_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_POLYGON_STIPPLE_BIT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GL_PIXEL_MODE_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Int8 GL_LIGHTING_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte GL_FOG_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Word GL_ACCUM_BUFFER_BIT = System::Word(0x200);
static _DELPHI_CONST System::Word GL_VIEWPORT_BIT = System::Word(0x800);
static _DELPHI_CONST System::Word GL_TRANSFORM_BIT = System::Word(0x1000);
static _DELPHI_CONST System::Word GL_ENABLE_BIT = System::Word(0x2000);
static _DELPHI_CONST System::Word GL_HINT_BIT = System::Word(0x8000);
static _DELPHI_CONST int GL_EVAL_BIT = int(0x10000);
static _DELPHI_CONST int GL_LIST_BIT = int(0x20000);
static _DELPHI_CONST int GL_TEXTURE_BIT = int(0x40000);
static _DELPHI_CONST int GL_SCISSOR_BIT = int(0x80000);
static _DELPHI_CONST unsigned GL_ALL_ATTRIB_BITS = unsigned(0xffffffff);
static _DELPHI_CONST System::Int8 GL_CLIENT_PIXEL_STORE_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_CLIENT_VERTEX_ARRAY_BIT = System::Int8(0x2);
static _DELPHI_CONST unsigned GL_CLIENT_ALL_ATTRIB_BITS = unsigned(0xffffffff);
static _DELPHI_CONST System::Int8 GL_QUADS = System::Int8(0x7);
static _DELPHI_CONST System::Int8 GL_QUAD_STRIP = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_POLYGON = System::Int8(0x9);
static _DELPHI_CONST System::Word GL_ACCUM = System::Word(0x100);
static _DELPHI_CONST System::Word GL_LOAD = System::Word(0x101);
static _DELPHI_CONST System::Word GL_RETURN = System::Word(0x102);
static _DELPHI_CONST System::Word GL_MULT = System::Word(0x103);
static _DELPHI_CONST System::Word GL_ADD = System::Word(0x104);
static _DELPHI_CONST System::Word GL_AUX0 = System::Word(0x409);
static _DELPHI_CONST System::Word GL_AUX1 = System::Word(0x40a);
static _DELPHI_CONST System::Word GL_AUX2 = System::Word(0x40b);
static _DELPHI_CONST System::Word GL_AUX3 = System::Word(0x40c);
static _DELPHI_CONST System::Word GL_STACK_OVERFLOW = System::Word(0x503);
static _DELPHI_CONST System::Word GL_STACK_UNDERFLOW = System::Word(0x504);
static _DELPHI_CONST System::Word GL_2D = System::Word(0x600);
static _DELPHI_CONST System::Word GL_3D = System::Word(0x601);
static _DELPHI_CONST System::Word GL_3D_COLOR = System::Word(0x602);
static _DELPHI_CONST System::Word GL_3D_COLOR_TEXTURE = System::Word(0x603);
static _DELPHI_CONST System::Word GL_4D_COLOR_TEXTURE = System::Word(0x604);
static _DELPHI_CONST System::Word GL_PASS_THROUGH_TOKEN = System::Word(0x700);
static _DELPHI_CONST System::Word GL_POINT_TOKEN = System::Word(0x701);
static _DELPHI_CONST System::Word GL_LINE_TOKEN = System::Word(0x702);
static _DELPHI_CONST System::Word GL_POLYGON_TOKEN = System::Word(0x703);
static _DELPHI_CONST System::Word GL_BITMAP_TOKEN = System::Word(0x704);
static _DELPHI_CONST System::Word GL_DRAW_PIXEL_TOKEN = System::Word(0x705);
static _DELPHI_CONST System::Word GL_COPY_PIXEL_TOKEN = System::Word(0x706);
static _DELPHI_CONST System::Word GL_LINE_RESET_TOKEN = System::Word(0x707);
static _DELPHI_CONST System::Word GL_EXP = System::Word(0x800);
static _DELPHI_CONST System::Word GL_EXP2 = System::Word(0x801);
static _DELPHI_CONST System::Word GL_COEFF = System::Word(0xa00);
static _DELPHI_CONST System::Word GL_ORDER = System::Word(0xa01);
static _DELPHI_CONST System::Word GL_DOMAIN = System::Word(0xa02);
static _DELPHI_CONST System::Word GL_CURRENT_COLOR = System::Word(0xb00);
static _DELPHI_CONST System::Word GL_CURRENT_INDEX = System::Word(0xb01);
static _DELPHI_CONST System::Word GL_CURRENT_NORMAL = System::Word(0xb02);
static _DELPHI_CONST System::Word GL_CURRENT_TEXTURE_COORDS = System::Word(0xb03);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_COLOR = System::Word(0xb04);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_INDEX = System::Word(0xb05);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_TEXTURE_COORDS = System::Word(0xb06);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_POSITION = System::Word(0xb07);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_POSITION_VALID = System::Word(0xb08);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_DISTANCE = System::Word(0xb09);
static _DELPHI_CONST System::Word GL_POINT_SMOOTH = System::Word(0xb10);
static _DELPHI_CONST System::Word GL_LINE_STIPPLE = System::Word(0xb24);
static _DELPHI_CONST System::Word GL_LINE_STIPPLE_PATTERN = System::Word(0xb25);
static _DELPHI_CONST System::Word GL_LINE_STIPPLE_REPEAT = System::Word(0xb26);
static _DELPHI_CONST System::Word GL_LIST_MODE = System::Word(0xb30);
static _DELPHI_CONST System::Word GL_MAX_LIST_NESTING = System::Word(0xb31);
static _DELPHI_CONST System::Word GL_LIST_BASE = System::Word(0xb32);
static _DELPHI_CONST System::Word GL_LIST_INDEX = System::Word(0xb33);
static _DELPHI_CONST System::Word GL_POLYGON_MODE = System::Word(0xb40);
static _DELPHI_CONST System::Word GL_POLYGON_STIPPLE = System::Word(0xb42);
static _DELPHI_CONST System::Word GL_EDGE_FLAG = System::Word(0xb43);
static _DELPHI_CONST System::Word GL_LIGHTING = System::Word(0xb50);
static _DELPHI_CONST System::Word GL_LIGHT_MODEL_LOCAL_VIEWER = System::Word(0xb51);
static _DELPHI_CONST System::Word GL_LIGHT_MODEL_TWO_SIDE = System::Word(0xb52);
static _DELPHI_CONST System::Word GL_LIGHT_MODEL_AMBIENT = System::Word(0xb53);
static _DELPHI_CONST System::Word GL_SHADE_MODEL = System::Word(0xb54);
static _DELPHI_CONST System::Word GL_COLOR_MATERIAL_FACE = System::Word(0xb55);
static _DELPHI_CONST System::Word GL_COLOR_MATERIAL_PARAMETER = System::Word(0xb56);
static _DELPHI_CONST System::Word GL_COLOR_MATERIAL = System::Word(0xb57);
static _DELPHI_CONST System::Word GL_FOG = System::Word(0xb60);
static _DELPHI_CONST System::Word GL_FOG_INDEX = System::Word(0xb61);
static _DELPHI_CONST System::Word GL_FOG_DENSITY = System::Word(0xb62);
static _DELPHI_CONST System::Word GL_FOG_START = System::Word(0xb63);
static _DELPHI_CONST System::Word GL_FOG_END = System::Word(0xb64);
static _DELPHI_CONST System::Word GL_FOG_MODE = System::Word(0xb65);
static _DELPHI_CONST System::Word GL_FOG_COLOR = System::Word(0xb66);
static _DELPHI_CONST System::Word GL_ACCUM_CLEAR_VALUE = System::Word(0xb80);
static _DELPHI_CONST System::Word GL_NORMALIZE = System::Word(0xba1);
static _DELPHI_CONST System::Word GL_MODELVIEW_STACK_DEPTH = System::Word(0xba3);
static _DELPHI_CONST System::Word GL_PROJECTION_STACK_DEPTH = System::Word(0xba4);
static _DELPHI_CONST System::Word GL_TEXTURE_STACK_DEPTH = System::Word(0xba5);
static _DELPHI_CONST System::Word GL_MODELVIEW_MATRIX = System::Word(0xba6);
static _DELPHI_CONST System::Word GL_PROJECTION_MATRIX = System::Word(0xba7);
static _DELPHI_CONST System::Word GL_TEXTURE_MATRIX = System::Word(0xba8);
static _DELPHI_CONST System::Word GL_ATTRIB_STACK_DEPTH = System::Word(0xbb0);
static _DELPHI_CONST System::Word GL_CLIENT_ATTRIB_STACK_DEPTH = System::Word(0xbb1);
static _DELPHI_CONST System::Word GL_ALPHA_TEST = System::Word(0xbc0);
static _DELPHI_CONST System::Word GL_ALPHA_TEST_FUNC = System::Word(0xbc1);
static _DELPHI_CONST System::Word GL_ALPHA_TEST_REF = System::Word(0xbc2);
static _DELPHI_CONST System::Word GL_INDEX_LOGIC_OP = System::Word(0xbf1);
static _DELPHI_CONST System::Word GL_LOGIC_OP = System::Word(0xbf1);
static _DELPHI_CONST System::Word GL_AUX_BUFFERS = System::Word(0xc00);
static _DELPHI_CONST System::Word GL_INDEX_CLEAR_VALUE = System::Word(0xc20);
static _DELPHI_CONST System::Word GL_INDEX_WRITEMASK = System::Word(0xc21);
static _DELPHI_CONST System::Word GL_INDEX_MODE = System::Word(0xc30);
static _DELPHI_CONST System::Word GL_RGBA_MODE = System::Word(0xc31);
static _DELPHI_CONST System::Word GL_RENDER_MODE = System::Word(0xc40);
static _DELPHI_CONST System::Word GL_PERSPECTIVE_CORRECTION_HINT = System::Word(0xc50);
static _DELPHI_CONST System::Word GL_POINT_SMOOTH_HINT = System::Word(0xc51);
static _DELPHI_CONST System::Word GL_FOG_HINT = System::Word(0xc54);
static _DELPHI_CONST System::Word GL_TEXTURE_GEN_S = System::Word(0xc60);
static _DELPHI_CONST System::Word GL_TEXTURE_GEN_T = System::Word(0xc61);
static _DELPHI_CONST System::Word GL_TEXTURE_GEN_R = System::Word(0xc62);
static _DELPHI_CONST System::Word GL_TEXTURE_GEN_Q = System::Word(0xc63);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_I = System::Word(0xc70);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_S_TO_S = System::Word(0xc71);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_R = System::Word(0xc72);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_G = System::Word(0xc73);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_B = System::Word(0xc74);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_A = System::Word(0xc75);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_R_TO_R = System::Word(0xc76);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_G_TO_G = System::Word(0xc77);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_B_TO_B = System::Word(0xc78);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_A_TO_A = System::Word(0xc79);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_I_SIZE = System::Word(0xcb0);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_S_TO_S_SIZE = System::Word(0xcb1);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_R_SIZE = System::Word(0xcb2);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_G_SIZE = System::Word(0xcb3);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_B_SIZE = System::Word(0xcb4);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_I_TO_A_SIZE = System::Word(0xcb5);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_R_TO_R_SIZE = System::Word(0xcb6);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_G_TO_G_SIZE = System::Word(0xcb7);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_B_TO_B_SIZE = System::Word(0xcb8);
static _DELPHI_CONST System::Word GL_PIXEL_MAP_A_TO_A_SIZE = System::Word(0xcb9);
static _DELPHI_CONST System::Word GL_MAP_COLOR = System::Word(0xd10);
static _DELPHI_CONST System::Word GL_MAP_STENCIL = System::Word(0xd11);
static _DELPHI_CONST System::Word GL_INDEX_SHIFT = System::Word(0xd12);
static _DELPHI_CONST System::Word GL_INDEX_OFFSET = System::Word(0xd13);
static _DELPHI_CONST System::Word GL_RED_SCALE = System::Word(0xd14);
static _DELPHI_CONST System::Word GL_RED_BIAS = System::Word(0xd15);
static _DELPHI_CONST System::Word GL_ZOOM_X = System::Word(0xd16);
static _DELPHI_CONST System::Word GL_ZOOM_Y = System::Word(0xd17);
static _DELPHI_CONST System::Word GL_GREEN_SCALE = System::Word(0xd18);
static _DELPHI_CONST System::Word GL_GREEN_BIAS = System::Word(0xd19);
static _DELPHI_CONST System::Word GL_BLUE_SCALE = System::Word(0xd1a);
static _DELPHI_CONST System::Word GL_BLUE_BIAS = System::Word(0xd1b);
static _DELPHI_CONST System::Word GL_ALPHA_SCALE = System::Word(0xd1c);
static _DELPHI_CONST System::Word GL_ALPHA_BIAS = System::Word(0xd1d);
static _DELPHI_CONST System::Word GL_DEPTH_SCALE = System::Word(0xd1e);
static _DELPHI_CONST System::Word GL_DEPTH_BIAS = System::Word(0xd1f);
static _DELPHI_CONST System::Word GL_MAX_EVAL_ORDER = System::Word(0xd30);
static _DELPHI_CONST System::Word GL_MAX_LIGHTS = System::Word(0xd31);
static _DELPHI_CONST System::Word GL_MAX_CLIP_PLANES = System::Word(0xd32);
static _DELPHI_CONST System::Word GL_MAX_PIXEL_MAP_TABLE = System::Word(0xd34);
static _DELPHI_CONST System::Word GL_MAX_ATTRIB_STACK_DEPTH = System::Word(0xd35);
static _DELPHI_CONST System::Word GL_MAX_MODELVIEW_STACK_DEPTH = System::Word(0xd36);
static _DELPHI_CONST System::Word GL_MAX_NAME_STACK_DEPTH = System::Word(0xd37);
static _DELPHI_CONST System::Word GL_MAX_PROJECTION_STACK_DEPTH = System::Word(0xd38);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_STACK_DEPTH = System::Word(0xd39);
static _DELPHI_CONST System::Word GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = System::Word(0xd3b);
static _DELPHI_CONST System::Word GL_INDEX_BITS = System::Word(0xd51);
static _DELPHI_CONST System::Word GL_RED_BITS = System::Word(0xd52);
static _DELPHI_CONST System::Word GL_GREEN_BITS = System::Word(0xd53);
static _DELPHI_CONST System::Word GL_BLUE_BITS = System::Word(0xd54);
static _DELPHI_CONST System::Word GL_ALPHA_BITS = System::Word(0xd55);
static _DELPHI_CONST System::Word GL_DEPTH_BITS = System::Word(0xd56);
static _DELPHI_CONST System::Word GL_STENCIL_BITS = System::Word(0xd57);
static _DELPHI_CONST System::Word GL_ACCUM_RED_BITS = System::Word(0xd58);
static _DELPHI_CONST System::Word GL_ACCUM_GREEN_BITS = System::Word(0xd59);
static _DELPHI_CONST System::Word GL_ACCUM_BLUE_BITS = System::Word(0xd5a);
static _DELPHI_CONST System::Word GL_ACCUM_ALPHA_BITS = System::Word(0xd5b);
static _DELPHI_CONST System::Word GL_NAME_STACK_DEPTH = System::Word(0xd70);
static _DELPHI_CONST System::Word GL_AUTO_NORMAL = System::Word(0xd80);
static _DELPHI_CONST System::Word GL_MAP1_COLOR_4 = System::Word(0xd90);
static _DELPHI_CONST System::Word GL_MAP1_INDEX = System::Word(0xd91);
static _DELPHI_CONST System::Word GL_MAP1_NORMAL = System::Word(0xd92);
static _DELPHI_CONST System::Word GL_MAP1_TEXTURE_COORD_1 = System::Word(0xd93);
static _DELPHI_CONST System::Word GL_MAP1_TEXTURE_COORD_2 = System::Word(0xd94);
static _DELPHI_CONST System::Word GL_MAP1_TEXTURE_COORD_3 = System::Word(0xd95);
static _DELPHI_CONST System::Word GL_MAP1_TEXTURE_COORD_4 = System::Word(0xd96);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_3 = System::Word(0xd97);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_4 = System::Word(0xd98);
static _DELPHI_CONST System::Word GL_MAP2_COLOR_4 = System::Word(0xdb0);
static _DELPHI_CONST System::Word GL_MAP2_INDEX = System::Word(0xdb1);
static _DELPHI_CONST System::Word GL_MAP2_NORMAL = System::Word(0xdb2);
static _DELPHI_CONST System::Word GL_MAP2_TEXTURE_COORD_1 = System::Word(0xdb3);
static _DELPHI_CONST System::Word GL_MAP2_TEXTURE_COORD_2 = System::Word(0xdb4);
static _DELPHI_CONST System::Word GL_MAP2_TEXTURE_COORD_3 = System::Word(0xdb5);
static _DELPHI_CONST System::Word GL_MAP2_TEXTURE_COORD_4 = System::Word(0xdb6);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_3 = System::Word(0xdb7);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_4 = System::Word(0xdb8);
static _DELPHI_CONST System::Word GL_MAP1_GRID_DOMAIN = System::Word(0xdd0);
static _DELPHI_CONST System::Word GL_MAP1_GRID_SEGMENTS = System::Word(0xdd1);
static _DELPHI_CONST System::Word GL_MAP2_GRID_DOMAIN = System::Word(0xdd2);
static _DELPHI_CONST System::Word GL_MAP2_GRID_SEGMENTS = System::Word(0xdd3);
static _DELPHI_CONST System::Word GL_FEEDBACK_BUFFER_POINTER = System::Word(0xdf0);
static _DELPHI_CONST System::Word GL_FEEDBACK_BUFFER_SIZE = System::Word(0xdf1);
static _DELPHI_CONST System::Word GL_FEEDBACK_BUFFER_TYPE = System::Word(0xdf2);
static _DELPHI_CONST System::Word GL_SELECTION_BUFFER_POINTER = System::Word(0xdf3);
static _DELPHI_CONST System::Word GL_SELECTION_BUFFER_SIZE = System::Word(0xdf4);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPONENTS = System::Word(0x1003);
static _DELPHI_CONST System::Word GL_TEXTURE_LUMINANCE_SIZE = System::Word(0x8060);
static _DELPHI_CONST System::Word GL_TEXTURE_INTENSITY_SIZE = System::Word(0x8061);
static _DELPHI_CONST System::Word GL_TEXTURE_PRIORITY = System::Word(0x8066);
static _DELPHI_CONST System::Word GL_TEXTURE_RESIDENT = System::Word(0x8067);
static _DELPHI_CONST System::Word GL_AMBIENT = System::Word(0x1200);
static _DELPHI_CONST System::Word GL_DIFFUSE = System::Word(0x1201);
static _DELPHI_CONST System::Word GL_SPECULAR = System::Word(0x1202);
static _DELPHI_CONST System::Word GL_POSITION = System::Word(0x1203);
static _DELPHI_CONST System::Word GL_SPOT_DIRECTION = System::Word(0x1204);
static _DELPHI_CONST System::Word GL_SPOT_EXPONENT = System::Word(0x1205);
static _DELPHI_CONST System::Word GL_SPOT_CUTOFF = System::Word(0x1206);
static _DELPHI_CONST System::Word GL_CONSTANT_ATTENUATION = System::Word(0x1207);
static _DELPHI_CONST System::Word GL_LINEAR_ATTENUATION = System::Word(0x1208);
static _DELPHI_CONST System::Word GL_QUADRATIC_ATTENUATION = System::Word(0x1209);
static _DELPHI_CONST System::Word GL_COMPILE = System::Word(0x1300);
static _DELPHI_CONST System::Word GL_COMPILE_AND_EXECUTE = System::Word(0x1301);
static _DELPHI_CONST System::Word GL_2_BYTES = System::Word(0x1407);
static _DELPHI_CONST System::Word GL_3_BYTES = System::Word(0x1408);
static _DELPHI_CONST System::Word GL_4_BYTES = System::Word(0x1409);
static _DELPHI_CONST System::Word GL_DOUBLE_EXT = System::Word(0x140a);
static _DELPHI_CONST System::Word GL_EMISSION = System::Word(0x1600);
static _DELPHI_CONST System::Word GL_SHININESS = System::Word(0x1601);
static _DELPHI_CONST System::Word GL_AMBIENT_AND_DIFFUSE = System::Word(0x1602);
static _DELPHI_CONST System::Word GL_COLOR_INDEXES = System::Word(0x1603);
static _DELPHI_CONST System::Word GL_MODELVIEW = System::Word(0x1700);
static _DELPHI_CONST System::Word GL_PROJECTION = System::Word(0x1701);
static _DELPHI_CONST System::Word GL_COLOR_INDEX = System::Word(0x1900);
static _DELPHI_CONST System::Word GL_LUMINANCE = System::Word(0x1909);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA = System::Word(0x190a);
static _DELPHI_CONST System::Word GL_BITMAP = System::Word(0x1a00);
static _DELPHI_CONST System::Word GL_RENDER = System::Word(0x1c00);
static _DELPHI_CONST System::Word GL_FEEDBACK = System::Word(0x1c01);
static _DELPHI_CONST System::Word GL_SELECT = System::Word(0x1c02);
static _DELPHI_CONST System::Word GL_FLAT = System::Word(0x1d00);
static _DELPHI_CONST System::Word GL_SMOOTH = System::Word(0x1d01);
static _DELPHI_CONST System::Word GL_S = System::Word(0x2000);
static _DELPHI_CONST System::Word GL_T = System::Word(0x2001);
static _DELPHI_CONST System::Word GL_R = System::Word(0x2002);
static _DELPHI_CONST System::Word GL_Q = System::Word(0x2003);
static _DELPHI_CONST System::Word GL_MODULATE = System::Word(0x2100);
static _DELPHI_CONST System::Word GL_DECAL = System::Word(0x2101);
static _DELPHI_CONST System::Word GL_TEXTURE_ENV_MODE = System::Word(0x2200);
static _DELPHI_CONST System::Word GL_TEXTURE_ENV_COLOR = System::Word(0x2201);
static _DELPHI_CONST System::Word GL_TEXTURE_ENV = System::Word(0x2300);
static _DELPHI_CONST System::Word GL_EYE_LINEAR = System::Word(0x2400);
static _DELPHI_CONST System::Word GL_OBJECT_LINEAR = System::Word(0x2401);
static _DELPHI_CONST System::Word GL_SPHERE_MAP = System::Word(0x2402);
static _DELPHI_CONST System::Word GL_TEXTURE_GEN_MODE = System::Word(0x2500);
static _DELPHI_CONST System::Word GL_OBJECT_PLANE = System::Word(0x2501);
static _DELPHI_CONST System::Word GL_EYE_PLANE = System::Word(0x2502);
static _DELPHI_CONST System::Word GL_CLAMP = System::Word(0x2900);
static _DELPHI_CONST System::Word GL_ALPHA4 = System::Word(0x803b);
static _DELPHI_CONST System::Word GL_ALPHA8 = System::Word(0x803c);
static _DELPHI_CONST System::Word GL_ALPHA12 = System::Word(0x803d);
static _DELPHI_CONST System::Word GL_ALPHA16 = System::Word(0x803e);
static _DELPHI_CONST System::Word GL_LUMINANCE4 = System::Word(0x803f);
static _DELPHI_CONST System::Word GL_LUMINANCE8 = System::Word(0x8040);
static _DELPHI_CONST System::Word GL_LUMINANCE12 = System::Word(0x8041);
static _DELPHI_CONST System::Word GL_LUMINANCE16 = System::Word(0x8042);
static _DELPHI_CONST System::Word GL_LUMINANCE4_ALPHA4 = System::Word(0x8043);
static _DELPHI_CONST System::Word GL_LUMINANCE6_ALPHA2 = System::Word(0x8044);
static _DELPHI_CONST System::Word GL_LUMINANCE8_ALPHA8 = System::Word(0x8045);
static _DELPHI_CONST System::Word GL_LUMINANCE12_ALPHA4 = System::Word(0x8046);
static _DELPHI_CONST System::Word GL_LUMINANCE12_ALPHA12 = System::Word(0x8047);
static _DELPHI_CONST System::Word GL_LUMINANCE16_ALPHA16 = System::Word(0x8048);
static _DELPHI_CONST System::Word GL_INTENSITY = System::Word(0x8049);
static _DELPHI_CONST System::Word GL_INTENSITY4 = System::Word(0x804a);
static _DELPHI_CONST System::Word GL_INTENSITY8 = System::Word(0x804b);
static _DELPHI_CONST System::Word GL_INTENSITY12 = System::Word(0x804c);
static _DELPHI_CONST System::Word GL_INTENSITY16 = System::Word(0x804d);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY = System::Word(0x8074);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY = System::Word(0x8075);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY = System::Word(0x8076);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY = System::Word(0x8077);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY = System::Word(0x8078);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY = System::Word(0x8079);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_SIZE = System::Word(0x807a);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_TYPE = System::Word(0x807b);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_STRIDE = System::Word(0x807c);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_TYPE = System::Word(0x807e);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_STRIDE = System::Word(0x807f);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_SIZE = System::Word(0x8081);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_TYPE = System::Word(0x8082);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_STRIDE = System::Word(0x8083);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_TYPE = System::Word(0x8085);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_STRIDE = System::Word(0x8086);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_SIZE = System::Word(0x8088);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_TYPE = System::Word(0x8089);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_STRIDE = System::Word(0x808a);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_STRIDE = System::Word(0x808c);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_POINTER = System::Word(0x808e);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_POINTER = System::Word(0x808f);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_POINTER = System::Word(0x8090);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_POINTER = System::Word(0x8091);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_POINTER = System::Word(0x8092);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_POINTER = System::Word(0x8093);
static _DELPHI_CONST System::Word GL_V2F = System::Word(0x2a20);
static _DELPHI_CONST System::Word GL_V3F = System::Word(0x2a21);
static _DELPHI_CONST System::Word GL_C4UB_V2F = System::Word(0x2a22);
static _DELPHI_CONST System::Word GL_C4UB_V3F = System::Word(0x2a23);
static _DELPHI_CONST System::Word GL_C3F_V3F = System::Word(0x2a24);
static _DELPHI_CONST System::Word GL_N3F_V3F = System::Word(0x2a25);
static _DELPHI_CONST System::Word GL_C4F_N3F_V3F = System::Word(0x2a26);
static _DELPHI_CONST System::Word GL_T2F_V3F = System::Word(0x2a27);
static _DELPHI_CONST System::Word GL_T4F_V4F = System::Word(0x2a28);
static _DELPHI_CONST System::Word GL_T2F_C4UB_V3F = System::Word(0x2a29);
static _DELPHI_CONST System::Word GL_T2F_C3F_V3F = System::Word(0x2a2a);
static _DELPHI_CONST System::Word GL_T2F_N3F_V3F = System::Word(0x2a2b);
static _DELPHI_CONST System::Word GL_T2F_C4F_N3F_V3F = System::Word(0x2a2c);
static _DELPHI_CONST System::Word GL_T4F_C4F_N3F_V4F = System::Word(0x2a2d);
static _DELPHI_CONST System::Word GL_CLIP_PLANE0 = System::Word(0x3000);
static _DELPHI_CONST System::Word GL_CLIP_PLANE1 = System::Word(0x3001);
static _DELPHI_CONST System::Word GL_CLIP_PLANE2 = System::Word(0x3002);
static _DELPHI_CONST System::Word GL_CLIP_PLANE3 = System::Word(0x3003);
static _DELPHI_CONST System::Word GL_CLIP_PLANE4 = System::Word(0x3004);
static _DELPHI_CONST System::Word GL_CLIP_PLANE5 = System::Word(0x3005);
static _DELPHI_CONST System::Word GL_LIGHT0 = System::Word(0x4000);
static _DELPHI_CONST System::Word GL_LIGHT1 = System::Word(0x4001);
static _DELPHI_CONST System::Word GL_LIGHT2 = System::Word(0x4002);
static _DELPHI_CONST System::Word GL_LIGHT3 = System::Word(0x4003);
static _DELPHI_CONST System::Word GL_LIGHT4 = System::Word(0x4004);
static _DELPHI_CONST System::Word GL_LIGHT5 = System::Word(0x4005);
static _DELPHI_CONST System::Word GL_LIGHT6 = System::Word(0x4006);
static _DELPHI_CONST System::Word GL_LIGHT7 = System::Word(0x4007);
static _DELPHI_CONST System::Word GL_UNSIGNED_BYTE_3_3_2 = System::Word(0x8032);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_4_4_4_4 = System::Word(0x8033);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_5_5_5_1 = System::Word(0x8034);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_8_8_8_8 = System::Word(0x8035);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_10_10_10_2 = System::Word(0x8036);
static _DELPHI_CONST System::Word GL_PACK_SKIP_IMAGES = System::Word(0x806b);
static _DELPHI_CONST System::Word GL_PACK_IMAGE_HEIGHT = System::Word(0x806c);
static _DELPHI_CONST System::Word GL_UNPACK_SKIP_IMAGES = System::Word(0x806d);
static _DELPHI_CONST System::Word GL_UNPACK_IMAGE_HEIGHT = System::Word(0x806e);
static _DELPHI_CONST System::Word GL_TEXTURE_3D = System::Word(0x806f);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_3D = System::Word(0x806a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_3D = System::Word(0x8070);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH = System::Word(0x8071);
static _DELPHI_CONST System::Word GL_TEXTURE_WRAP_R = System::Word(0x8072);
static _DELPHI_CONST System::Word GL_MAX_3D_TEXTURE_SIZE = System::Word(0x8073);
static _DELPHI_CONST System::Word GL_UNSIGNED_BYTE_2_3_3_REV = System::Word(0x8362);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_5_6_5 = System::Word(0x8363);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_5_6_5_REV = System::Word(0x8364);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_4_4_4_4_REV = System::Word(0x8365);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_1_5_5_5_REV = System::Word(0x8366);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_8_8_8_8_REV = System::Word(0x8367);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_2_10_10_10_REV = System::Word(0x8368);
static _DELPHI_CONST System::Word GL_BGR = System::Word(0x80e0);
static _DELPHI_CONST System::Word GL_BGRA = System::Word(0x80e1);
static _DELPHI_CONST System::Word GL_MAX_ELEMENTS_VERTICES = System::Word(0x80e8);
static _DELPHI_CONST System::Word GL_MAX_ELEMENTS_INDICES = System::Word(0x80e9);
static _DELPHI_CONST System::Word GL_CLAMP_TO_EDGE = System::Word(0x812f);
static _DELPHI_CONST System::Word GL_TEXTURE_MIN_LOD = System::Word(0x813a);
static _DELPHI_CONST System::Word GL_TEXTURE_MAX_LOD = System::Word(0x813b);
static _DELPHI_CONST System::Word GL_TEXTURE_BASE_LEVEL = System::Word(0x813c);
static _DELPHI_CONST System::Word GL_TEXTURE_MAX_LEVEL = System::Word(0x813d);
static _DELPHI_CONST System::Word GL_SMOOTH_POINT_SIZE_RANGE = System::Word(0xb12);
static _DELPHI_CONST System::Word GL_SMOOTH_POINT_SIZE_GRANULARITY = System::Word(0xb13);
static _DELPHI_CONST System::Word GL_SMOOTH_LINE_WIDTH_RANGE = System::Word(0xb22);
static _DELPHI_CONST System::Word GL_SMOOTH_LINE_WIDTH_GRANULARITY = System::Word(0xb23);
static _DELPHI_CONST System::Word GL_ALIASED_LINE_WIDTH_RANGE = System::Word(0x846e);
static _DELPHI_CONST System::Word GL_CONSTANT_COLOR = System::Word(0x8001);
static _DELPHI_CONST System::Word GL_ONE_MINUS_CONSTANT_COLOR = System::Word(0x8002);
static _DELPHI_CONST System::Word GL_CONSTANT_ALPHA = System::Word(0x8003);
static _DELPHI_CONST System::Word GL_ONE_MINUS_CONSTANT_ALPHA = System::Word(0x8004);
static _DELPHI_CONST System::Word GL_BLEND_COLOR = System::Word(0x8005);
static _DELPHI_CONST System::Word GL_FUNC_ADD = System::Word(0x8006);
static _DELPHI_CONST System::Word GL_MIN = System::Word(0x8007);
static _DELPHI_CONST System::Word GL_MAX = System::Word(0x8008);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION = System::Word(0x8009);
static _DELPHI_CONST System::Word GL_FUNC_SUBTRACT = System::Word(0x800a);
static _DELPHI_CONST System::Word GL_FUNC_REVERSE_SUBTRACT = System::Word(0x800b);
static _DELPHI_CONST System::Word GL_RESCALE_NORMAL = System::Word(0x803a);
static _DELPHI_CONST System::Word GL_LIGHT_MODEL_COLOR_CONTROL = System::Word(0x81f8);
static _DELPHI_CONST System::Word GL_SINGLE_COLOR = System::Word(0x81f9);
static _DELPHI_CONST System::Word GL_SEPARATE_SPECULAR_COLOR = System::Word(0x81fa);
static _DELPHI_CONST System::Word GL_ALIASED_POINT_SIZE_RANGE = System::Word(0x846d);
static _DELPHI_CONST System::Word GL_CONVOLUTION_1D = System::Word(0x8010);
static _DELPHI_CONST System::Word GL_CONVOLUTION_2D = System::Word(0x8011);
static _DELPHI_CONST System::Word GL_SEPARABLE_2D = System::Word(0x8012);
static _DELPHI_CONST System::Word GL_CONVOLUTION_BORDER_MODE = System::Word(0x8013);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FILTER_SCALE = System::Word(0x8014);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FILTER_BIAS = System::Word(0x8015);
static _DELPHI_CONST System::Word GL_REDUCE = System::Word(0x8016);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FORMAT = System::Word(0x8017);
static _DELPHI_CONST System::Word GL_CONVOLUTION_WIDTH = System::Word(0x8018);
static _DELPHI_CONST System::Word GL_CONVOLUTION_HEIGHT = System::Word(0x8019);
static _DELPHI_CONST System::Word GL_MAX_CONVOLUTION_WIDTH = System::Word(0x801a);
static _DELPHI_CONST System::Word GL_MAX_CONVOLUTION_HEIGHT = System::Word(0x801b);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_RED_SCALE = System::Word(0x801c);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_GREEN_SCALE = System::Word(0x801d);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_BLUE_SCALE = System::Word(0x801e);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_ALPHA_SCALE = System::Word(0x801f);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_RED_BIAS = System::Word(0x8020);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_GREEN_BIAS = System::Word(0x8021);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_BLUE_BIAS = System::Word(0x8022);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_ALPHA_BIAS = System::Word(0x8023);
static _DELPHI_CONST System::Word GL_HISTOGRAM = System::Word(0x8024);
static _DELPHI_CONST System::Word GL_PROXY_HISTOGRAM = System::Word(0x8025);
static _DELPHI_CONST System::Word GL_HISTOGRAM_WIDTH = System::Word(0x8026);
static _DELPHI_CONST System::Word GL_HISTOGRAM_FORMAT = System::Word(0x8027);
static _DELPHI_CONST System::Word GL_HISTOGRAM_RED_SIZE = System::Word(0x8028);
static _DELPHI_CONST System::Word GL_HISTOGRAM_GREEN_SIZE = System::Word(0x8029);
static _DELPHI_CONST System::Word GL_HISTOGRAM_BLUE_SIZE = System::Word(0x802a);
static _DELPHI_CONST System::Word GL_HISTOGRAM_ALPHA_SIZE = System::Word(0x802b);
static _DELPHI_CONST System::Word GL_HISTOGRAM_LUMINANCE_SIZE = System::Word(0x802c);
static _DELPHI_CONST System::Word GL_HISTOGRAM_SINK = System::Word(0x802d);
static _DELPHI_CONST System::Word GL_MINMAX = System::Word(0x802e);
static _DELPHI_CONST System::Word GL_MINMAX_FORMAT = System::Word(0x802f);
static _DELPHI_CONST System::Word GL_MINMAX_SINK = System::Word(0x8030);
static _DELPHI_CONST System::Word GL_TABLE_TOO_LARGE = System::Word(0x8031);
static _DELPHI_CONST System::Word GL_COLOR_MATRIX = System::Word(0x80b1);
static _DELPHI_CONST System::Word GL_COLOR_MATRIX_STACK_DEPTH = System::Word(0x80b2);
static _DELPHI_CONST System::Word GL_MAX_COLOR_MATRIX_STACK_DEPTH = System::Word(0x80b3);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_RED_SCALE = System::Word(0x80b4);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_GREEN_SCALE = System::Word(0x80b5);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_BLUE_SCALE = System::Word(0x80b6);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_ALPHA_SCALE = System::Word(0x80b7);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_RED_BIAS = System::Word(0x80b8);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_GREEN_BIAS = System::Word(0x80b9);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_BLUE_BIAS = System::Word(0x80ba);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_ALPHA_BIAS = System::Word(0x80bb);
static _DELPHI_CONST System::Word GL_COLOR_TABLE = System::Word(0x80d0);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_COLOR_TABLE = System::Word(0x80d1);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_COLOR_TABLE = System::Word(0x80d2);
static _DELPHI_CONST System::Word GL_PROXY_COLOR_TABLE = System::Word(0x80d3);
static _DELPHI_CONST System::Word GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = System::Word(0x80d4);
static _DELPHI_CONST System::Word GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = System::Word(0x80d5);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_SCALE = System::Word(0x80d6);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_BIAS = System::Word(0x80d7);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_FORMAT = System::Word(0x80d8);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_WIDTH = System::Word(0x80d9);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_RED_SIZE = System::Word(0x80da);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_GREEN_SIZE = System::Word(0x80db);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_BLUE_SIZE = System::Word(0x80dc);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_ALPHA_SIZE = System::Word(0x80dd);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_LUMINANCE_SIZE = System::Word(0x80de);
static _DELPHI_CONST System::Word GL_COLOR_TABLE_INTENSITY_SIZE = System::Word(0x80df);
static _DELPHI_CONST System::Word GL_CONSTANT_BORDER = System::Word(0x8151);
static _DELPHI_CONST System::Word GL_REPLICATE_BORDER = System::Word(0x8153);
static _DELPHI_CONST System::Word GL_CONVOLUTION_BORDER_COLOR = System::Word(0x8154);
static _DELPHI_CONST System::Word GL_TEXTURE0 = System::Word(0x84c0);
static _DELPHI_CONST System::Word GL_TEXTURE1 = System::Word(0x84c1);
static _DELPHI_CONST System::Word GL_TEXTURE2 = System::Word(0x84c2);
static _DELPHI_CONST System::Word GL_TEXTURE3 = System::Word(0x84c3);
static _DELPHI_CONST System::Word GL_TEXTURE4 = System::Word(0x84c4);
static _DELPHI_CONST System::Word GL_TEXTURE5 = System::Word(0x84c5);
static _DELPHI_CONST System::Word GL_TEXTURE6 = System::Word(0x84c6);
static _DELPHI_CONST System::Word GL_TEXTURE7 = System::Word(0x84c7);
static _DELPHI_CONST System::Word GL_TEXTURE8 = System::Word(0x84c8);
static _DELPHI_CONST System::Word GL_TEXTURE9 = System::Word(0x84c9);
static _DELPHI_CONST System::Word GL_TEXTURE10 = System::Word(0x84ca);
static _DELPHI_CONST System::Word GL_TEXTURE11 = System::Word(0x84cb);
static _DELPHI_CONST System::Word GL_TEXTURE12 = System::Word(0x84cc);
static _DELPHI_CONST System::Word GL_TEXTURE13 = System::Word(0x84cd);
static _DELPHI_CONST System::Word GL_TEXTURE14 = System::Word(0x84ce);
static _DELPHI_CONST System::Word GL_TEXTURE15 = System::Word(0x84cf);
static _DELPHI_CONST System::Word GL_TEXTURE16 = System::Word(0x84d0);
static _DELPHI_CONST System::Word GL_TEXTURE17 = System::Word(0x84d1);
static _DELPHI_CONST System::Word GL_TEXTURE18 = System::Word(0x84d2);
static _DELPHI_CONST System::Word GL_TEXTURE19 = System::Word(0x84d3);
static _DELPHI_CONST System::Word GL_TEXTURE20 = System::Word(0x84d4);
static _DELPHI_CONST System::Word GL_TEXTURE21 = System::Word(0x84d5);
static _DELPHI_CONST System::Word GL_TEXTURE22 = System::Word(0x84d6);
static _DELPHI_CONST System::Word GL_TEXTURE23 = System::Word(0x84d7);
static _DELPHI_CONST System::Word GL_TEXTURE24 = System::Word(0x84d8);
static _DELPHI_CONST System::Word GL_TEXTURE25 = System::Word(0x84d9);
static _DELPHI_CONST System::Word GL_TEXTURE26 = System::Word(0x84da);
static _DELPHI_CONST System::Word GL_TEXTURE27 = System::Word(0x84db);
static _DELPHI_CONST System::Word GL_TEXTURE28 = System::Word(0x84dc);
static _DELPHI_CONST System::Word GL_TEXTURE29 = System::Word(0x84dd);
static _DELPHI_CONST System::Word GL_TEXTURE30 = System::Word(0x84de);
static _DELPHI_CONST System::Word GL_TEXTURE31 = System::Word(0x84df);
static _DELPHI_CONST System::Word GL_ACTIVE_TEXTURE = System::Word(0x84e0);
static _DELPHI_CONST System::Word GL_MULTISAMPLE = System::Word(0x809d);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_COVERAGE = System::Word(0x809e);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_ONE = System::Word(0x809f);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE = System::Word(0x80a0);
static _DELPHI_CONST System::Word GL_SAMPLE_BUFFERS = System::Word(0x80a8);
static _DELPHI_CONST System::Word GL_SAMPLES = System::Word(0x80a9);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE_VALUE = System::Word(0x80aa);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE_INVERT = System::Word(0x80ab);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP = System::Word(0x8513);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_CUBE_MAP = System::Word(0x8514);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_X = System::Word(0x8515);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_X = System::Word(0x8516);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Y = System::Word(0x8517);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = System::Word(0x8518);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Z = System::Word(0x8519);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = System::Word(0x851a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_CUBE_MAP = System::Word(0x851b);
static _DELPHI_CONST System::Word GL_MAX_CUBE_MAP_TEXTURE_SIZE = System::Word(0x851c);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB = System::Word(0x84ed);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA = System::Word(0x84ee);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSION_HINT = System::Word(0x84ef);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_IMAGE_SIZE = System::Word(0x86a0);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED = System::Word(0x86a1);
static _DELPHI_CONST System::Word GL_NUM_COMPRESSED_TEXTURE_FORMATS = System::Word(0x86a2);
static _DELPHI_CONST System::Word GL_COMPRESSED_TEXTURE_FORMATS = System::Word(0x86a3);
static _DELPHI_CONST System::Word GL_CLAMP_TO_BORDER = System::Word(0x812d);
static _DELPHI_CONST System::Word GL_CLIENT_ACTIVE_TEXTURE = System::Word(0x84e1);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_UNITS = System::Word(0x84e2);
static _DELPHI_CONST System::Word GL_TRANSPOSE_MODELVIEW_MATRIX = System::Word(0x84e3);
static _DELPHI_CONST System::Word GL_TRANSPOSE_PROJECTION_MATRIX = System::Word(0x84e4);
static _DELPHI_CONST System::Word GL_TRANSPOSE_TEXTURE_MATRIX = System::Word(0x84e5);
static _DELPHI_CONST System::Word GL_TRANSPOSE_COLOR_MATRIX = System::Word(0x84e6);
static _DELPHI_CONST int GL_MULTISAMPLE_BIT = int(0x20000000);
static _DELPHI_CONST System::Word GL_NORMAL_MAP = System::Word(0x8511);
static _DELPHI_CONST System::Word GL_REFLECTION_MAP = System::Word(0x8512);
static _DELPHI_CONST System::Word GL_COMPRESSED_ALPHA = System::Word(0x84e9);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE = System::Word(0x84ea);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_ALPHA = System::Word(0x84eb);
static _DELPHI_CONST System::Word GL_COMPRESSED_INTENSITY = System::Word(0x84ec);
static _DELPHI_CONST System::Word GL_COMBINE = System::Word(0x8570);
static _DELPHI_CONST System::Word GL_COMBINE_RGB = System::Word(0x8571);
static _DELPHI_CONST System::Word GL_COMBINE_ALPHA = System::Word(0x8572);
static _DELPHI_CONST System::Word GL_SOURCE0_RGB = System::Word(0x8580);
static _DELPHI_CONST System::Word GL_SOURCE1_RGB = System::Word(0x8581);
static _DELPHI_CONST System::Word GL_SOURCE2_RGB = System::Word(0x8582);
static _DELPHI_CONST System::Word GL_SOURCE0_ALPHA = System::Word(0x8588);
static _DELPHI_CONST System::Word GL_SOURCE1_ALPHA = System::Word(0x8589);
static _DELPHI_CONST System::Word GL_SOURCE2_ALPHA = System::Word(0x858a);
static _DELPHI_CONST System::Word GL_OPERAND0_RGB = System::Word(0x8590);
static _DELPHI_CONST System::Word GL_OPERAND1_RGB = System::Word(0x8591);
static _DELPHI_CONST System::Word GL_OPERAND2_RGB = System::Word(0x8592);
static _DELPHI_CONST System::Word GL_OPERAND0_ALPHA = System::Word(0x8598);
static _DELPHI_CONST System::Word GL_OPERAND1_ALPHA = System::Word(0x8599);
static _DELPHI_CONST System::Word GL_OPERAND2_ALPHA = System::Word(0x859a);
static _DELPHI_CONST System::Word GL_RGB_SCALE = System::Word(0x8573);
static _DELPHI_CONST System::Word GL_ADD_SIGNED = System::Word(0x8574);
static _DELPHI_CONST System::Word GL_INTERPOLATE = System::Word(0x8575);
static _DELPHI_CONST System::Word GL_SUBTRACT = System::Word(0x84e7);
static _DELPHI_CONST System::Word GL_CONSTANT = System::Word(0x8576);
static _DELPHI_CONST System::Word GL_PRIMARY_COLOR = System::Word(0x8577);
static _DELPHI_CONST System::Word GL_PREVIOUS = System::Word(0x8578);
static _DELPHI_CONST System::Word GL_DOT3_RGB = System::Word(0x86ae);
static _DELPHI_CONST System::Word GL_DOT3_RGBA = System::Word(0x86af);
static _DELPHI_CONST System::Word GL_BLEND_DST_RGB = System::Word(0x80c8);
static _DELPHI_CONST System::Word GL_BLEND_SRC_RGB = System::Word(0x80c9);
static _DELPHI_CONST System::Word GL_BLEND_DST_ALPHA = System::Word(0x80ca);
static _DELPHI_CONST System::Word GL_BLEND_SRC_ALPHA = System::Word(0x80cb);
static _DELPHI_CONST System::Word GL_POINT_FADE_THRESHOLD_SIZE = System::Word(0x8128);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT16 = System::Word(0x81a5);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT24 = System::Word(0x81a6);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT32 = System::Word(0x81a7);
static _DELPHI_CONST System::Word GL_MIRRORED_REPEAT = System::Word(0x8370);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_LOD_BIAS = System::Word(0x84fd);
static _DELPHI_CONST System::Word GL_TEXTURE_LOD_BIAS = System::Word(0x8501);
static _DELPHI_CONST System::Word GL_INCR_WRAP = System::Word(0x8507);
static _DELPHI_CONST System::Word GL_DECR_WRAP = System::Word(0x8508);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH_SIZE = System::Word(0x884a);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_MODE = System::Word(0x884c);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_FUNC = System::Word(0x884d);
static _DELPHI_CONST System::Word GL_POINT_SIZE_MIN = System::Word(0x8126);
static _DELPHI_CONST System::Word GL_POINT_SIZE_MAX = System::Word(0x8127);
static _DELPHI_CONST System::Word GL_POINT_DISTANCE_ATTENUATION = System::Word(0x8129);
static _DELPHI_CONST System::Word GL_GENERATE_MIPMAP = System::Word(0x8191);
static _DELPHI_CONST System::Word GL_GENERATE_MIPMAP_HINT = System::Word(0x8192);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_SOURCE = System::Word(0x8450);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE = System::Word(0x8451);
static _DELPHI_CONST System::Word GL_FRAGMENT_DEPTH = System::Word(0x8452);
static _DELPHI_CONST System::Word GL_CURRENT_FOG_COORDINATE = System::Word(0x8453);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_TYPE = System::Word(0x8454);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_STRIDE = System::Word(0x8455);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_POINTER = System::Word(0x8456);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY = System::Word(0x8457);
static _DELPHI_CONST System::Word GL_COLOR_SUM = System::Word(0x8458);
static _DELPHI_CONST System::Word GL_CURRENT_SECONDARY_COLOR = System::Word(0x8459);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_SIZE = System::Word(0x845a);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_TYPE = System::Word(0x845b);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_STRIDE = System::Word(0x845c);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_POINTER = System::Word(0x845d);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY = System::Word(0x845e);
static _DELPHI_CONST System::Word GL_TEXTURE_FILTER_CONTROL = System::Word(0x8500);
static _DELPHI_CONST System::Word GL_DEPTH_TEXTURE_MODE = System::Word(0x884b);
static _DELPHI_CONST System::Word GL_COMPARE_R_TO_TEXTURE = System::Word(0x884e);
static _DELPHI_CONST System::Word GL_BUFFER_SIZE = System::Word(0x8764);
static _DELPHI_CONST System::Word GL_BUFFER_USAGE = System::Word(0x8765);
static _DELPHI_CONST System::Word GL_QUERY_COUNTER_BITS = System::Word(0x8864);
static _DELPHI_CONST System::Word GL_CURRENT_QUERY = System::Word(0x8865);
static _DELPHI_CONST System::Word GL_QUERY_RESULT = System::Word(0x8866);
static _DELPHI_CONST System::Word GL_QUERY_RESULT_AVAILABLE = System::Word(0x8867);
static _DELPHI_CONST System::Word GL_ARRAY_BUFFER = System::Word(0x8892);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_BUFFER = System::Word(0x8893);
static _DELPHI_CONST System::Word GL_ARRAY_BUFFER_BINDING = System::Word(0x8894);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_BUFFER_BINDING = System::Word(0x8895);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = System::Word(0x889f);
static _DELPHI_CONST System::Word GL_READ_ONLY = System::Word(0x88b8);
static _DELPHI_CONST System::Word GL_WRITE_ONLY = System::Word(0x88b9);
static _DELPHI_CONST System::Word GL_READ_WRITE = System::Word(0x88ba);
static _DELPHI_CONST System::Word GL_BUFFER_ACCESS = System::Word(0x88bb);
static _DELPHI_CONST System::Word GL_BUFFER_MAPPED = System::Word(0x88bc);
static _DELPHI_CONST System::Word GL_BUFFER_MAP_POINTER = System::Word(0x88bd);
static _DELPHI_CONST System::Word GL_STREAM_DRAW = System::Word(0x88e0);
static _DELPHI_CONST System::Word GL_STREAM_READ = System::Word(0x88e1);
static _DELPHI_CONST System::Word GL_STREAM_COPY = System::Word(0x88e2);
static _DELPHI_CONST System::Word GL_STATIC_DRAW = System::Word(0x88e4);
static _DELPHI_CONST System::Word GL_STATIC_READ = System::Word(0x88e5);
static _DELPHI_CONST System::Word GL_STATIC_COPY = System::Word(0x88e6);
static _DELPHI_CONST System::Word GL_DYNAMIC_DRAW = System::Word(0x88e8);
static _DELPHI_CONST System::Word GL_DYNAMIC_READ = System::Word(0x88e9);
static _DELPHI_CONST System::Word GL_DYNAMIC_COPY = System::Word(0x88ea);
static _DELPHI_CONST System::Word GL_SAMPLES_PASSED = System::Word(0x8914);
static _DELPHI_CONST System::Word GL_SRC1_ALPHA = System::Word(0x8589);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_BUFFER_BINDING = System::Word(0x8896);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_BUFFER_BINDING = System::Word(0x8897);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_BUFFER_BINDING = System::Word(0x8898);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_BUFFER_BINDING = System::Word(0x8899);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = System::Word(0x889a);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = System::Word(0x889b);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = System::Word(0x889c);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = System::Word(0x889d);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_BUFFER_BINDING = System::Word(0x889e);
static _DELPHI_CONST System::Word GL_FOG_COORD_SRC = System::Word(0x8450);
static _DELPHI_CONST System::Word GL_FOG_COORD = System::Word(0x8451);
static _DELPHI_CONST System::Word GL_CURRENT_FOG_COORD = System::Word(0x8453);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_TYPE = System::Word(0x8454);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_STRIDE = System::Word(0x8455);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_POINTER = System::Word(0x8456);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY = System::Word(0x8457);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_BUFFER_BINDING = System::Word(0x889d);
static _DELPHI_CONST System::Word GL_SRC0_RGB = System::Word(0x8580);
static _DELPHI_CONST System::Word GL_SRC1_RGB = System::Word(0x8581);
static _DELPHI_CONST System::Word GL_SRC2_RGB = System::Word(0x8582);
static _DELPHI_CONST System::Word GL_SRC0_ALPHA = System::Word(0x8588);
static _DELPHI_CONST System::Word GL_SRC2_ALPHA = System::Word(0x858a);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION_RGB = System::Word(0x8009);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_ENABLED = System::Word(0x8622);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_SIZE = System::Word(0x8623);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_STRIDE = System::Word(0x8624);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_TYPE = System::Word(0x8625);
static _DELPHI_CONST System::Word GL_CURRENT_VERTEX_ATTRIB = System::Word(0x8626);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_POINT_SIZE = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_POINTER = System::Word(0x8645);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_FUNC = System::Word(0x8800);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_FAIL = System::Word(0x8801);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_PASS_DEPTH_FAIL = System::Word(0x8802);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_PASS_DEPTH_PASS = System::Word(0x8803);
static _DELPHI_CONST System::Word GL_MAX_DRAW_BUFFERS = System::Word(0x8824);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER0 = System::Word(0x8825);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER1 = System::Word(0x8826);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER2 = System::Word(0x8827);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER3 = System::Word(0x8828);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER4 = System::Word(0x8829);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER5 = System::Word(0x882a);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER6 = System::Word(0x882b);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER7 = System::Word(0x882c);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER8 = System::Word(0x882d);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER9 = System::Word(0x882e);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER10 = System::Word(0x882f);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER11 = System::Word(0x8830);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER12 = System::Word(0x8831);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER13 = System::Word(0x8832);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER14 = System::Word(0x8833);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER15 = System::Word(0x8834);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION_ALPHA = System::Word(0x883d);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATTRIBS = System::Word(0x8869);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = System::Word(0x886a);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_IMAGE_UNITS = System::Word(0x8872);
static _DELPHI_CONST System::Word GL_FRAGMENT_SHADER = System::Word(0x8b30);
static _DELPHI_CONST System::Word GL_VERTEX_SHADER = System::Word(0x8b31);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = System::Word(0x8b49);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_UNIFORM_COMPONENTS = System::Word(0x8b4a);
static _DELPHI_CONST System::Word GL_MAX_VARYING_FLOATS = System::Word(0x8b4b);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = System::Word(0x8b4c);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = System::Word(0x8b4d);
static _DELPHI_CONST System::Word GL_SHADER_TYPE = System::Word(0x8b4f);
static _DELPHI_CONST System::Word GL_FLOAT_VEC2 = System::Word(0x8b50);
static _DELPHI_CONST System::Word GL_FLOAT_VEC3 = System::Word(0x8b51);
static _DELPHI_CONST System::Word GL_FLOAT_VEC4 = System::Word(0x8b52);
static _DELPHI_CONST System::Word GL_INT_VEC2 = System::Word(0x8b53);
static _DELPHI_CONST System::Word GL_INT_VEC3 = System::Word(0x8b54);
static _DELPHI_CONST System::Word GL_INT_VEC4 = System::Word(0x8b55);
static _DELPHI_CONST System::Word GL_BOOL = System::Word(0x8b56);
static _DELPHI_CONST System::Word GL_BOOL_VEC2 = System::Word(0x8b57);
static _DELPHI_CONST System::Word GL_BOOL_VEC3 = System::Word(0x8b58);
static _DELPHI_CONST System::Word GL_BOOL_VEC4 = System::Word(0x8b59);
static _DELPHI_CONST System::Word GL_FLOAT_MAT2 = System::Word(0x8b5a);
static _DELPHI_CONST System::Word GL_FLOAT_MAT3 = System::Word(0x8b5b);
static _DELPHI_CONST System::Word GL_FLOAT_MAT4 = System::Word(0x8b5c);
static _DELPHI_CONST System::Word GL_SAMPLER_1D = System::Word(0x8b5d);
static _DELPHI_CONST System::Word GL_SAMPLER_2D = System::Word(0x8b5e);
static _DELPHI_CONST System::Word GL_SAMPLER_3D = System::Word(0x8b5f);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE = System::Word(0x8b60);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_SHADOW = System::Word(0x8b61);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_SHADOW = System::Word(0x8b62);
static _DELPHI_CONST System::Word GL_DELETE_STATUS = System::Word(0x8b80);
static _DELPHI_CONST System::Word GL_COMPILE_STATUS = System::Word(0x8b81);
static _DELPHI_CONST System::Word GL_LINK_STATUS = System::Word(0x8b82);
static _DELPHI_CONST System::Word GL_VALIDATE_STATUS = System::Word(0x8b83);
static _DELPHI_CONST System::Word GL_INFO_LOG_LENGTH = System::Word(0x8b84);
static _DELPHI_CONST System::Word GL_ATTACHED_SHADERS = System::Word(0x8b85);
static _DELPHI_CONST System::Word GL_ACTIVE_UNIFORMS = System::Word(0x8b86);
static _DELPHI_CONST System::Word GL_ACTIVE_UNIFORM_MAX_LENGTH = System::Word(0x8b87);
static _DELPHI_CONST System::Word GL_SHADER_SOURCE_LENGTH = System::Word(0x8b88);
static _DELPHI_CONST System::Word GL_ACTIVE_ATTRIBUTES = System::Word(0x8b89);
static _DELPHI_CONST System::Word GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = System::Word(0x8b8a);
static _DELPHI_CONST System::Word GL_FRAGMENT_SHADER_DERIVATIVE_HINT = System::Word(0x8b8b);
static _DELPHI_CONST System::Word GL_SHADING_LANGUAGE_VERSION = System::Word(0x8b8c);
static _DELPHI_CONST System::Word GL_CURRENT_PROGRAM = System::Word(0x8b8d);
static _DELPHI_CONST System::Word GL_POINT_SPRITE_COORD_ORIGIN = System::Word(0x8ca0);
static _DELPHI_CONST System::Word GL_LOWER_LEFT = System::Word(0x8ca1);
static _DELPHI_CONST System::Word GL_UPPER_LEFT = System::Word(0x8ca2);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_REF = System::Word(0x8ca3);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_VALUE_MASK = System::Word(0x8ca4);
static _DELPHI_CONST System::Word GL_STENCIL_BACK_WRITEMASK = System::Word(0x8ca5);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_TWO_SIDE = System::Word(0x8643);
static _DELPHI_CONST System::Word GL_POINT_SPRITE = System::Word(0x8861);
static _DELPHI_CONST System::Word GL_COORD_REPLACE = System::Word(0x8862);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_COORDS = System::Word(0x8871);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER = System::Word(0x88eb);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER = System::Word(0x88ec);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER_BINDING = System::Word(0x88ed);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER_BINDING = System::Word(0x88ef);
static _DELPHI_CONST System::Word GL_FLOAT_MAT2x3 = System::Word(0x8b65);
static _DELPHI_CONST System::Word GL_FLOAT_MAT2x4 = System::Word(0x8b66);
static _DELPHI_CONST System::Word GL_FLOAT_MAT3x2 = System::Word(0x8b67);
static _DELPHI_CONST System::Word GL_FLOAT_MAT3x4 = System::Word(0x8b68);
static _DELPHI_CONST System::Word GL_FLOAT_MAT4x2 = System::Word(0x8b69);
static _DELPHI_CONST System::Word GL_FLOAT_MAT4x3 = System::Word(0x8b6a);
static _DELPHI_CONST System::Word GL_SRGB = System::Word(0x8c40);
static _DELPHI_CONST System::Word GL_SRGB8 = System::Word(0x8c41);
static _DELPHI_CONST System::Word GL_SRGB_ALPHA = System::Word(0x8c42);
static _DELPHI_CONST System::Word GL_SRGB8_ALPHA8 = System::Word(0x8c43);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB = System::Word(0x8c48);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA = System::Word(0x8c49);
static _DELPHI_CONST System::Word GL_CURRENT_RASTER_SECONDARY_COLOR = System::Word(0x845f);
static _DELPHI_CONST System::Word GL_SLUMINANCE_ALPHA = System::Word(0x8c44);
static _DELPHI_CONST System::Word GL_SLUMINANCE8_ALPHA8 = System::Word(0x8c45);
static _DELPHI_CONST System::Word GL_SLUMINANCE = System::Word(0x8c46);
static _DELPHI_CONST System::Word GL_SLUMINANCE8 = System::Word(0x8c47);
static _DELPHI_CONST System::Word GL_COMPRESSED_SLUMINANCE = System::Word(0x8c4a);
static _DELPHI_CONST System::Word GL_COMPRESSED_SLUMINANCE_ALPHA = System::Word(0x8c4b);
static _DELPHI_CONST System::Word GL_COMPARE_REF_TO_TEXTURE = System::Word(0x884e);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE0 = System::Word(0x3000);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE1 = System::Word(0x3001);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE2 = System::Word(0x3002);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE3 = System::Word(0x3003);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE4 = System::Word(0x3004);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE5 = System::Word(0x3005);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE6 = System::Word(0x3006);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE7 = System::Word(0x3007);
static _DELPHI_CONST System::Word GL_MAX_CLIP_DISTANCES = System::Word(0xd32);
static _DELPHI_CONST System::Word GL_MAJOR_VERSION = System::Word(0x821b);
static _DELPHI_CONST System::Word GL_MINOR_VERSION = System::Word(0x821c);
static _DELPHI_CONST System::Word GL_NUM_EXTENSIONS = System::Word(0x821d);
static _DELPHI_CONST System::Word GL_CONTEXT_FLAGS = System::Word(0x821e);
static _DELPHI_CONST System::Word GL_DEPTH_BUFFER = System::Word(0x8223);
static _DELPHI_CONST System::Word GL_STENCIL_BUFFER = System::Word(0x8224);
static _DELPHI_CONST System::Word GL_COMPRESSED_RED = System::Word(0x8225);
static _DELPHI_CONST System::Word GL_COMPRESSED_RG = System::Word(0x8226);
static _DELPHI_CONST System::Int8 GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Word GL_RGBA32F = System::Word(0x8814);
static _DELPHI_CONST System::Word GL_RGB32F = System::Word(0x8815);
static _DELPHI_CONST System::Word GL_RGBA16F = System::Word(0x881a);
static _DELPHI_CONST System::Word GL_RGB16F = System::Word(0x881b);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_INTEGER = System::Word(0x88fd);
static _DELPHI_CONST System::Word GL_MAX_ARRAY_TEXTURE_LAYERS = System::Word(0x88ff);
static _DELPHI_CONST System::Word GL_MIN_PROGRAM_TEXEL_OFFSET = System::Word(0x8904);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEXEL_OFFSET = System::Word(0x8905);
static _DELPHI_CONST System::Word GL_CLAMP_VERTEX_COLOR = System::Word(0x891a);
static _DELPHI_CONST System::Word GL_CLAMP_FRAGMENT_COLOR = System::Word(0x891b);
static _DELPHI_CONST System::Word GL_CLAMP_READ_COLOR = System::Word(0x891c);
static _DELPHI_CONST System::Word GL_FIXED_ONLY = System::Word(0x891d);
static _DELPHI_CONST System::Word GL_MAX_VARYING_COMPONENTS = System::Word(0x8b4b);
static _DELPHI_CONST System::Word GL_TEXTURE_1D_ARRAY = System::Word(0x8c18);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_1D_ARRAY = System::Word(0x8c19);
static _DELPHI_CONST System::Word GL_TEXTURE_2D_ARRAY = System::Word(0x8c1a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D_ARRAY = System::Word(0x8c1b);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_1D_ARRAY = System::Word(0x8c1c);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_2D_ARRAY = System::Word(0x8c1d);
static _DELPHI_CONST System::Word GL_R11F_G11F_B10F = System::Word(0x8c3a);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_10F_11F_11F_REV = System::Word(0x8c3b);
static _DELPHI_CONST System::Word GL_RGB9_E5 = System::Word(0x8c3d);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_5_9_9_9_REV = System::Word(0x8c3e);
static _DELPHI_CONST System::Word GL_TEXTURE_SHARED_SIZE = System::Word(0x8c3f);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = System::Word(0x8c76);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_MODE = System::Word(0x8c7f);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = System::Word(0x8c80);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYINGS = System::Word(0x8c83);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_START = System::Word(0x8c84);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = System::Word(0x8c85);
static _DELPHI_CONST System::Word GL_PRIMITIVES_GENERATED = System::Word(0x8c87);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = System::Word(0x8c88);
static _DELPHI_CONST System::Word GL_RASTERIZER_DISCARD = System::Word(0x8c89);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = System::Word(0x8c8a);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = System::Word(0x8c8b);
static _DELPHI_CONST System::Word GL_INTERLEAVED_ATTRIBS = System::Word(0x8c8c);
static _DELPHI_CONST System::Word GL_SEPARATE_ATTRIBS = System::Word(0x8c8d);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER = System::Word(0x8c8e);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = System::Word(0x8c8f);
static _DELPHI_CONST System::Word GL_RGBA32UI = System::Word(0x8d70);
static _DELPHI_CONST System::Word GL_RGB32UI = System::Word(0x8d71);
static _DELPHI_CONST System::Word GL_RGBA16UI = System::Word(0x8d76);
static _DELPHI_CONST System::Word GL_RGB16UI = System::Word(0x8d77);
static _DELPHI_CONST System::Word GL_RGBA8UI = System::Word(0x8d7c);
static _DELPHI_CONST System::Word GL_RGB8UI = System::Word(0x8d7d);
static _DELPHI_CONST System::Word GL_RGBA32I = System::Word(0x8d82);
static _DELPHI_CONST System::Word GL_RGB32I = System::Word(0x8d83);
static _DELPHI_CONST System::Word GL_RGBA16I = System::Word(0x8d88);
static _DELPHI_CONST System::Word GL_RGB16I = System::Word(0x8d89);
static _DELPHI_CONST System::Word GL_RGBA8I = System::Word(0x8d8e);
static _DELPHI_CONST System::Word GL_RGB8I = System::Word(0x8d8f);
static _DELPHI_CONST System::Word GL_RED_INTEGER = System::Word(0x8d94);
static _DELPHI_CONST System::Word GL_GREEN_INTEGER = System::Word(0x8d95);
static _DELPHI_CONST System::Word GL_BLUE_INTEGER = System::Word(0x8d96);
static _DELPHI_CONST System::Word GL_ALPHA_INTEGER = System::Word(0x8d97);
static _DELPHI_CONST System::Word GL_RGB_INTEGER = System::Word(0x8d98);
static _DELPHI_CONST System::Word GL_RGBA_INTEGER = System::Word(0x8d99);
static _DELPHI_CONST System::Word GL_BGR_INTEGER = System::Word(0x8d9a);
static _DELPHI_CONST System::Word GL_BGRA_INTEGER = System::Word(0x8d9b);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_ARRAY = System::Word(0x8dc0);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_ARRAY = System::Word(0x8dc1);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_ARRAY_SHADOW = System::Word(0x8dc3);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_ARRAY_SHADOW = System::Word(0x8dc4);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_SHADOW = System::Word(0x8dc5);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC2 = System::Word(0x8dc6);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC3 = System::Word(0x8dc7);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC4 = System::Word(0x8dc8);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_1D = System::Word(0x8dc9);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D = System::Word(0x8dca);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_3D = System::Word(0x8dcb);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_CUBE = System::Word(0x8dcc);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_1D_ARRAY = System::Word(0x8dce);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_ARRAY = System::Word(0x8dcf);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_1D = System::Word(0x8dd1);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D = System::Word(0x8dd2);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_3D = System::Word(0x8dd3);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_CUBE = System::Word(0x8dd4);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = System::Word(0x8dd6);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = System::Word(0x8dd7);
static _DELPHI_CONST System::Word GL_QUERY_WAIT = System::Word(0x8e13);
static _DELPHI_CONST System::Word GL_QUERY_NO_WAIT = System::Word(0x8e14);
static _DELPHI_CONST System::Word GL_QUERY_BY_REGION_WAIT = System::Word(0x8e15);
static _DELPHI_CONST System::Word GL_QUERY_BY_REGION_NO_WAIT = System::Word(0x8e16);
static _DELPHI_CONST System::Word GL_BUFFER_ACCESS_FLAGS = System::Word(0x911f);
static _DELPHI_CONST System::Word GL_BUFFER_MAP_LENGTH = System::Word(0x9120);
static _DELPHI_CONST System::Word GL_BUFFER_MAP_OFFSET = System::Word(0x9121);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_RECT = System::Word(0x8b63);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_RECT_SHADOW = System::Word(0x8b64);
static _DELPHI_CONST System::Word GL_SAMPLER_BUFFER = System::Word(0x8dc2);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_RECT = System::Word(0x8dcd);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_BUFFER = System::Word(0x8dd0);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_RECT = System::Word(0x8dd5);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_BUFFER = System::Word(0x8dd8);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER = System::Word(0x8c2a);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_BUFFER_SIZE = System::Word(0x8c2b);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_BUFFER = System::Word(0x8c2c);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_DATA_STORE_BINDING = System::Word(0x8c2d);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_FORMAT = System::Word(0x8c2e);
static _DELPHI_CONST System::Word GL_TEXTURE_RECTANGLE = System::Word(0x84f5);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_RECTANGLE = System::Word(0x84f6);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_RECTANGLE = System::Word(0x84f7);
static _DELPHI_CONST System::Word GL_MAX_RECTANGLE_TEXTURE_SIZE = System::Word(0x84f8);
static _DELPHI_CONST System::Word GL_RED_SNORM = System::Word(0x8f90);
static _DELPHI_CONST System::Word GL_RG_SNORM = System::Word(0x8f91);
static _DELPHI_CONST System::Word GL_RGB_SNORM = System::Word(0x8f92);
static _DELPHI_CONST System::Word GL_RGBA_SNORM = System::Word(0x8f93);
static _DELPHI_CONST System::Word GL_R8_SNORM = System::Word(0x8f94);
static _DELPHI_CONST System::Word GL_RG8_SNORM = System::Word(0x8f95);
static _DELPHI_CONST System::Word GL_RGB8_SNORM = System::Word(0x8f96);
static _DELPHI_CONST System::Word GL_RGBA8_SNORM = System::Word(0x8f97);
static _DELPHI_CONST System::Word GL_R16_SNORM = System::Word(0x8f98);
static _DELPHI_CONST System::Word GL_RG16_SNORM = System::Word(0x8f99);
static _DELPHI_CONST System::Word GL_RGB16_SNORM = System::Word(0x8f9a);
static _DELPHI_CONST System::Word GL_RGBA16_SNORM = System::Word(0x8f9b);
static _DELPHI_CONST System::Word GL_SIGNED_NORMALIZED = System::Word(0x8f9c);
static _DELPHI_CONST System::Word GL_PRIMITIVE_RESTART = System::Word(0x8f9d);
static _DELPHI_CONST System::Word GL_PRIMITIVE_RESTART_INDEX = System::Word(0x8f9e);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_BPTC_UNORM_ARB = System::Word(0x8e8c);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB = System::Word(0x8e8d);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB = System::Word(0x8e8e);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB = System::Word(0x8e8f);
static _DELPHI_CONST System::Int8 GL_CONTEXT_CORE_PROFILE_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_LINES_ADJACENCY = System::Int8(0xa);
static _DELPHI_CONST System::Int8 GL_LINE_STRIP_ADJACENCY = System::Int8(0xb);
static _DELPHI_CONST System::Int8 GL_TRIANGLES_ADJACENCY = System::Int8(0xc);
static _DELPHI_CONST System::Int8 GL_TRIANGLE_STRIP_ADJACENCY = System::Int8(0xd);
static _DELPHI_CONST System::Word GL_PROGRAM_POINT_SIZE = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = System::Word(0x8c29);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_LAYERED = System::Word(0x8da7);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = System::Word(0x8da8);
static _DELPHI_CONST System::Word GL_GEOMETRY_SHADER = System::Word(0x8dd9);
static _DELPHI_CONST System::Word GL_GEOMETRY_VERTICES_OUT = System::Word(0x8916);
static _DELPHI_CONST System::Word GL_GEOMETRY_INPUT_TYPE = System::Word(0x8917);
static _DELPHI_CONST System::Word GL_GEOMETRY_OUTPUT_TYPE = System::Word(0x8918);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = System::Word(0x8ddf);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_OUTPUT_VERTICES = System::Word(0x8de0);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = System::Word(0x8de1);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_OUTPUT_COMPONENTS = System::Word(0x9122);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_INPUT_COMPONENTS = System::Word(0x9123);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = System::Word(0x9124);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_INPUT_COMPONENTS = System::Word(0x9125);
static _DELPHI_CONST System::Word GL_CONTEXT_PROFILE_MASK = System::Word(0x9126);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_DIVISOR = System::Word(0x88fe);
static _DELPHI_CONST System::Word GL_SAMPLE_SHADING = System::Word(0x8c36);
static _DELPHI_CONST System::Word GL_MIN_SAMPLE_SHADING_VALUE = System::Word(0x8c37);
static _DELPHI_CONST System::Word GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET = System::Word(0x8e5e);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET = System::Word(0x8e5f);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_ARRAY = System::Word(0x9009);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_CUBE_MAP_ARRAY = System::Word(0x900a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_CUBE_MAP_ARRAY = System::Word(0x900b);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_MAP_ARRAY = System::Word(0x900c);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW = System::Word(0x900d);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_CUBE_MAP_ARRAY = System::Word(0x900e);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY = System::Word(0x900f);
static _DELPHI_CONST System::Word GL_ACTIVE_TEXTURE_ARB = System::Word(0x84e0);
static _DELPHI_CONST System::Word GL_CLIENT_ACTIVE_TEXTURE_ARB = System::Word(0x84e1);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_UNITS_ARB = System::Word(0x84e2);
static _DELPHI_CONST System::Word GL_TEXTURE0_ARB = System::Word(0x84c0);
static _DELPHI_CONST System::Word GL_TEXTURE1_ARB = System::Word(0x84c1);
static _DELPHI_CONST System::Word GL_TEXTURE2_ARB = System::Word(0x84c2);
static _DELPHI_CONST System::Word GL_TEXTURE3_ARB = System::Word(0x84c3);
static _DELPHI_CONST System::Word GL_TEXTURE4_ARB = System::Word(0x84c4);
static _DELPHI_CONST System::Word GL_TEXTURE5_ARB = System::Word(0x84c5);
static _DELPHI_CONST System::Word GL_TEXTURE6_ARB = System::Word(0x84c6);
static _DELPHI_CONST System::Word GL_TEXTURE7_ARB = System::Word(0x84c7);
static _DELPHI_CONST System::Word GL_TEXTURE8_ARB = System::Word(0x84c8);
static _DELPHI_CONST System::Word GL_TEXTURE9_ARB = System::Word(0x84c9);
static _DELPHI_CONST System::Word GL_TEXTURE10_ARB = System::Word(0x84ca);
static _DELPHI_CONST System::Word GL_TEXTURE11_ARB = System::Word(0x84cb);
static _DELPHI_CONST System::Word GL_TEXTURE12_ARB = System::Word(0x84cc);
static _DELPHI_CONST System::Word GL_TEXTURE13_ARB = System::Word(0x84cd);
static _DELPHI_CONST System::Word GL_TEXTURE14_ARB = System::Word(0x84ce);
static _DELPHI_CONST System::Word GL_TEXTURE15_ARB = System::Word(0x84cf);
static _DELPHI_CONST System::Word GL_TEXTURE16_ARB = System::Word(0x84d0);
static _DELPHI_CONST System::Word GL_TEXTURE17_ARB = System::Word(0x84d1);
static _DELPHI_CONST System::Word GL_TEXTURE18_ARB = System::Word(0x84d2);
static _DELPHI_CONST System::Word GL_TEXTURE19_ARB = System::Word(0x84d3);
static _DELPHI_CONST System::Word GL_TEXTURE20_ARB = System::Word(0x84d4);
static _DELPHI_CONST System::Word GL_TEXTURE21_ARB = System::Word(0x84d5);
static _DELPHI_CONST System::Word GL_TEXTURE22_ARB = System::Word(0x84d6);
static _DELPHI_CONST System::Word GL_TEXTURE23_ARB = System::Word(0x84d7);
static _DELPHI_CONST System::Word GL_TEXTURE24_ARB = System::Word(0x84d8);
static _DELPHI_CONST System::Word GL_TEXTURE25_ARB = System::Word(0x84d9);
static _DELPHI_CONST System::Word GL_TEXTURE26_ARB = System::Word(0x84da);
static _DELPHI_CONST System::Word GL_TEXTURE27_ARB = System::Word(0x84db);
static _DELPHI_CONST System::Word GL_TEXTURE28_ARB = System::Word(0x84dc);
static _DELPHI_CONST System::Word GL_TEXTURE29_ARB = System::Word(0x84dd);
static _DELPHI_CONST System::Word GL_TEXTURE30_ARB = System::Word(0x84de);
static _DELPHI_CONST System::Word GL_TEXTURE31_ARB = System::Word(0x84df);
static _DELPHI_CONST System::Word GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = System::Word(0x84e3);
static _DELPHI_CONST System::Word GL_TRANSPOSE_PROJECTION_MATRIX_ARB = System::Word(0x84e4);
static _DELPHI_CONST System::Word GL_TRANSPOSE_TEXTURE_MATRIX_ARB = System::Word(0x84e5);
static _DELPHI_CONST System::Word GL_TRANSPOSE_COLOR_MATRIX_ARB = System::Word(0x84e6);
static _DELPHI_CONST System::Int8 WGL_FRONT_COLOR_BUFFER_BIT_ARB = System::Int8(0x1);
static _DELPHI_CONST System::Int8 WGL_BACK_COLOR_BUFFER_BIT_ARB = System::Int8(0x2);
static _DELPHI_CONST System::Int8 WGL_DEPTH_BUFFER_BIT_ARB = System::Int8(0x4);
static _DELPHI_CONST System::Int8 WGL_STENCIL_BUFFER_BIT_ARB = System::Int8(0x8);
static _DELPHI_CONST System::Word GL_MULTISAMPLE_ARB = System::Word(0x809d);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = System::Word(0x809e);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_ONE_ARB = System::Word(0x809f);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE_ARB = System::Word(0x80a0);
static _DELPHI_CONST System::Word GL_SAMPLE_BUFFERS_ARB = System::Word(0x80a8);
static _DELPHI_CONST System::Word GL_SAMPLES_ARB = System::Word(0x80a9);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE_VALUE_ARB = System::Word(0x80aa);
static _DELPHI_CONST System::Word GL_SAMPLE_COVERAGE_INVERT_ARB = System::Word(0x80ab);
static _DELPHI_CONST int GL_MULTISAMPLE_BIT_ARB = int(0x20000000);
static _DELPHI_CONST int GLX_SAMPLE_BUFFERS_ARB = int(0x186a0);
static _DELPHI_CONST int GLX_SAMPLES_ARB = int(0x186a1);
static _DELPHI_CONST System::Word WGL_SAMPLE_BUFFERS_ARB = System::Word(0x2041);
static _DELPHI_CONST System::Word WGL_SAMPLES_ARB = System::Word(0x2042);
static _DELPHI_CONST int GLX_SAMPLE_BUFFERS_SGIS = int(0x100000);
static _DELPHI_CONST int GLX_SAMPLES_SGIS = int(0x100001);
static _DELPHI_CONST int GLX_SAMPLE_BUFFERS = int(0x100000);
static _DELPHI_CONST int GLX_SAMPLES = int(0x100001);
static _DELPHI_CONST System::Word GL_NORMAL_MAP_ARB = System::Word(0x8511);
static _DELPHI_CONST System::Word GL_REFLECTION_MAP_ARB = System::Word(0x8512);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_ARB = System::Word(0x8513);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_CUBE_MAP_ARB = System::Word(0x8514);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = System::Word(0x8515);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = System::Word(0x8516);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = System::Word(0x8517);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = System::Word(0x8518);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = System::Word(0x8519);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = System::Word(0x851a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_CUBE_MAP_ARB = System::Word(0x851b);
static _DELPHI_CONST System::Word GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = System::Word(0x851c);
static _DELPHI_CONST System::Word WGL_NUMBER_PIXEL_FORMATS_ARB = System::Word(0x2000);
static _DELPHI_CONST System::Word WGL_DRAW_TO_WINDOW_ARB = System::Word(0x2001);
static _DELPHI_CONST System::Word WGL_DRAW_TO_BITMAP_ARB = System::Word(0x2002);
static _DELPHI_CONST System::Word WGL_ACCELERATION_ARB = System::Word(0x2003);
static _DELPHI_CONST System::Word WGL_NEED_PALETTE_ARB = System::Word(0x2004);
static _DELPHI_CONST System::Word WGL_NEED_SYSTEM_PALETTE_ARB = System::Word(0x2005);
static _DELPHI_CONST System::Word WGL_SWAP_LAYER_BUFFERS_ARB = System::Word(0x2006);
static _DELPHI_CONST System::Word WGL_SWAP_METHOD_ARB = System::Word(0x2007);
static _DELPHI_CONST System::Word WGL_NUMBER_OVERLAYS_ARB = System::Word(0x2008);
static _DELPHI_CONST System::Word WGL_NUMBER_UNDERLAYS_ARB = System::Word(0x2009);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_ARB = System::Word(0x200a);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_RED_VALUE_ARB = System::Word(0x2037);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_GREEN_VALUE_ARB = System::Word(0x2038);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_BLUE_VALUE_ARB = System::Word(0x2039);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_ALPHA_VALUE_ARB = System::Word(0x203a);
static _DELPHI_CONST System::Word WGL_TRANSPARENT_INDEX_VALUE_ARB = System::Word(0x203b);
static _DELPHI_CONST System::Word WGL_SHARE_DEPTH_ARB = System::Word(0x200c);
static _DELPHI_CONST System::Word WGL_SHARE_STENCIL_ARB = System::Word(0x200d);
static _DELPHI_CONST System::Word WGL_SHARE_ACCUM_ARB = System::Word(0x200e);
static _DELPHI_CONST System::Word WGL_SUPPORT_GDI_ARB = System::Word(0x200f);
static _DELPHI_CONST System::Word WGL_SUPPORT_OPENGL_ARB = System::Word(0x2010);
static _DELPHI_CONST System::Word WGL_DOUBLE_BUFFER_ARB = System::Word(0x2011);
static _DELPHI_CONST System::Word WGL_STEREO_ARB = System::Word(0x2012);
static _DELPHI_CONST System::Word WGL_PIXEL_TYPE_ARB = System::Word(0x2013);
static _DELPHI_CONST System::Word WGL_COLOR_BITS_ARB = System::Word(0x2014);
static _DELPHI_CONST System::Word WGL_RED_BITS_ARB = System::Word(0x2015);
static _DELPHI_CONST System::Word WGL_RED_SHIFT_ARB = System::Word(0x2016);
static _DELPHI_CONST System::Word WGL_GREEN_BITS_ARB = System::Word(0x2017);
static _DELPHI_CONST System::Word WGL_GREEN_SHIFT_ARB = System::Word(0x2018);
static _DELPHI_CONST System::Word WGL_BLUE_BITS_ARB = System::Word(0x2019);
static _DELPHI_CONST System::Word WGL_BLUE_SHIFT_ARB = System::Word(0x201a);
static _DELPHI_CONST System::Word WGL_ALPHA_BITS_ARB = System::Word(0x201b);
static _DELPHI_CONST System::Word WGL_ALPHA_SHIFT_ARB = System::Word(0x201c);
static _DELPHI_CONST System::Word WGL_ACCUM_BITS_ARB = System::Word(0x201d);
static _DELPHI_CONST System::Word WGL_ACCUM_RED_BITS_ARB = System::Word(0x201e);
static _DELPHI_CONST System::Word WGL_ACCUM_GREEN_BITS_ARB = System::Word(0x201f);
static _DELPHI_CONST System::Word WGL_ACCUM_BLUE_BITS_ARB = System::Word(0x2020);
static _DELPHI_CONST System::Word WGL_ACCUM_ALPHA_BITS_ARB = System::Word(0x2021);
static _DELPHI_CONST System::Word WGL_DEPTH_BITS_ARB = System::Word(0x2022);
static _DELPHI_CONST System::Word WGL_STENCIL_BITS_ARB = System::Word(0x2023);
static _DELPHI_CONST System::Word WGL_AUX_BUFFERS_ARB = System::Word(0x2024);
static _DELPHI_CONST System::Word WGL_NO_ACCELERATION_ARB = System::Word(0x2025);
static _DELPHI_CONST System::Word WGL_GENERIC_ACCELERATION_ARB = System::Word(0x2026);
static _DELPHI_CONST System::Word WGL_FULL_ACCELERATION_ARB = System::Word(0x2027);
static _DELPHI_CONST System::Word WGL_SWAP_EXCHANGE_ARB = System::Word(0x2028);
static _DELPHI_CONST System::Word WGL_SWAP_COPY_ARB = System::Word(0x2029);
static _DELPHI_CONST System::Word WGL_SWAP_UNDEFINED_ARB = System::Word(0x202a);
static _DELPHI_CONST System::Word WGL_TYPE_RGBA_ARB = System::Word(0x202b);
static _DELPHI_CONST System::Word WGL_TYPE_COLORINDEX_ARB = System::Word(0x202c);
static _DELPHI_CONST System::Word ERROR_INVALID_PIXEL_TYPE_ARB = System::Word(0x2043);
static _DELPHI_CONST System::Word ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = System::Word(0x2054);
static _DELPHI_CONST System::Word WGL_DRAW_TO_PBUFFER_ARB = System::Word(0x202d);
static _DELPHI_CONST System::Word WGL_MAX_PBUFFER_PIXELS_ARB = System::Word(0x202e);
static _DELPHI_CONST System::Word WGL_MAX_PBUFFER_WIDTH_ARB = System::Word(0x202f);
static _DELPHI_CONST System::Word WGL_MAX_PBUFFER_HEIGHT_ARB = System::Word(0x2030);
static _DELPHI_CONST System::Word WGL_PBUFFER_LARGEST_ARB = System::Word(0x2033);
static _DELPHI_CONST System::Word WGL_PBUFFER_WIDTH_ARB = System::Word(0x2034);
static _DELPHI_CONST System::Word WGL_PBUFFER_HEIGHT_ARB = System::Word(0x2035);
static _DELPHI_CONST System::Word WGL_PBUFFER_LOST_ARB = System::Word(0x2036);
static _DELPHI_CONST System::Word GL_COMPRESSED_ALPHA_ARB = System::Word(0x84e9);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_ARB = System::Word(0x84ea);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_ALPHA_ARB = System::Word(0x84eb);
static _DELPHI_CONST System::Word GL_COMPRESSED_INTENSITY_ARB = System::Word(0x84ec);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB_ARB = System::Word(0x84ed);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ARB = System::Word(0x84ee);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSION_HINT_ARB = System::Word(0x84ef);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = System::Word(0x86a0);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_ARB = System::Word(0x86a1);
static _DELPHI_CONST System::Word GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = System::Word(0x86a2);
static _DELPHI_CONST System::Word GL_COMPRESSED_TEXTURE_FORMATS_ARB = System::Word(0x86a3);
static _DELPHI_CONST System::Word GL_CLAMP_TO_BORDER_ARB = System::Word(0x812d);
static _DELPHI_CONST System::Word GL_POINT_SIZE_MIN_ARB = System::Word(0x8126);
static _DELPHI_CONST System::Word GL_POINT_SIZE_MAX_ARB = System::Word(0x8127);
static _DELPHI_CONST System::Word GL_POINT_FADE_THRESHOLD_SIZE_ARB = System::Word(0x8128);
static _DELPHI_CONST System::Word GL_DISTANCE_ATTENUATION_ARB = System::Word(0x8129);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_UNITS_ARB = System::Word(0x86a4);
static _DELPHI_CONST System::Word GL_ACTIVE_VERTEX_UNITS_ARB = System::Word(0x86a5);
static _DELPHI_CONST System::Word GL_WEIGHT_SUM_UNITY_ARB = System::Word(0x86a6);
static _DELPHI_CONST System::Word GL_VERTEX_BLEND_ARB = System::Word(0x86a7);
static _DELPHI_CONST System::Word GL_CURRENT_WEIGHT_ARB = System::Word(0x86a8);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_TYPE_ARB = System::Word(0x86a9);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_STRIDE_ARB = System::Word(0x86aa);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_SIZE_ARB = System::Word(0x86ab);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_POINTER_ARB = System::Word(0x86ac);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_ARB = System::Word(0x86ad);
static _DELPHI_CONST System::Word GL_MODELVIEW0_ARB = System::Word(0x1700);
static _DELPHI_CONST System::Word GL_MODELVIEW1_ARB = System::Word(0x850a);
static _DELPHI_CONST System::Word GL_MODELVIEW2_ARB = System::Word(0x8722);
static _DELPHI_CONST System::Word GL_MODELVIEW3_ARB = System::Word(0x8723);
static _DELPHI_CONST System::Word GL_MODELVIEW4_ARB = System::Word(0x8724);
static _DELPHI_CONST System::Word GL_MODELVIEW5_ARB = System::Word(0x8725);
static _DELPHI_CONST System::Word GL_MODELVIEW6_ARB = System::Word(0x8726);
static _DELPHI_CONST System::Word GL_MODELVIEW7_ARB = System::Word(0x8727);
static _DELPHI_CONST System::Word GL_MODELVIEW8_ARB = System::Word(0x8728);
static _DELPHI_CONST System::Word GL_MODELVIEW9_ARB = System::Word(0x8729);
static _DELPHI_CONST System::Word GL_MODELVIEW10_ARB = System::Word(0x872a);
static _DELPHI_CONST System::Word GL_MODELVIEW11_ARB = System::Word(0x872b);
static _DELPHI_CONST System::Word GL_MODELVIEW12_ARB = System::Word(0x872c);
static _DELPHI_CONST System::Word GL_MODELVIEW13_ARB = System::Word(0x872d);
static _DELPHI_CONST System::Word GL_MODELVIEW14_ARB = System::Word(0x872e);
static _DELPHI_CONST System::Word GL_MODELVIEW15_ARB = System::Word(0x872f);
static _DELPHI_CONST System::Word GL_MODELVIEW16_ARB = System::Word(0x8730);
static _DELPHI_CONST System::Word GL_MODELVIEW17_ARB = System::Word(0x8731);
static _DELPHI_CONST System::Word GL_MODELVIEW18_ARB = System::Word(0x8732);
static _DELPHI_CONST System::Word GL_MODELVIEW19_ARB = System::Word(0x8733);
static _DELPHI_CONST System::Word GL_MODELVIEW20_ARB = System::Word(0x8734);
static _DELPHI_CONST System::Word GL_MODELVIEW21_ARB = System::Word(0x8735);
static _DELPHI_CONST System::Word GL_MODELVIEW22_ARB = System::Word(0x8736);
static _DELPHI_CONST System::Word GL_MODELVIEW23_ARB = System::Word(0x8737);
static _DELPHI_CONST System::Word GL_MODELVIEW24_ARB = System::Word(0x8738);
static _DELPHI_CONST System::Word GL_MODELVIEW25_ARB = System::Word(0x8739);
static _DELPHI_CONST System::Word GL_MODELVIEW26_ARB = System::Word(0x873a);
static _DELPHI_CONST System::Word GL_MODELVIEW27_ARB = System::Word(0x873b);
static _DELPHI_CONST System::Word GL_MODELVIEW28_ARB = System::Word(0x873c);
static _DELPHI_CONST System::Word GL_MODELVIEW29_ARB = System::Word(0x873d);
static _DELPHI_CONST System::Word GL_MODELVIEW30_ARB = System::Word(0x873e);
static _DELPHI_CONST System::Word GL_MODELVIEW31_ARB = System::Word(0x873f);
static _DELPHI_CONST System::Word GL_MATRIX_PALETTE_ARB = System::Word(0x8840);
static _DELPHI_CONST System::Word GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = System::Word(0x8841);
static _DELPHI_CONST System::Word GL_MAX_PALETTE_MATRICES_ARB = System::Word(0x8842);
static _DELPHI_CONST System::Word GL_CURRENT_PALETTE_MATRIX_ARB = System::Word(0x8843);
static _DELPHI_CONST System::Word GL_MATRIX_INDEX_ARRAY_ARB = System::Word(0x8844);
static _DELPHI_CONST System::Word GL_CURRENT_MATRIX_INDEX_ARB = System::Word(0x8845);
static _DELPHI_CONST System::Word GL_MATRIX_INDEX_ARRAY_SIZE_ARB = System::Word(0x8846);
static _DELPHI_CONST System::Word GL_MATRIX_INDEX_ARRAY_TYPE_ARB = System::Word(0x8847);
static _DELPHI_CONST System::Word GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = System::Word(0x8848);
static _DELPHI_CONST System::Word GL_MATRIX_INDEX_ARRAY_POINTER_ARB = System::Word(0x8849);
static _DELPHI_CONST System::Word GL_COMBINE_ARB = System::Word(0x8570);
static _DELPHI_CONST System::Word GL_COMBINE_RGB_ARB = System::Word(0x8571);
static _DELPHI_CONST System::Word GL_COMBINE_ALPHA_ARB = System::Word(0x8572);
static _DELPHI_CONST System::Word GL_RGB_SCALE_ARB = System::Word(0x8573);
static _DELPHI_CONST System::Word GL_ADD_SIGNED_ARB = System::Word(0x8574);
static _DELPHI_CONST System::Word GL_INTERPOLATE_ARB = System::Word(0x8575);
static _DELPHI_CONST System::Word GL_CONSTANT_ARB = System::Word(0x8576);
static _DELPHI_CONST System::Word GL_CONSTANT_COLOR_ARB = System::Word(0x8576);
static _DELPHI_CONST System::Word GL_PRIMARY_COLOR_ARB = System::Word(0x8577);
static _DELPHI_CONST System::Word GL_PREVIOUS_ARB = System::Word(0x8578);
static _DELPHI_CONST System::Word GL_SOURCE0_RGB_ARB = System::Word(0x8580);
static _DELPHI_CONST System::Word GL_SOURCE1_RGB_ARB = System::Word(0x8581);
static _DELPHI_CONST System::Word GL_SOURCE2_RGB_ARB = System::Word(0x8582);
static _DELPHI_CONST System::Word GL_SOURCE0_ALPHA_ARB = System::Word(0x8588);
static _DELPHI_CONST System::Word GL_SOURCE1_ALPHA_ARB = System::Word(0x8589);
static _DELPHI_CONST System::Word GL_SOURCE2_ALPHA_ARB = System::Word(0x858a);
static _DELPHI_CONST System::Word GL_OPERAND0_RGB_ARB = System::Word(0x8590);
static _DELPHI_CONST System::Word GL_OPERAND1_RGB_ARB = System::Word(0x8591);
static _DELPHI_CONST System::Word GL_OPERAND2_RGB_ARB = System::Word(0x8592);
static _DELPHI_CONST System::Word GL_OPERAND0_ALPHA_ARB = System::Word(0x8598);
static _DELPHI_CONST System::Word GL_OPERAND1_ALPHA_ARB = System::Word(0x8599);
static _DELPHI_CONST System::Word GL_OPERAND2_ALPHA_ARB = System::Word(0x859a);
static _DELPHI_CONST System::Word GL_SUBTRACT_ARB = System::Word(0x84e7);
static _DELPHI_CONST System::Word GL_DOT3_RGB_ARB = System::Word(0x86ae);
static _DELPHI_CONST System::Word GL_DOT3_RGBA_ARB = System::Word(0x86af);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RGB_ARB = System::Word(0x2070);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RGBA_ARB = System::Word(0x2071);
static _DELPHI_CONST System::Word WGL_TEXTURE_FORMAT_ARB = System::Word(0x2072);
static _DELPHI_CONST System::Word WGL_TEXTURE_TARGET_ARB = System::Word(0x2073);
static _DELPHI_CONST System::Word WGL_MIPMAP_TEXTURE_ARB = System::Word(0x2074);
static _DELPHI_CONST System::Word WGL_TEXTURE_RGB_ARB = System::Word(0x2075);
static _DELPHI_CONST System::Word WGL_TEXTURE_RGBA_ARB = System::Word(0x2076);
static _DELPHI_CONST System::Word WGL_NO_TEXTURE_ARB = System::Word(0x2077);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_ARB = System::Word(0x2078);
static _DELPHI_CONST System::Word WGL_TEXTURE_1D_ARB = System::Word(0x2079);
static _DELPHI_CONST System::Word WGL_TEXTURE_2D_ARB = System::Word(0x207a);
static _DELPHI_CONST System::Word WGL_MIPMAP_LEVEL_ARB = System::Word(0x207b);
static _DELPHI_CONST System::Word WGL_CUBE_MAP_FACE_ARB = System::Word(0x207c);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = System::Word(0x207d);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = System::Word(0x207e);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = System::Word(0x207f);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = System::Word(0x2080);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = System::Word(0x2081);
static _DELPHI_CONST System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = System::Word(0x2082);
static _DELPHI_CONST System::Word WGL_FRONT_LEFT_ARB = System::Word(0x2083);
static _DELPHI_CONST System::Word WGL_FRONT_RIGHT_ARB = System::Word(0x2084);
static _DELPHI_CONST System::Word WGL_BACK_LEFT_ARB = System::Word(0x2085);
static _DELPHI_CONST System::Word WGL_BACK_RIGHT_ARB = System::Word(0x2086);
static _DELPHI_CONST System::Word WGL_AUX0_ARB = System::Word(0x2087);
static _DELPHI_CONST System::Word WGL_AUX1_ARB = System::Word(0x2088);
static _DELPHI_CONST System::Word WGL_AUX2_ARB = System::Word(0x2089);
static _DELPHI_CONST System::Word WGL_AUX3_ARB = System::Word(0x208a);
static _DELPHI_CONST System::Word WGL_AUX4_ARB = System::Word(0x208b);
static _DELPHI_CONST System::Word WGL_AUX5_ARB = System::Word(0x208c);
static _DELPHI_CONST System::Word WGL_AUX6_ARB = System::Word(0x208d);
static _DELPHI_CONST System::Word WGL_AUX7_ARB = System::Word(0x208e);
static _DELPHI_CONST System::Word WGL_AUX8_ARB = System::Word(0x208f);
static _DELPHI_CONST System::Word WGL_AUX9_ARB = System::Word(0x2090);
static _DELPHI_CONST System::Word GL_MIRRORED_REPEAT_ARB = System::Word(0x8370);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT16_ARB = System::Word(0x81a5);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT24_ARB = System::Word(0x81a6);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT32_ARB = System::Word(0x81a7);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH_SIZE_ARB = System::Word(0x884a);
static _DELPHI_CONST System::Word GL_DEPTH_TEXTURE_MODE_ARB = System::Word(0x884b);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_MODE_ARB = System::Word(0x884c);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_FUNC_ARB = System::Word(0x884d);
static _DELPHI_CONST System::Word GL_COMPARE_R_TO_TEXTURE_ARB = System::Word(0x884e);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = System::Word(0x80bf);
static _DELPHI_CONST System::Word GL_COLOR_SUM_ARB = System::Word(0x8458);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_ARB = System::Word(0x8620);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = System::Word(0x8622);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB = System::Word(0x8623);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = System::Word(0x8624);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB = System::Word(0x8625);
static _DELPHI_CONST System::Word GL_CURRENT_VERTEX_ATTRIB_ARB = System::Word(0x8626);
static _DELPHI_CONST System::Word GL_PROGRAM_LENGTH_ARB = System::Word(0x8627);
static _DELPHI_CONST System::Word GL_PROGRAM_STRING_ARB = System::Word(0x8628);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = System::Word(0x862e);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_MATRICES_ARB = System::Word(0x862f);
static _DELPHI_CONST System::Word GL_CURRENT_MATRIX_STACK_DEPTH_ARB = System::Word(0x8640);
static _DELPHI_CONST System::Word GL_CURRENT_MATRIX_ARB = System::Word(0x8641);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_POINT_SIZE_ARB = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_TWO_SIDE_ARB = System::Word(0x8643);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = System::Word(0x8645);
static _DELPHI_CONST System::Word GL_PROGRAM_ERROR_POSITION_ARB = System::Word(0x864b);
static _DELPHI_CONST System::Word GL_PROGRAM_BINDING_ARB = System::Word(0x8677);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATTRIBS_ARB = System::Word(0x8869);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = System::Word(0x886a);
static _DELPHI_CONST System::Word GL_PROGRAM_ERROR_STRING_ARB = System::Word(0x8874);
static _DELPHI_CONST System::Word GL_PROGRAM_FORMAT_ASCII_ARB = System::Word(0x8875);
static _DELPHI_CONST System::Word GL_PROGRAM_FORMAT_ARB = System::Word(0x8876);
static _DELPHI_CONST System::Word GL_PROGRAM_INSTRUCTIONS_ARB = System::Word(0x88a0);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_INSTRUCTIONS_ARB = System::Word(0x88a1);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = System::Word(0x88a2);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = System::Word(0x88a3);
static _DELPHI_CONST System::Word GL_PROGRAM_TEMPORARIES_ARB = System::Word(0x88a4);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEMPORARIES_ARB = System::Word(0x88a5);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_TEMPORARIES_ARB = System::Word(0x88a6);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = System::Word(0x88a7);
static _DELPHI_CONST System::Word GL_PROGRAM_PARAMETERS_ARB = System::Word(0x88a8);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_PARAMETERS_ARB = System::Word(0x88a9);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_PARAMETERS_ARB = System::Word(0x88aa);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = System::Word(0x88ab);
static _DELPHI_CONST System::Word GL_PROGRAM_ATTRIBS_ARB = System::Word(0x88ac);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_ATTRIBS_ARB = System::Word(0x88ad);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_ATTRIBS_ARB = System::Word(0x88ae);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = System::Word(0x88af);
static _DELPHI_CONST System::Word GL_PROGRAM_ADDRESS_REGISTERS_ARB = System::Word(0x88b0);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = System::Word(0x88b1);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = System::Word(0x88b2);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = System::Word(0x88b3);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = System::Word(0x88b4);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = System::Word(0x88b5);
static _DELPHI_CONST System::Word GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = System::Word(0x88b6);
static _DELPHI_CONST System::Word GL_TRANSPOSE_CURRENT_MATRIX_ARB = System::Word(0x88b7);
static _DELPHI_CONST System::Word GL_MATRIX0_ARB = System::Word(0x88c0);
static _DELPHI_CONST System::Word GL_MATRIX1_ARB = System::Word(0x88c1);
static _DELPHI_CONST System::Word GL_MATRIX2_ARB = System::Word(0x88c2);
static _DELPHI_CONST System::Word GL_MATRIX3_ARB = System::Word(0x88c3);
static _DELPHI_CONST System::Word GL_MATRIX4_ARB = System::Word(0x88c4);
static _DELPHI_CONST System::Word GL_MATRIX5_ARB = System::Word(0x88c5);
static _DELPHI_CONST System::Word GL_MATRIX6_ARB = System::Word(0x88c6);
static _DELPHI_CONST System::Word GL_MATRIX7_ARB = System::Word(0x88c7);
static _DELPHI_CONST System::Word GL_MATRIX8_ARB = System::Word(0x88c8);
static _DELPHI_CONST System::Word GL_MATRIX9_ARB = System::Word(0x88c9);
static _DELPHI_CONST System::Word GL_MATRIX10_ARB = System::Word(0x88ca);
static _DELPHI_CONST System::Word GL_MATRIX11_ARB = System::Word(0x88cb);
static _DELPHI_CONST System::Word GL_MATRIX12_ARB = System::Word(0x88cc);
static _DELPHI_CONST System::Word GL_MATRIX13_ARB = System::Word(0x88cd);
static _DELPHI_CONST System::Word GL_MATRIX14_ARB = System::Word(0x88ce);
static _DELPHI_CONST System::Word GL_MATRIX15_ARB = System::Word(0x88cf);
static _DELPHI_CONST System::Word GL_MATRIX16_ARB = System::Word(0x88d0);
static _DELPHI_CONST System::Word GL_MATRIX17_ARB = System::Word(0x88d1);
static _DELPHI_CONST System::Word GL_MATRIX18_ARB = System::Word(0x88d2);
static _DELPHI_CONST System::Word GL_MATRIX19_ARB = System::Word(0x88d3);
static _DELPHI_CONST System::Word GL_MATRIX20_ARB = System::Word(0x88d4);
static _DELPHI_CONST System::Word GL_MATRIX21_ARB = System::Word(0x88d5);
static _DELPHI_CONST System::Word GL_MATRIX22_ARB = System::Word(0x88d6);
static _DELPHI_CONST System::Word GL_MATRIX23_ARB = System::Word(0x88d7);
static _DELPHI_CONST System::Word GL_MATRIX24_ARB = System::Word(0x88d8);
static _DELPHI_CONST System::Word GL_MATRIX25_ARB = System::Word(0x88d9);
static _DELPHI_CONST System::Word GL_MATRIX26_ARB = System::Word(0x88da);
static _DELPHI_CONST System::Word GL_MATRIX27_ARB = System::Word(0x88db);
static _DELPHI_CONST System::Word GL_MATRIX28_ARB = System::Word(0x88dc);
static _DELPHI_CONST System::Word GL_MATRIX29_ARB = System::Word(0x88dd);
static _DELPHI_CONST System::Word GL_MATRIX30_ARB = System::Word(0x88de);
static _DELPHI_CONST System::Word GL_MATRIX31_ARB = System::Word(0x88df);
static _DELPHI_CONST System::Word GL_FRAGMENT_PROGRAM_ARB = System::Word(0x8804);
static _DELPHI_CONST System::Word GL_PROGRAM_ALU_INSTRUCTIONS_ARB = System::Word(0x8805);
static _DELPHI_CONST System::Word GL_PROGRAM_TEX_INSTRUCTIONS_ARB = System::Word(0x8806);
static _DELPHI_CONST System::Word GL_PROGRAM_TEX_INDIRECTIONS_ARB = System::Word(0x8807);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = System::Word(0x8808);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = System::Word(0x8809);
static _DELPHI_CONST System::Word GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = System::Word(0x880a);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB = System::Word(0x880b);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB = System::Word(0x880c);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB = System::Word(0x880d);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = System::Word(0x880e);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = System::Word(0x880f);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = System::Word(0x8810);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_COORDS_ARB = System::Word(0x8871);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_IMAGE_UNITS_ARB = System::Word(0x8872);
static _DELPHI_CONST System::Word GL_BUFFER_SIZE_ARB = System::Word(0x8764);
static _DELPHI_CONST System::Word GL_BUFFER_USAGE_ARB = System::Word(0x8765);
static _DELPHI_CONST System::Word GL_ARRAY_BUFFER_ARB = System::Word(0x8892);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_BUFFER_ARB = System::Word(0x8893);
static _DELPHI_CONST System::Word GL_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8894);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8895);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8896);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8897);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8898);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_BUFFER_BINDING_ARB = System::Word(0x8899);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889a);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889b);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889c);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889d);
static _DELPHI_CONST System::Word GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889e);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = System::Word(0x889f);
static _DELPHI_CONST System::Word GL_READ_ONLY_ARB = System::Word(0x88b8);
static _DELPHI_CONST System::Word GL_WRITE_ONLY_ARB = System::Word(0x88b9);
static _DELPHI_CONST System::Word GL_READ_WRITE_ARB = System::Word(0x88ba);
static _DELPHI_CONST System::Word GL_BUFFER_ACCESS_ARB = System::Word(0x88bb);
static _DELPHI_CONST System::Word GL_BUFFER_MAPPED_ARB = System::Word(0x88bc);
static _DELPHI_CONST System::Word GL_BUFFER_MAP_POINTER_ARB = System::Word(0x88bd);
static _DELPHI_CONST System::Word GL_STREAM_DRAW_ARB = System::Word(0x88e0);
static _DELPHI_CONST System::Word GL_STREAM_READ_ARB = System::Word(0x88e1);
static _DELPHI_CONST System::Word GL_STREAM_COPY_ARB = System::Word(0x88e2);
static _DELPHI_CONST System::Word GL_STATIC_DRAW_ARB = System::Word(0x88e4);
static _DELPHI_CONST System::Word GL_STATIC_READ_ARB = System::Word(0x88e5);
static _DELPHI_CONST System::Word GL_STATIC_COPY_ARB = System::Word(0x88e6);
static _DELPHI_CONST System::Word GL_DYNAMIC_DRAW_ARB = System::Word(0x88e8);
static _DELPHI_CONST System::Word GL_DYNAMIC_READ_ARB = System::Word(0x88e9);
static _DELPHI_CONST System::Word GL_DYNAMIC_COPY_ARB = System::Word(0x88ea);
static _DELPHI_CONST System::Word GL_QUERY_COUNTER_BITS_ARB = System::Word(0x8864);
static _DELPHI_CONST System::Word GL_CURRENT_QUERY_ARB = System::Word(0x8865);
static _DELPHI_CONST System::Word GL_QUERY_RESULT_ARB = System::Word(0x8866);
static _DELPHI_CONST System::Word GL_QUERY_RESULT_AVAILABLE_ARB = System::Word(0x8867);
static _DELPHI_CONST System::Word GL_SAMPLES_PASSED_ARB = System::Word(0x8914);
static _DELPHI_CONST System::Word GL_PROGRAM_OBJECT_ARB = System::Word(0x8b40);
static _DELPHI_CONST System::Word GL_SHADER_OBJECT_ARB = System::Word(0x8b48);
static _DELPHI_CONST System::Word GL_OBJECT_TYPE_ARB = System::Word(0x8b4e);
static _DELPHI_CONST System::Word GL_OBJECT_SUBTYPE_ARB = System::Word(0x8b4f);
static _DELPHI_CONST System::Word GL_FLOAT_VEC2_ARB = System::Word(0x8b50);
static _DELPHI_CONST System::Word GL_FLOAT_VEC3_ARB = System::Word(0x8b51);
static _DELPHI_CONST System::Word GL_FLOAT_VEC4_ARB = System::Word(0x8b52);
static _DELPHI_CONST System::Word GL_INT_VEC2_ARB = System::Word(0x8b53);
static _DELPHI_CONST System::Word GL_INT_VEC3_ARB = System::Word(0x8b54);
static _DELPHI_CONST System::Word GL_INT_VEC4_ARB = System::Word(0x8b55);
static _DELPHI_CONST System::Word GL_BOOL_ARB = System::Word(0x8b56);
static _DELPHI_CONST System::Word GL_BOOL_VEC2_ARB = System::Word(0x8b57);
static _DELPHI_CONST System::Word GL_BOOL_VEC3_ARB = System::Word(0x8b58);
static _DELPHI_CONST System::Word GL_BOOL_VEC4_ARB = System::Word(0x8b59);
static _DELPHI_CONST System::Word GL_FLOAT_MAT2_ARB = System::Word(0x8b5a);
static _DELPHI_CONST System::Word GL_FLOAT_MAT3_ARB = System::Word(0x8b5b);
static _DELPHI_CONST System::Word GL_FLOAT_MAT4_ARB = System::Word(0x8b5c);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_ARB = System::Word(0x8b5d);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_ARB = System::Word(0x8b5e);
static _DELPHI_CONST System::Word GL_SAMPLER_3D_ARB = System::Word(0x8b5f);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_ARB = System::Word(0x8b60);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_SHADOW_ARB = System::Word(0x8b61);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_SHADOW_ARB = System::Word(0x8b62);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_RECT_ARB = System::Word(0x8b63);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_RECT_SHADOW_ARB = System::Word(0x8b64);
static _DELPHI_CONST System::Word GL_OBJECT_DELETE_STATUS_ARB = System::Word(0x8b80);
static _DELPHI_CONST System::Word GL_OBJECT_COMPILE_STATUS_ARB = System::Word(0x8b81);
static _DELPHI_CONST System::Word GL_OBJECT_LINK_STATUS_ARB = System::Word(0x8b82);
static _DELPHI_CONST System::Word GL_OBJECT_VALIDATE_STATUS_ARB = System::Word(0x8b83);
static _DELPHI_CONST System::Word GL_OBJECT_INFO_LOG_LENGTH_ARB = System::Word(0x8b84);
static _DELPHI_CONST System::Word GL_OBJECT_ATTACHED_OBJECTS_ARB = System::Word(0x8b85);
static _DELPHI_CONST System::Word GL_OBJECT_ACTIVE_UNIFORMS_ARB = System::Word(0x8b86);
static _DELPHI_CONST System::Word GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = System::Word(0x8b87);
static _DELPHI_CONST System::Word GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = System::Word(0x8b88);
static _DELPHI_CONST System::Word GL_VERTEX_SHADER_ARB = System::Word(0x8b31);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = System::Word(0x8b4a);
static _DELPHI_CONST System::Word GL_MAX_VARYING_FLOATS_ARB = System::Word(0x8b4b);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = System::Word(0x8b4c);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = System::Word(0x8b4d);
static _DELPHI_CONST System::Word GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = System::Word(0x8b89);
static _DELPHI_CONST System::Word GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = System::Word(0x8b8a);
static _DELPHI_CONST System::Word GL_FRAGMENT_SHADER_ARB = System::Word(0x8b30);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = System::Word(0x8b49);
static _DELPHI_CONST System::Word GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = System::Word(0x8b8b);
static _DELPHI_CONST System::Word GL_SHADING_LANGUAGE_VERSION_ARB = System::Word(0x8b8c);
static _DELPHI_CONST System::Word GL_POINT_SPRITE_ARB = System::Word(0x8861);
static _DELPHI_CONST System::Word GL_COORD_REPLACE_ARB = System::Word(0x8862);
static _DELPHI_CONST System::Word GL_MAX_DRAW_BUFFERS_ARB = System::Word(0x8824);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER0_ARB = System::Word(0x8825);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER1_ARB = System::Word(0x8826);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER2_ARB = System::Word(0x8827);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER3_ARB = System::Word(0x8828);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER4_ARB = System::Word(0x8829);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER5_ARB = System::Word(0x882a);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER6_ARB = System::Word(0x882b);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER7_ARB = System::Word(0x882c);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER8_ARB = System::Word(0x882d);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER9_ARB = System::Word(0x882e);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER10_ARB = System::Word(0x882f);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER11_ARB = System::Word(0x8830);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER12_ARB = System::Word(0x8831);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER13_ARB = System::Word(0x8832);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER14_ARB = System::Word(0x8833);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER15_ARB = System::Word(0x8834);
static _DELPHI_CONST System::Word GL_TEXTURE_RECTANGLE_ARB = System::Word(0x84f5);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_RECTANGLE_ARB = System::Word(0x84f6);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_RECTANGLE_ARB = System::Word(0x84f7);
static _DELPHI_CONST System::Word GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = System::Word(0x84f8);
static _DELPHI_CONST System::Word GL_RGBA_FLOAT_MODE_ARB = System::Word(0x8820);
static _DELPHI_CONST System::Word GL_CLAMP_VERTEX_COLOR_ARB = System::Word(0x891a);
static _DELPHI_CONST System::Word GL_CLAMP_FRAGMENT_COLOR_ARB = System::Word(0x891b);
static _DELPHI_CONST System::Word GL_CLAMP_READ_COLOR_ARB = System::Word(0x891c);
static _DELPHI_CONST System::Word GL_FIXED_ONLY_ARB = System::Word(0x891d);
static _DELPHI_CONST System::Word WGL_TYPE_RGBA_FLOAT_ARB = System::Word(0x21a0);
static _DELPHI_CONST System::Word GLX_RGBA_FLOAT_TYPE_ARB = System::Word(0x20b9);
static _DELPHI_CONST System::Int8 GLX_RGBA_FLOAT_BIT_ARB = System::Int8(0x4);
static _DELPHI_CONST System::Word GL_HALF_FLOAT_ARB = System::Word(0x140b);
static _DELPHI_CONST System::Word GL_TEXTURE_RED_TYPE_ARB = System::Word(0x8c10);
static _DELPHI_CONST System::Word GL_TEXTURE_GREEN_TYPE_ARB = System::Word(0x8c11);
static _DELPHI_CONST System::Word GL_TEXTURE_BLUE_TYPE_ARB = System::Word(0x8c12);
static _DELPHI_CONST System::Word GL_TEXTURE_ALPHA_TYPE_ARB = System::Word(0x8c13);
static _DELPHI_CONST System::Word GL_TEXTURE_LUMINANCE_TYPE_ARB = System::Word(0x8c14);
static _DELPHI_CONST System::Word GL_TEXTURE_INTENSITY_TYPE_ARB = System::Word(0x8c15);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH_TYPE_ARB = System::Word(0x8c16);
static _DELPHI_CONST System::Word GL_UNSIGNED_NORMALIZED_ARB = System::Word(0x8c17);
static _DELPHI_CONST System::Word GL_RGBA32F_ARB = System::Word(0x8814);
static _DELPHI_CONST System::Word GL_RGB32F_ARB = System::Word(0x8815);
static _DELPHI_CONST System::Word GL_ALPHA32F_ARB = System::Word(0x8816);
static _DELPHI_CONST System::Word GL_INTENSITY32F_ARB = System::Word(0x8817);
static _DELPHI_CONST System::Word GL_LUMINANCE32F_ARB = System::Word(0x8818);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA32F_ARB = System::Word(0x8819);
static _DELPHI_CONST System::Word GL_RGBA16F_ARB = System::Word(0x881a);
static _DELPHI_CONST System::Word GL_RGB16F_ARB = System::Word(0x881b);
static _DELPHI_CONST System::Word GL_ALPHA16F_ARB = System::Word(0x881c);
static _DELPHI_CONST System::Word GL_INTENSITY16F_ARB = System::Word(0x881d);
static _DELPHI_CONST System::Word GL_LUMINANCE16F_ARB = System::Word(0x881e);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA16F_ARB = System::Word(0x881f);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER_ARB = System::Word(0x88eb);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER_ARB = System::Word(0x88ec);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER_BINDING_ARB = System::Word(0x88ed);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER_BINDING_ARB = System::Word(0x88ef);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENT32F = System::Word(0x8cac);
static _DELPHI_CONST System::Word GL_DEPTH32F_STENCIL8 = System::Word(0x8cad);
static _DELPHI_CONST System::Word GL_FLOAT_32_UNSIGNED_INT_24_8_REV = System::Word(0x8dad);
static _DELPHI_CONST System::Word GL_INVALID_FRAMEBUFFER_OPERATION = System::Word(0x506);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = System::Word(0x8210);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = System::Word(0x8211);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = System::Word(0x8212);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = System::Word(0x8213);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = System::Word(0x8214);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = System::Word(0x8215);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = System::Word(0x8216);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = System::Word(0x8217);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT = System::Word(0x8218);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_UNDEFINED = System::Word(0x8219);
static _DELPHI_CONST System::Word GL_DEPTH_STENCIL_ATTACHMENT = System::Word(0x821a);
static _DELPHI_CONST System::Word GL_INDEX = System::Word(0x8222);
static _DELPHI_CONST System::Word GL_MAX_RENDERBUFFER_SIZE = System::Word(0x84e8);
static _DELPHI_CONST System::Word GL_DEPTH_STENCIL = System::Word(0x84f9);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_24_8 = System::Word(0x84fa);
static _DELPHI_CONST System::Word GL_DEPTH24_STENCIL8 = System::Word(0x88f0);
static _DELPHI_CONST System::Word GL_TEXTURE_STENCIL_SIZE = System::Word(0x88f1);
static _DELPHI_CONST System::Word GL_TEXTURE_RED_TYPE = System::Word(0x8c10);
static _DELPHI_CONST System::Word GL_TEXTURE_GREEN_TYPE = System::Word(0x8c11);
static _DELPHI_CONST System::Word GL_TEXTURE_BLUE_TYPE = System::Word(0x8c12);
static _DELPHI_CONST System::Word GL_TEXTURE_ALPHA_TYPE = System::Word(0x8c13);
static _DELPHI_CONST System::Word GL_TEXTURE_LUMINANCE_TYPE = System::Word(0x8c14);
static _DELPHI_CONST System::Word GL_TEXTURE_INTENSITY_TYPE = System::Word(0x8c15);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH_TYPE = System::Word(0x8c16);
static _DELPHI_CONST System::Word GL_UNSIGNED_NORMALIZED = System::Word(0x8c17);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_BINDING = System::Word(0x8ca6);
static _DELPHI_CONST System::Word GL_DRAW_FRAMEBUFFER_BINDING = System::Word(0x8ca6);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_BINDING = System::Word(0x8ca7);
static _DELPHI_CONST System::Word GL_READ_FRAMEBUFFER = System::Word(0x8ca8);
static _DELPHI_CONST System::Word GL_DRAW_FRAMEBUFFER = System::Word(0x8ca9);
static _DELPHI_CONST System::Word GL_READ_FRAMEBUFFER_BINDING = System::Word(0x8caa);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_SAMPLES = System::Word(0x8cab);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = System::Word(0x8cd0);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = System::Word(0x8cd1);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = System::Word(0x8cd2);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = System::Word(0x8cd3);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = System::Word(0x8cd4);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_COMPLETE = System::Word(0x8cd5);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = System::Word(0x8cd6);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = System::Word(0x8cd7);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = System::Word(0x8cdb);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = System::Word(0x8cdc);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_UNSUPPORTED = System::Word(0x8cdd);
static _DELPHI_CONST System::Word GL_MAX_COLOR_ATTACHMENTS = System::Word(0x8cdf);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT0 = System::Word(0x8ce0);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT1 = System::Word(0x8ce1);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT2 = System::Word(0x8ce2);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT3 = System::Word(0x8ce3);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT4 = System::Word(0x8ce4);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT5 = System::Word(0x8ce5);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT6 = System::Word(0x8ce6);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT7 = System::Word(0x8ce7);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT8 = System::Word(0x8ce8);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT9 = System::Word(0x8ce9);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT10 = System::Word(0x8cea);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT11 = System::Word(0x8ceb);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT12 = System::Word(0x8cec);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT13 = System::Word(0x8ced);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT14 = System::Word(0x8cee);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT15 = System::Word(0x8cef);
static _DELPHI_CONST System::Word GL_DEPTH_ATTACHMENT = System::Word(0x8d00);
static _DELPHI_CONST System::Word GL_STENCIL_ATTACHMENT = System::Word(0x8d20);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER = System::Word(0x8d40);
static _DELPHI_CONST System::Word GL_RENDERBUFFER = System::Word(0x8d41);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_WIDTH = System::Word(0x8d42);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_HEIGHT = System::Word(0x8d43);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_INTERNAL_FORMAT = System::Word(0x8d44);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX1 = System::Word(0x8d46);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX4 = System::Word(0x8d47);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX8 = System::Word(0x8d48);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX16 = System::Word(0x8d49);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_RED_SIZE = System::Word(0x8d50);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_GREEN_SIZE = System::Word(0x8d51);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_BLUE_SIZE = System::Word(0x8d52);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_ALPHA_SIZE = System::Word(0x8d53);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_DEPTH_SIZE = System::Word(0x8d54);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_STENCIL_SIZE = System::Word(0x8d55);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = System::Word(0x8d56);
static _DELPHI_CONST System::Word GL_MAX_SAMPLES = System::Word(0x8d57);
static _DELPHI_CONST System::Word GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB = System::Word(0x20b2);
static _DELPHI_CONST System::Word WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = System::Word(0x20a9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_SRGB = System::Word(0x8db9);
static _DELPHI_CONST System::Word GL_GEOMETRY_SHADER_ARB = System::Word(0x8dd9);
static _DELPHI_CONST System::Word GL_GEOMETRY_VERTICES_OUT_ARB = System::Word(0x8dda);
static _DELPHI_CONST System::Word GL_GEOMETRY_INPUT_TYPE_ARB = System::Word(0x8ddb);
static _DELPHI_CONST System::Word GL_GEOMETRY_OUTPUT_TYPE_ARB = System::Word(0x8ddc);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB = System::Word(0x8c29);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB = System::Word(0x8ddd);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_VARYING_COMPONENTS_ARB = System::Word(0x8dde);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB = System::Word(0x8ddf);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB = System::Word(0x8de0);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB = System::Word(0x8de1);
static _DELPHI_CONST System::Int8 GL_LINES_ADJACENCY_ARB = System::Int8(0xa);
static _DELPHI_CONST System::Int8 GL_LINE_STRIP_ADJACENCY_ARB = System::Int8(0xb);
static _DELPHI_CONST System::Int8 GL_TRIANGLES_ADJACENCY_ARB = System::Int8(0xc);
static _DELPHI_CONST System::Int8 GL_TRIANGLE_STRIP_ADJACENCY_ARB = System::Int8(0xd);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB = System::Word(0x8da8);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB = System::Word(0x8da9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB = System::Word(0x8da7);
static _DELPHI_CONST System::Word GL_PROGRAM_POINT_SIZE_ARB = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_HALF_FLOAT = System::Word(0x140b);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB = System::Word(0x88fe);
static _DELPHI_CONST System::Int8 GL_MAP_READ_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_MAP_WRITE_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_MAP_INVALIDATE_RANGE_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_MAP_INVALIDATE_BUFFER_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_MAP_FLUSH_EXPLICIT_BIT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GL_MAP_UNSYNCHRONIZED_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_ARB = System::Word(0x8c2a);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_BUFFER_SIZE_ARB = System::Word(0x8c2b);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_BUFFER_ARB = System::Word(0x8c2c);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB = System::Word(0x8c2d);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_FORMAT_ARB = System::Word(0x8c2e);
static _DELPHI_CONST System::Word GL_COMPRESSED_RED_RGTC1 = System::Word(0x8dbb);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_RED_RGTC1 = System::Word(0x8dbc);
static _DELPHI_CONST System::Word GL_COMPRESSED_RG_RGTC2 = System::Word(0x8dbd);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_RG_RGTC2 = System::Word(0x8dbe);
static _DELPHI_CONST System::Word GL_R8 = System::Word(0x8229);
static _DELPHI_CONST System::Word GL_R16 = System::Word(0x822a);
static _DELPHI_CONST System::Word GL_RG8 = System::Word(0x822b);
static _DELPHI_CONST System::Word GL_RG16 = System::Word(0x822c);
static _DELPHI_CONST System::Word GL_R16F = System::Word(0x822d);
static _DELPHI_CONST System::Word GL_R32F = System::Word(0x822e);
static _DELPHI_CONST System::Word GL_RG16F = System::Word(0x822f);
static _DELPHI_CONST System::Word GL_RG32F = System::Word(0x8230);
static _DELPHI_CONST System::Word GL_R8I = System::Word(0x8231);
static _DELPHI_CONST System::Word GL_R8UI = System::Word(0x8232);
static _DELPHI_CONST System::Word GL_R16I = System::Word(0x8233);
static _DELPHI_CONST System::Word GL_R16UI = System::Word(0x8234);
static _DELPHI_CONST System::Word GL_R32I = System::Word(0x8235);
static _DELPHI_CONST System::Word GL_R32UI = System::Word(0x8236);
static _DELPHI_CONST System::Word GL_RG8I = System::Word(0x8237);
static _DELPHI_CONST System::Word GL_RG8UI = System::Word(0x8238);
static _DELPHI_CONST System::Word GL_RG16I = System::Word(0x8239);
static _DELPHI_CONST System::Word GL_RG16UI = System::Word(0x823a);
static _DELPHI_CONST System::Word GL_RG32I = System::Word(0x823b);
static _DELPHI_CONST System::Word GL_RG32UI = System::Word(0x823c);
static _DELPHI_CONST System::Word GL_RG = System::Word(0x8227);
static _DELPHI_CONST System::Word GL_RG_INTEGER = System::Word(0x8228);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_BINDING = System::Word(0x85b5);
static _DELPHI_CONST System::Word WGL_CONTEXT_MAJOR_VERSION_ARB = System::Word(0x2091);
static _DELPHI_CONST System::Word WGL_CONTEXT_MINOR_VERSION_ARB = System::Word(0x2092);
static _DELPHI_CONST System::Word WGL_CONTEXT_LAYER_PLANE_ARB = System::Word(0x2093);
static _DELPHI_CONST System::Word WGL_CONTEXT_FLAGS_ARB = System::Word(0x2094);
static _DELPHI_CONST System::Int8 WGL_CONTEXT_DEBUG_BIT_ARB = System::Int8(0x1);
static _DELPHI_CONST System::Int8 WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = System::Int8(0x2);
static _DELPHI_CONST System::Word ERROR_INVALID_VERSION_ARB = System::Word(0x2095);
static _DELPHI_CONST System::Int8 WGL_CONTEXT_ES2_PROFILE_BIT_EXT = System::Int8(0x4);
static _DELPHI_CONST System::Word GLX_CONTEXT_MAJOR_VERSION_ARB = System::Word(0x2091);
static _DELPHI_CONST System::Word GLX_CONTEXT_MINOR_VERSION_ARB = System::Word(0x2092);
static _DELPHI_CONST System::Word GLX_CONTEXT_FLAGS_ARB = System::Word(0x2094);
static _DELPHI_CONST System::Int8 GLX_CONTEXT_DEBUG_BIT_ARB = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = System::Int8(0x2);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER = System::Word(0x8a11);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_BINDING = System::Word(0x8a28);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_START = System::Word(0x8a29);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_SIZE = System::Word(0x8a2a);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_UNIFORM_BLOCKS = System::Word(0x8a2b);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_UNIFORM_BLOCKS = System::Word(0x8a2c);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_UNIFORM_BLOCKS = System::Word(0x8a2d);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_UNIFORM_BLOCKS = System::Word(0x8a2e);
static _DELPHI_CONST System::Word GL_MAX_UNIFORM_BUFFER_BINDINGS = System::Word(0x8a2f);
static _DELPHI_CONST System::Word GL_MAX_UNIFORM_BLOCK_SIZE = System::Word(0x8a30);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = System::Word(0x8a31);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = System::Word(0x8a32);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = System::Word(0x8a33);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = System::Word(0x8a34);
static _DELPHI_CONST System::Word GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = System::Word(0x8a35);
static _DELPHI_CONST System::Word GL_ACTIVE_UNIFORM_BLOCKS = System::Word(0x8a36);
static _DELPHI_CONST System::Word GL_UNIFORM_TYPE = System::Word(0x8a37);
static _DELPHI_CONST System::Word GL_UNIFORM_SIZE = System::Word(0x8a38);
static _DELPHI_CONST System::Word GL_UNIFORM_NAME_LENGTH = System::Word(0x8a39);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_INDEX = System::Word(0x8a3a);
static _DELPHI_CONST System::Word GL_UNIFORM_OFFSET = System::Word(0x8a3b);
static _DELPHI_CONST System::Word GL_UNIFORM_ARRAY_STRIDE = System::Word(0x8a3c);
static _DELPHI_CONST System::Word GL_UNIFORM_MATRIX_STRIDE = System::Word(0x8a3d);
static _DELPHI_CONST System::Word GL_UNIFORM_IS_ROW_MAJOR = System::Word(0x8a3e);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_BINDING = System::Word(0x8a3f);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_DATA_SIZE = System::Word(0x8a40);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_NAME_LENGTH = System::Word(0x8a41);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = System::Word(0x8a42);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = System::Word(0x8a43);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = System::Word(0x8a44);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER = System::Word(0x8a45);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = System::Word(0x8a46);
static _DELPHI_CONST unsigned GL_INVALID_INDEX = unsigned(0xffffffff);
static _DELPHI_CONST System::Word GL_COPY_READ_BUFFER = System::Word(0x8f36);
static _DELPHI_CONST System::Word GL_COPY_WRITE_BUFFER = System::Word(0x8f37);
static _DELPHI_CONST System::Word GL_DEPTH_CLAMP = System::Word(0x864f);
static _DELPHI_CONST System::Word GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION = System::Word(0x8e4c);
static _DELPHI_CONST System::Word GL_FIRST_VERTEX_CONVENTION = System::Word(0x8e4d);
static _DELPHI_CONST System::Word GL_LAST_VERTEX_CONVENTION = System::Word(0x8e4e);
static _DELPHI_CONST System::Word GL_PROVOKING_VERTEX = System::Word(0x8e4f);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_SEAMLESS = System::Word(0x884f);
static _DELPHI_CONST System::Word GL_MAX_SERVER_WAIT_TIMEOUT = System::Word(0x9111);
static _DELPHI_CONST System::Word GL_OBJECT_TYPE = System::Word(0x9112);
static _DELPHI_CONST System::Word GL_SYNC_CONDITION = System::Word(0x9113);
static _DELPHI_CONST System::Word GL_SYNC_STATUS = System::Word(0x9114);
static _DELPHI_CONST System::Word GL_SYNC_FLAGS = System::Word(0x9115);
static _DELPHI_CONST System::Word GL_SYNC_FENCE = System::Word(0x9116);
static _DELPHI_CONST System::Word GL_SYNC_GPU_COMMANDS_COMPLETE = System::Word(0x9117);
static _DELPHI_CONST System::Word GL_UNSIGNALED = System::Word(0x9118);
static _DELPHI_CONST System::Word GL_SIGNALED = System::Word(0x9119);
static _DELPHI_CONST System::Word GL_ALREADY_SIGNALED = System::Word(0x911a);
static _DELPHI_CONST System::Word GL_TIMEOUT_EXPIRED = System::Word(0x911b);
static _DELPHI_CONST System::Word GL_CONDITION_SATISFIED = System::Word(0x911c);
static _DELPHI_CONST System::Word GL_WAIT_FAILED = System::Word(0x911d);
static _DELPHI_CONST System::Int8 GL_SYNC_FLUSH_COMMANDS_BIT = System::Int8(0x1);
static _DELPHI_CONST unsigned __int64 GL_TIMEOUT_IGNORED = 0xffffffffffffffffULL;
static _DELPHI_CONST System::Word GL_SAMPLE_POSITION = System::Word(0x8e50);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK = System::Word(0x8e51);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_VALUE = System::Word(0x8e52);
static _DELPHI_CONST System::Word GL_MAX_SAMPLE_MASK_WORDS = System::Word(0x8e59);
static _DELPHI_CONST System::Word GL_TEXTURE_2D_MULTISAMPLE = System::Word(0x9100);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D_MULTISAMPLE = System::Word(0x9101);
static _DELPHI_CONST System::Word GL_TEXTURE_2D_MULTISAMPLE_ARRAY = System::Word(0x9102);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY = System::Word(0x9103);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_2D_MULTISAMPLE = System::Word(0x9104);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = System::Word(0x9105);
static _DELPHI_CONST System::Word GL_TEXTURE_SAMPLES = System::Word(0x9106);
static _DELPHI_CONST System::Word GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = System::Word(0x9107);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_MULTISAMPLE = System::Word(0x9108);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_MULTISAMPLE = System::Word(0x9109);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = System::Word(0x910a);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_MULTISAMPLE_ARRAY = System::Word(0x910b);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = System::Word(0x910c);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = System::Word(0x910d);
static _DELPHI_CONST System::Word GL_MAX_COLOR_TEXTURE_SAMPLES = System::Word(0x910e);
static _DELPHI_CONST System::Word GL_MAX_DEPTH_TEXTURE_SAMPLES = System::Word(0x910f);
static _DELPHI_CONST System::Word GL_MAX_INTEGER_SAMPLES = System::Word(0x9110);
static _DELPHI_CONST System::Word GL_SAMPLE_SHADING_ARB = System::Word(0x8c36);
static _DELPHI_CONST System::Word GL_MIN_SAMPLE_SHADING_VALUE_ARB = System::Word(0x8c37);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_ARRAY_ARB = System::Word(0x9009);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB = System::Word(0x900a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB = System::Word(0x900b);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_MAP_ARRAY_ARB = System::Word(0x900c);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB = System::Word(0x900d);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = System::Word(0x900e);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = System::Word(0x900f);
static _DELPHI_CONST System::Word GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = System::Word(0x8e5e);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = System::Word(0x8e5f);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB = System::Word(0x8f9f);
static _DELPHI_CONST System::Word WGL_CONTEXT_PROFILE_MASK_ARB = System::Word(0x9126);
static _DELPHI_CONST System::Int8 WGL_CONTEXT_CORE_PROFILE_BIT_ARB = System::Int8(0x1);
static _DELPHI_CONST System::Int8 WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = System::Int8(0x2);
static _DELPHI_CONST System::Word ERROR_INVALID_PROFILE_ARB = System::Word(0x2096);
static _DELPHI_CONST System::Word GLX_CONTEXT_PROFILE_MASK_ARB = System::Word(0x9126);
static _DELPHI_CONST System::Int8 GLX_CONTEXT_CORE_PROFILE_BIT_ARB = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = System::Int8(0x2);
static _DELPHI_CONST System::Word GL_SRC1_COLOR = System::Word(0x88f9);
static _DELPHI_CONST System::Word GL_ONE_MINUS_SRC1_COLOR = System::Word(0x88fa);
static _DELPHI_CONST System::Word GL_ONE_MINUS_SRC1_ALPHA = System::Word(0x88fb);
static _DELPHI_CONST System::Word GL_MAX_DUAL_SOURCE_DRAW_BUFFERS = System::Word(0x88fc);
static _DELPHI_CONST System::Word GL_ANY_SAMPLES_PASSED = System::Word(0x8c2f);
static _DELPHI_CONST System::Word GL_SAMPLER_BINDING = System::Word(0x8919);
static _DELPHI_CONST System::Word GL_RGB10_A2UI = System::Word(0x906f);
static _DELPHI_CONST System::Word GL_TEXTURE_SWIZZLE_R = System::Word(0x8e42);
static _DELPHI_CONST System::Word GL_TEXTURE_SWIZZLE_G = System::Word(0x8e43);
static _DELPHI_CONST System::Word GL_TEXTURE_SWIZZLE_B = System::Word(0x8e44);
static _DELPHI_CONST System::Word GL_TEXTURE_SWIZZLE_A = System::Word(0x8e45);
static _DELPHI_CONST System::Word GL_TEXTURE_SWIZZLE_RGBA = System::Word(0x8e46);
static _DELPHI_CONST System::Word GL_TIME_ELAPSED = System::Word(0x88bf);
static _DELPHI_CONST System::Word GL_TIMESTAMP = System::Word(0x8e28);
static _DELPHI_CONST System::Word GL_INT_2_10_10_10_REV = System::Word(0x8d9f);
static _DELPHI_CONST System::Word GL_DRAW_INDIRECT_BUFFER = System::Word(0x8f3f);
static _DELPHI_CONST System::Word GL_DRAW_INDIRECT_BUFFER_BINDING = System::Word(0x8f43);
static _DELPHI_CONST System::Word GL_GEOMETRY_SHADER_INVOCATIONS = System::Word(0x887f);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_SHADER_INVOCATIONS = System::Word(0x8e5a);
static _DELPHI_CONST System::Word GL_MIN_FRAGMENT_INTERPOLATION_OFFSET = System::Word(0x8e5b);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_INTERPOLATION_OFFSET = System::Word(0x8e5c);
static _DELPHI_CONST System::Word GL_FRAGMENT_INTERPOLATION_OFFSET_BITS = System::Word(0x8e5d);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_STREAMS = System::Word(0x8e71);
static _DELPHI_CONST System::Word GL_DOUBLE_VEC2 = System::Word(0x8ffc);
static _DELPHI_CONST System::Word GL_DOUBLE_VEC3 = System::Word(0x8ffd);
static _DELPHI_CONST System::Word GL_DOUBLE_VEC4 = System::Word(0x8ffe);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT2 = System::Word(0x8f46);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT3 = System::Word(0x8f47);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT4 = System::Word(0x8f48);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT2x3 = System::Word(0x8f49);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT2x4 = System::Word(0x8f4a);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT3x2 = System::Word(0x8f4b);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT3x4 = System::Word(0x8f4c);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT4x2 = System::Word(0x8f4d);
static _DELPHI_CONST System::Word GL_DOUBLE_MAT4x3 = System::Word(0x8f4e);
static _DELPHI_CONST System::Word GL_ACTIVE_SUBROUTINES = System::Word(0x8de5);
static _DELPHI_CONST System::Word GL_ACTIVE_SUBROUTINE_UNIFORMS = System::Word(0x8de6);
static _DELPHI_CONST System::Word GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS = System::Word(0x8e47);
static _DELPHI_CONST System::Word GL_ACTIVE_SUBROUTINE_MAX_LENGTH = System::Word(0x8e48);
static _DELPHI_CONST System::Word GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH = System::Word(0x8e49);
static _DELPHI_CONST System::Word GL_MAX_SUBROUTINES = System::Word(0x8de7);
static _DELPHI_CONST System::Word GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS = System::Word(0x8de8);
static _DELPHI_CONST System::Word GL_NUM_COMPATIBLE_SUBROUTINES = System::Word(0x8e4a);
static _DELPHI_CONST System::Word GL_COMPATIBLE_SUBROUTINES = System::Word(0x8e4b);
static _DELPHI_CONST System::Int8 GL_PATCHES = System::Int8(0xe);
static _DELPHI_CONST System::Word GL_PATCH_VERTICES = System::Word(0x8e72);
static _DELPHI_CONST System::Word GL_PATCH_DEFAULT_INNER_LEVEL = System::Word(0x8e73);
static _DELPHI_CONST System::Word GL_PATCH_DEFAULT_OUTER_LEVEL = System::Word(0x8e74);
static _DELPHI_CONST System::Word GL_TESS_CONTROL_OUTPUT_VERTICES = System::Word(0x8e75);
static _DELPHI_CONST System::Word GL_TESS_GEN_MODE = System::Word(0x8e76);
static _DELPHI_CONST System::Word GL_TESS_GEN_SPACING = System::Word(0x8e77);
static _DELPHI_CONST System::Word GL_TESS_GEN_VERTEX_ORDER = System::Word(0x8e78);
static _DELPHI_CONST System::Word GL_TESS_GEN_POINT_MODE = System::Word(0x8e79);
static _DELPHI_CONST System::Word GL_ISOLINES = System::Word(0x8e7a);
static _DELPHI_CONST System::Word GL_FRACTIONAL_ODD = System::Word(0x8e7b);
static _DELPHI_CONST System::Word GL_FRACTIONAL_EVEN = System::Word(0x8e7c);
static _DELPHI_CONST System::Word GL_MAX_PATCH_VERTICES = System::Word(0x8e7d);
static _DELPHI_CONST System::Word GL_MAX_TESS_GEN_LEVEL = System::Word(0x8e7e);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = System::Word(0x8e7f);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = System::Word(0x8e80);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = System::Word(0x8e81);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = System::Word(0x8e82);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = System::Word(0x8e83);
static _DELPHI_CONST System::Word GL_MAX_TESS_PATCH_COMPONENTS = System::Word(0x8e84);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = System::Word(0x8e85);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = System::Word(0x8e86);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = System::Word(0x8e89);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = System::Word(0x8e8a);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = System::Word(0x886c);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = System::Word(0x886d);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = System::Word(0x8e1e);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = System::Word(0x8e1f);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER = System::Word(0x84f0);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER = System::Word(0x84f1);
static _DELPHI_CONST System::Word GL_TESS_EVALUATION_SHADER = System::Word(0x8e87);
static _DELPHI_CONST System::Word GL_TESS_CONTROL_SHADER = System::Word(0x8e88);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK = System::Word(0x8e22);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED = System::Word(0x8e23);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE = System::Word(0x8e24);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BINDING = System::Word(0x8e25);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_BUFFERS = System::Word(0x8e70);
static _DELPHI_CONST System::Word GL_FIXED = System::Word(0x140c);
static _DELPHI_CONST System::Word GL_IMPLEMENTATION_COLOR_READ_TYPE = System::Word(0x8b9a);
static _DELPHI_CONST System::Word GL_IMPLEMENTATION_COLOR_READ_FORMAT = System::Word(0x8b9b);
static _DELPHI_CONST System::Word GL_LOW_FLOAT = System::Word(0x8df0);
static _DELPHI_CONST System::Word GL_MEDIUM_FLOAT = System::Word(0x8df1);
static _DELPHI_CONST System::Word GL_HIGH_FLOAT = System::Word(0x8df2);
static _DELPHI_CONST System::Word GL_LOW_INT = System::Word(0x8df3);
static _DELPHI_CONST System::Word GL_MEDIUM_INT = System::Word(0x8df4);
static _DELPHI_CONST System::Word GL_HIGH_INT = System::Word(0x8df5);
static _DELPHI_CONST System::Word GL_SHADER_COMPILER = System::Word(0x8dfa);
static _DELPHI_CONST System::Word GL_NUM_SHADER_BINARY_FORMATS = System::Word(0x8df9);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_UNIFORM_VECTORS = System::Word(0x8dfb);
static _DELPHI_CONST System::Word GL_MAX_VARYING_VECTORS = System::Word(0x8dfc);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_UNIFORM_VECTORS = System::Word(0x8dfd);
static _DELPHI_CONST System::Word GL_PROGRAM_BINARY_RETRIEVABLE_HINT = System::Word(0x8257);
static _DELPHI_CONST System::Word GL_PROGRAM_BINARY_LENGTH = System::Word(0x8741);
static _DELPHI_CONST System::Word GL_NUM_PROGRAM_BINARY_FORMATS = System::Word(0x87fe);
static _DELPHI_CONST System::Word GL_PROGRAM_BINARY_FORMATS = System::Word(0x87ff);
static _DELPHI_CONST System::Int8 GL_VERTEX_SHADER_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_FRAGMENT_SHADER_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_GEOMETRY_SHADER_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_TESS_CONTROL_SHADER_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_TESS_EVALUATION_SHADER_BIT = System::Int8(0x10);
static _DELPHI_CONST unsigned GL_ALL_SHADER_BITS = unsigned(0xffffffff);
static _DELPHI_CONST System::Word GL_PROGRAM_SEPARABLE = System::Word(0x8258);
static _DELPHI_CONST System::Word GL_ACTIVE_PROGRAM = System::Word(0x8259);
static _DELPHI_CONST System::Word GL_PROGRAM_PIPELINE_BINDING = System::Word(0x825a);
static _DELPHI_CONST System::Word GL_MAX_VIEWPORTS = System::Word(0x825b);
static _DELPHI_CONST System::Word GL_VIEWPORT_SUBPIXEL_BITS = System::Word(0x825c);
static _DELPHI_CONST System::Word GL_VIEWPORT_BOUNDS_RANGE = System::Word(0x825d);
static _DELPHI_CONST System::Word GL_LAYER_PROVOKING_VERTEX = System::Word(0x825e);
static _DELPHI_CONST System::Word GL_VIEWPORT_INDEX_PROVOKING_VERTEX = System::Word(0x825f);
static _DELPHI_CONST System::Word GL_UNDEFINED_VERTEX = System::Word(0x8260);
static _DELPHI_CONST System::Word GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB = System::Word(0x8242);
static _DELPHI_CONST System::Word GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB = System::Word(0x8243);
static _DELPHI_CONST System::Word GL_DEBUG_CALLBACK_FUNCTION_ARB = System::Word(0x8244);
static _DELPHI_CONST System::Word GL_DEBUG_CALLBACK_USER_PARAM_ARB = System::Word(0x8245);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_API_ARB = System::Word(0x8246);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB = System::Word(0x8247);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_SHADER_COMPILER_ARB = System::Word(0x8248);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_THIRD_PARTY_ARB = System::Word(0x8249);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_APPLICATION_ARB = System::Word(0x824a);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_OTHER_ARB = System::Word(0x824b);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_ERROR_ARB = System::Word(0x824c);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB = System::Word(0x824d);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB = System::Word(0x824e);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_PORTABILITY_ARB = System::Word(0x824f);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_PERFORMANCE_ARB = System::Word(0x8250);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_OTHER_ARB = System::Word(0x8251);
static _DELPHI_CONST System::Word GL_MAX_DEBUG_MESSAGE_LENGTH_ARB = System::Word(0x9143);
static _DELPHI_CONST System::Word GL_MAX_DEBUG_LOGGED_MESSAGES_ARB = System::Word(0x9144);
static _DELPHI_CONST System::Word GL_DEBUG_LOGGED_MESSAGES_ARB = System::Word(0x9145);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_HIGH_ARB = System::Word(0x9146);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_MEDIUM_ARB = System::Word(0x9147);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_LOW_ARB = System::Word(0x9148);
static _DELPHI_CONST System::Int8 GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB = System::Int8(0x4);
static _DELPHI_CONST System::Word GL_LOSE_CONTEXT_ON_RESET_ARB = System::Word(0x8252);
static _DELPHI_CONST System::Word GL_GUILTY_CONTEXT_RESET_ARB = System::Word(0x8253);
static _DELPHI_CONST System::Word GL_INNOCENT_CONTEXT_RESET_ARB = System::Word(0x8254);
static _DELPHI_CONST System::Word GL_UNKNOWN_CONTEXT_RESET_ARB = System::Word(0x8255);
static _DELPHI_CONST System::Word GL_RESET_NOTIFICATION_STRATEGY_ARB = System::Word(0x8256);
static _DELPHI_CONST System::Word GL_NO_RESET_NOTIFICATION_ARB = System::Word(0x8261);
static _DELPHI_CONST System::Word GL_UNPACK_COMPRESSED_BLOCK_WIDTH = System::Word(0x9127);
static _DELPHI_CONST System::Word GL_UNPACK_COMPRESSED_BLOCK_HEIGHT = System::Word(0x9128);
static _DELPHI_CONST System::Word GL_UNPACK_COMPRESSED_BLOCK_DEPTH = System::Word(0x9129);
static _DELPHI_CONST System::Word GL_UNPACK_COMPRESSED_BLOCK_SIZE = System::Word(0x912a);
static _DELPHI_CONST System::Word GL_PACK_COMPRESSED_BLOCK_WIDTH = System::Word(0x912b);
static _DELPHI_CONST System::Word GL_PACK_COMPRESSED_BLOCK_HEIGHT = System::Word(0x912c);
static _DELPHI_CONST System::Word GL_PACK_COMPRESSED_BLOCK_DEPTH = System::Word(0x912d);
static _DELPHI_CONST System::Word GL_PACK_COMPRESSED_BLOCK_SIZE = System::Word(0x912e);
static _DELPHI_CONST System::Word GL_NUM_SAMPLE_COUNTS = System::Word(0x9380);
static _DELPHI_CONST System::Word GL_MIN_MAP_BUFFER_ALIGNMENT = System::Word(0x90bc);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER = System::Word(0x92c0);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_BINDING = System::Word(0x92c1);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_START = System::Word(0x92c2);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_SIZE = System::Word(0x92c3);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE = System::Word(0x92c4);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS = System::Word(0x92c5);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES = System::Word(0x92c6);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER = System::Word(0x92c7);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER = System::Word(0x92c8);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER = System::Word(0x92c9);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER = System::Word(0x92ca);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER = System::Word(0x92cb);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS = System::Word(0x92cc);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS = System::Word(0x92cd);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS = System::Word(0x92ce);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS = System::Word(0x92cf);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS = System::Word(0x92d0);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS = System::Word(0x92d1);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATOMIC_COUNTERS = System::Word(0x92d2);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS = System::Word(0x92d3);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS = System::Word(0x92d4);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_ATOMIC_COUNTERS = System::Word(0x92d5);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_ATOMIC_COUNTERS = System::Word(0x92d6);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_ATOMIC_COUNTERS = System::Word(0x92d7);
static _DELPHI_CONST System::Word GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE = System::Word(0x92d8);
static _DELPHI_CONST System::Word GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS = System::Word(0x92dc);
static _DELPHI_CONST System::Word GL_ACTIVE_ATOMIC_COUNTER_BUFFERS = System::Word(0x92d9);
static _DELPHI_CONST System::Word GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX = System::Word(0x92da);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_ATOMIC_COUNTER = System::Word(0x92db);
static _DELPHI_CONST System::Int8 GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_ELEMENT_ARRAY_BARRIER_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_UNIFORM_BARRIER_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_TEXTURE_FETCH_BARRIER_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_SHADER_IMAGE_ACCESS_BARRIER_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Int8 GL_COMMAND_BARRIER_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte GL_PIXEL_BUFFER_BARRIER_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Word GL_TEXTURE_UPDATE_BARRIER_BIT = System::Word(0x100);
static _DELPHI_CONST System::Word GL_BUFFER_UPDATE_BARRIER_BIT = System::Word(0x200);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_BARRIER_BIT = System::Word(0x400);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BARRIER_BIT = System::Word(0x800);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BARRIER_BIT = System::Word(0x1000);
static _DELPHI_CONST unsigned GL_ALL_BARRIER_BITS = unsigned(0xffffffff);
static _DELPHI_CONST System::Word GL_MAX_IMAGE_UNITS = System::Word(0x8f38);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS = System::Word(0x8f39);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_NAME = System::Word(0x8f3a);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_LEVEL = System::Word(0x8f3b);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_LAYERED = System::Word(0x8f3c);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_LAYER = System::Word(0x8f3d);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_ACCESS = System::Word(0x8f3e);
static _DELPHI_CONST System::Word GL_IMAGE_1D = System::Word(0x904c);
static _DELPHI_CONST System::Word GL_IMAGE_2D = System::Word(0x904d);
static _DELPHI_CONST System::Word GL_IMAGE_3D = System::Word(0x904e);
static _DELPHI_CONST System::Word GL_IMAGE_2D_RECT = System::Word(0x904f);
static _DELPHI_CONST System::Word GL_IMAGE_CUBE = System::Word(0x9050);
static _DELPHI_CONST System::Word GL_IMAGE_BUFFER = System::Word(0x9051);
static _DELPHI_CONST System::Word GL_IMAGE_1D_ARRAY = System::Word(0x9052);
static _DELPHI_CONST System::Word GL_IMAGE_2D_ARRAY = System::Word(0x9053);
static _DELPHI_CONST System::Word GL_IMAGE_CUBE_MAP_ARRAY = System::Word(0x9054);
static _DELPHI_CONST System::Word GL_IMAGE_2D_MULTISAMPLE = System::Word(0x9055);
static _DELPHI_CONST System::Word GL_IMAGE_2D_MULTISAMPLE_ARRAY = System::Word(0x9056);
static _DELPHI_CONST System::Word GL_INT_IMAGE_1D = System::Word(0x9057);
static _DELPHI_CONST System::Word GL_INT_IMAGE_2D = System::Word(0x9058);
static _DELPHI_CONST System::Word GL_INT_IMAGE_3D = System::Word(0x9059);
static _DELPHI_CONST System::Word GL_INT_IMAGE_2D_RECT = System::Word(0x905a);
static _DELPHI_CONST System::Word GL_INT_IMAGE_CUBE = System::Word(0x905b);
static _DELPHI_CONST System::Word GL_INT_IMAGE_BUFFER = System::Word(0x905c);
static _DELPHI_CONST System::Word GL_INT_IMAGE_1D_ARRAY = System::Word(0x905d);
static _DELPHI_CONST System::Word GL_INT_IMAGE_2D_ARRAY = System::Word(0x905e);
static _DELPHI_CONST System::Word GL_INT_IMAGE_CUBE_MAP_ARRAY = System::Word(0x905f);
static _DELPHI_CONST System::Word GL_INT_IMAGE_2D_MULTISAMPLE = System::Word(0x9060);
static _DELPHI_CONST System::Word GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY = System::Word(0x9061);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_1D = System::Word(0x9062);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_2D = System::Word(0x9063);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_3D = System::Word(0x9064);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_2D_RECT = System::Word(0x9065);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_CUBE = System::Word(0x9066);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_BUFFER = System::Word(0x9067);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_1D_ARRAY = System::Word(0x9068);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_2D_ARRAY = System::Word(0x9069);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY = System::Word(0x906a);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE = System::Word(0x906b);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY = System::Word(0x906c);
static _DELPHI_CONST System::Word GL_MAX_IMAGE_SAMPLES = System::Word(0x906d);
static _DELPHI_CONST System::Word GL_IMAGE_BINDING_FORMAT = System::Word(0x906e);
static _DELPHI_CONST System::Word GL_IMAGE_FORMAT_COMPATIBILITY_TYPE = System::Word(0x90c7);
static _DELPHI_CONST System::Word GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE = System::Word(0x90c8);
static _DELPHI_CONST System::Word GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS = System::Word(0x90c9);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_IMAGE_UNIFORMS = System::Word(0x90ca);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS = System::Word(0x90cb);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS = System::Word(0x90cc);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_IMAGE_UNIFORMS = System::Word(0x90cd);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_IMAGE_UNIFORMS = System::Word(0x90ce);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_IMAGE_UNIFORMS = System::Word(0x90cf);
static _DELPHI_CONST System::Word GL_TEXTURE_IMMUTABLE_FORMAT = System::Word(0x912f);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_4x4_KHR = System::Word(0x93b0);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_5x4_KHR = System::Word(0x93b1);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_5x5_KHR = System::Word(0x93b2);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_6x5_KHR = System::Word(0x93b3);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_6x6_KHR = System::Word(0x93b4);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_8x5_KHR = System::Word(0x93b5);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_8x6_KHR = System::Word(0x93b6);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_8x8_KHR = System::Word(0x93b7);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_105_KHR = System::Word(0x93b8);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_106_KHR = System::Word(0x93b9);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_108_KHR = System::Word(0x93ba);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_110_KHR = System::Word(0x93bb);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_12x10_KHR = System::Word(0x93bc);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_ASTC_12x12_KHR = System::Word(0x93bd);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR = System::Word(0x93d0);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR = System::Word(0x93d1);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR = System::Word(0x93d2);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR = System::Word(0x93d3);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR = System::Word(0x93d4);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR = System::Word(0x93d5);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR = System::Word(0x93d6);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR = System::Word(0x93d7);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR = System::Word(0x93d8);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR = System::Word(0x93d9);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR = System::Word(0x93da);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR = System::Word(0x93db);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR = System::Word(0x93dc);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR = System::Word(0x93dd);
static _DELPHI_CONST System::Word GL_DEBUG_OUTPUT_SYNCHRONOUS = System::Word(0x8242);
static _DELPHI_CONST System::Word GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH = System::Word(0x8243);
static _DELPHI_CONST System::Word GL_DEBUG_CALLBACK_FUNCTION = System::Word(0x8244);
static _DELPHI_CONST System::Word GL_DEBUG_CALLBACK_USER_PARAM = System::Word(0x8245);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_API = System::Word(0x8246);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_WINDOW_SYSTEM = System::Word(0x8247);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_SHADER_COMPILER = System::Word(0x8248);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_THIRD_PARTY = System::Word(0x8249);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_APPLICATION = System::Word(0x824a);
static _DELPHI_CONST System::Word GL_DEBUG_SOURCE_OTHER = System::Word(0x824b);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_ERROR = System::Word(0x824c);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR = System::Word(0x824d);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR = System::Word(0x824e);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_PORTABILITY = System::Word(0x824f);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_PERFORMANCE = System::Word(0x8250);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_OTHER = System::Word(0x8251);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_MARKER = System::Word(0x8268);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_PUSH_GROUP = System::Word(0x8269);
static _DELPHI_CONST System::Word GL_DEBUG_TYPE_POP_GROUP = System::Word(0x826a);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_NOTIFICATION = System::Word(0x826b);
static _DELPHI_CONST System::Word GL_MAX_DEBUG_GROUP_STACK_DEPTH = System::Word(0x826c);
static _DELPHI_CONST System::Word GL_DEBUG_GROUP_STACK_DEPTH = System::Word(0x826d);
static _DELPHI_CONST System::Word GL_BUFFER = System::Word(0x82e0);
static _DELPHI_CONST System::Word GL_SHADER = System::Word(0x82e1);
static _DELPHI_CONST System::Word GL_PROGRAM = System::Word(0x82e2);
static _DELPHI_CONST System::Word GL_QUERY = System::Word(0x82e3);
static _DELPHI_CONST System::Word GL_PROGRAM_PIPELINE = System::Word(0x82e4);
static _DELPHI_CONST System::Word GL_SAMPLER = System::Word(0x82e6);
static _DELPHI_CONST System::Word GL_DISPLAY_LIST = System::Word(0x82e7);
static _DELPHI_CONST System::Word GL_MAX_LABEL_LENGTH = System::Word(0x82e8);
static _DELPHI_CONST System::Word GL_MAX_DEBUG_MESSAGE_LENGTH = System::Word(0x9143);
static _DELPHI_CONST System::Word GL_MAX_DEBUG_LOGGED_MESSAGES = System::Word(0x9144);
static _DELPHI_CONST System::Word GL_DEBUG_LOGGED_MESSAGES = System::Word(0x9145);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_HIGH = System::Word(0x9146);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_MEDIUM = System::Word(0x9147);
static _DELPHI_CONST System::Word GL_DEBUG_SEVERITY_LOW = System::Word(0x9148);
static _DELPHI_CONST System::Word GL_DEBUG_OUTPUT = System::Word(0x92e0);
static _DELPHI_CONST System::Int8 GL_CONTEXT_FLAG_DEBUG_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Word GL_COMPUTE_SHADER = System::Word(0x91b9);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_UNIFORM_BLOCKS = System::Word(0x91bb);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS = System::Word(0x91bc);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_IMAGE_UNIFORMS = System::Word(0x91bd);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_SHARED_MEMORY_SIZE = System::Word(0x8262);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_UNIFORM_COMPONENTS = System::Word(0x8263);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS = System::Word(0x8264);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_ATOMIC_COUNTERS = System::Word(0x8265);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS = System::Word(0x8266);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_LOCAL_INVOCATIONS = System::Word(0x90eb);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_WORK_GROUP_COUNT = System::Word(0x91be);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_WORK_GROUP_SIZE = System::Word(0x91bf);
static _DELPHI_CONST System::Word GL_COMPUTE_LOCAL_WORK_SIZE = System::Word(0x8267);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER = System::Word(0x90ec);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER = System::Word(0x90ed);
static _DELPHI_CONST System::Word GL_DISPATCH_INDIRECT_BUFFER = System::Word(0x90ee);
static _DELPHI_CONST System::Word GL_DISPATCH_INDIRECT_BUFFER_BINDING = System::Word(0x90ef);
static _DELPHI_CONST System::Int8 GL_COMPUTE_SHADER_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB8_ETC2 = System::Word(0x9274);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ETC2 = System::Word(0x9275);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = System::Word(0x9276);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = System::Word(0x9277);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA8_ETC2_EAC = System::Word(0x9278);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = System::Word(0x9279);
static _DELPHI_CONST System::Word GL_COMPRESSED_R11_EAC = System::Word(0x9270);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_R11_EAC = System::Word(0x9271);
static _DELPHI_CONST System::Word GL_COMPRESSED_RG11_EAC = System::Word(0x9272);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_RG11_EAC = System::Word(0x9273);
static _DELPHI_CONST System::Word GL_PRIMITIVE_RESTART_FIXED_INDEX = System::Word(0x8d69);
static _DELPHI_CONST System::Word GL_ANY_SAMPLES_PASSED_CONSERVATIVE = System::Word(0x8d6a);
static _DELPHI_CONST System::Word GL_MAX_ELEMENT_INDEX = System::Word(0x8d6b);
static _DELPHI_CONST System::Word GL_MAX_UNIFORM_LOCATIONS = System::Word(0x826e);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT_WIDTH = System::Word(0x9310);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT_HEIGHT = System::Word(0x9311);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT_LAYERS = System::Word(0x9312);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT_SAMPLES = System::Word(0x9313);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS = System::Word(0x9314);
static _DELPHI_CONST System::Word GL_MAX_FRAMEBUFFER_WIDTH = System::Word(0x9315);
static _DELPHI_CONST System::Word GL_MAX_FRAMEBUFFER_HEIGHT = System::Word(0x9316);
static _DELPHI_CONST System::Word GL_MAX_FRAMEBUFFER_LAYERS = System::Word(0x9317);
static _DELPHI_CONST System::Word GL_MAX_FRAMEBUFFER_SAMPLES = System::Word(0x9318);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_SUPPORTED = System::Word(0x826f);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_PREFERRED = System::Word(0x8270);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_RED_SIZE = System::Word(0x8271);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_GREEN_SIZE = System::Word(0x8272);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_BLUE_SIZE = System::Word(0x8273);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_ALPHA_SIZE = System::Word(0x8274);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_DEPTH_SIZE = System::Word(0x8275);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_STENCIL_SIZE = System::Word(0x8276);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_SHARED_SIZE = System::Word(0x8277);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_RED_TYPE = System::Word(0x8278);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_GREEN_TYPE = System::Word(0x8279);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_BLUE_TYPE = System::Word(0x827a);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_ALPHA_TYPE = System::Word(0x827b);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_DEPTH_TYPE = System::Word(0x827c);
static _DELPHI_CONST System::Word GL_INTERNALFORMAT_STENCIL_TYPE = System::Word(0x827d);
static _DELPHI_CONST System::Word GL_MAX_WIDTH = System::Word(0x827e);
static _DELPHI_CONST System::Word GL_MAX_HEIGHT = System::Word(0x827f);
static _DELPHI_CONST System::Word GL_MAX_DEPTH = System::Word(0x8280);
static _DELPHI_CONST System::Word GL_MAX_LAYERS = System::Word(0x8281);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_DIMENSIONS = System::Word(0x8282);
static _DELPHI_CONST System::Word GL_COLOR_COMPONENTS = System::Word(0x8283);
static _DELPHI_CONST System::Word GL_DEPTH_COMPONENTS = System::Word(0x8284);
static _DELPHI_CONST System::Word GL_STENCIL_COMPONENTS = System::Word(0x8285);
static _DELPHI_CONST System::Word GL_COLOR_RENDERABLE = System::Word(0x8286);
static _DELPHI_CONST System::Word GL_DEPTH_RENDERABLE = System::Word(0x8287);
static _DELPHI_CONST System::Word GL_STENCIL_RENDERABLE = System::Word(0x8288);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_RENDERABLE = System::Word(0x8289);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_RENDERABLE_LAYERED = System::Word(0x828a);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_BLEND = System::Word(0x828b);
static _DELPHI_CONST System::Word GL_READ_PIXELS = System::Word(0x828c);
static _DELPHI_CONST System::Word GL_READ_PIXELS_FORMAT = System::Word(0x828d);
static _DELPHI_CONST System::Word GL_READ_PIXELS_TYPE = System::Word(0x828e);
static _DELPHI_CONST System::Word GL_TEXTURE_IMAGE_FORMAT = System::Word(0x828f);
static _DELPHI_CONST System::Word GL_TEXTURE_IMAGE_TYPE = System::Word(0x8290);
static _DELPHI_CONST System::Word GL_GET_TEXTURE_IMAGE_FORMAT = System::Word(0x8291);
static _DELPHI_CONST System::Word GL_GET_TEXTURE_IMAGE_TYPE = System::Word(0x8292);
static _DELPHI_CONST System::Word GL_MIPMAP = System::Word(0x8293);
static _DELPHI_CONST System::Word GL_MANUAL_GENERATE_MIPMAP = System::Word(0x8294);
static _DELPHI_CONST System::Word GL_AUTO_GENERATE_MIPMAP = System::Word(0x8295);
static _DELPHI_CONST System::Word GL_COLOR_ENCODING = System::Word(0x8296);
static _DELPHI_CONST System::Word GL_SRGB_READ = System::Word(0x8297);
static _DELPHI_CONST System::Word GL_SRGB_WRITE = System::Word(0x8298);
static _DELPHI_CONST System::Word GL_SRGB_DECODE_ARB = System::Word(0x8299);
static _DELPHI_CONST System::Word GL_FILTER = System::Word(0x829a);
static _DELPHI_CONST System::Word GL_VERTEX_TEXTURE = System::Word(0x829b);
static _DELPHI_CONST System::Word GL_TESS_CONTROL_TEXTURE = System::Word(0x829c);
static _DELPHI_CONST System::Word GL_TESS_EVALUATION_TEXTURE = System::Word(0x829d);
static _DELPHI_CONST System::Word GL_GEOMETRY_TEXTURE = System::Word(0x829e);
static _DELPHI_CONST System::Word GL_FRAGMENT_TEXTURE = System::Word(0x829f);
static _DELPHI_CONST System::Word GL_COMPUTE_TEXTURE = System::Word(0x82a0);
static _DELPHI_CONST System::Word GL_TEXTURE_SHADOW = System::Word(0x82a1);
static _DELPHI_CONST System::Word GL_TEXTURE_GATHER = System::Word(0x82a2);
static _DELPHI_CONST System::Word GL_TEXTURE_GATHER_SHADOW = System::Word(0x82a3);
static _DELPHI_CONST System::Word GL_SHADER_IMAGE_LOAD = System::Word(0x82a4);
static _DELPHI_CONST System::Word GL_SHADER_IMAGE_STORE = System::Word(0x82a5);
static _DELPHI_CONST System::Word GL_SHADER_IMAGE_ATOMIC = System::Word(0x82a6);
static _DELPHI_CONST System::Word GL_IMAGE_TEXEL_SIZE = System::Word(0x82a7);
static _DELPHI_CONST System::Word GL_IMAGE_COMPATIBILITY_CLASS = System::Word(0x82a8);
static _DELPHI_CONST System::Word GL_IMAGE_PIXEL_FORMAT = System::Word(0x82a9);
static _DELPHI_CONST System::Word GL_IMAGE_PIXEL_TYPE = System::Word(0x82aa);
static _DELPHI_CONST System::Word GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_TEST = System::Word(0x82ac);
static _DELPHI_CONST System::Word GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_TEST = System::Word(0x82ad);
static _DELPHI_CONST System::Word GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_WRITE = System::Word(0x82ae);
static _DELPHI_CONST System::Word GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_WRITE = System::Word(0x82af);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_BLOCK_WIDTH = System::Word(0x82b1);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_BLOCK_HEIGHT = System::Word(0x82b2);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPRESSED_BLOCK_SIZE = System::Word(0x82b3);
static _DELPHI_CONST System::Word GL_CLEAR_BUFFER = System::Word(0x82b4);
static _DELPHI_CONST System::Word GL_TEXTURE_VIEW = System::Word(0x82b5);
static _DELPHI_CONST System::Word GL_VIEW_COMPATIBILITY_CLASS = System::Word(0x82b6);
static _DELPHI_CONST System::Word GL_FULL_SUPPORT = System::Word(0x82b7);
static _DELPHI_CONST System::Word GL_CAVEAT_SUPPORT = System::Word(0x82b8);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_4_X_32 = System::Word(0x82b9);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_2_X_32 = System::Word(0x82ba);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_1_X_32 = System::Word(0x82bb);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_4_X_16 = System::Word(0x82bc);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_2_X_16 = System::Word(0x82bd);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_1_X_16 = System::Word(0x82be);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_4_X_8 = System::Word(0x82bf);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_2_X_8 = System::Word(0x82c0);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_1_X_8 = System::Word(0x82c1);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_11_11_10 = System::Word(0x82c2);
static _DELPHI_CONST System::Word GL_IMAGE_CLASS_10_10_10_2 = System::Word(0x82c3);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_128_BITS = System::Word(0x82c4);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_96_BITS = System::Word(0x82c5);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_64_BITS = System::Word(0x82c6);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_48_BITS = System::Word(0x82c7);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_32_BITS = System::Word(0x82c8);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_24_BITS = System::Word(0x82c9);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_16_BITS = System::Word(0x82ca);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_8_BITS = System::Word(0x82cb);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_S3TC_DXT1_RGB = System::Word(0x82cc);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_S3TC_DXT1_RGBA = System::Word(0x82cd);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_S3TC_DXT3_RGBA = System::Word(0x82ce);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_S3TC_DXT5_RGBA = System::Word(0x82cf);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_RGTC1_RED = System::Word(0x82d0);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_RGTC2_RG = System::Word(0x82d1);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_BPTC_UNORM = System::Word(0x82d2);
static _DELPHI_CONST System::Word GL_VIEW_CLASS_BPTC_FLOAT = System::Word(0x82d3);
static _DELPHI_CONST System::Word GL_UNIFORM = System::Word(0x92e1);
static _DELPHI_CONST System::Word GL_UNIFORM_BLOCK = System::Word(0x92e2);
static _DELPHI_CONST System::Word GL_PROGRAM_INPUT = System::Word(0x92e3);
static _DELPHI_CONST System::Word GL_PROGRAM_OUTPUT = System::Word(0x92e4);
static _DELPHI_CONST System::Word GL_BUFFER_VARIABLE = System::Word(0x92e5);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BLOCK = System::Word(0x92e6);
static _DELPHI_CONST System::Word GL_VERTEX_SUBROUTINE = System::Word(0x92e8);
static _DELPHI_CONST System::Word GL_TESS_CONTROL_SUBROUTINE = System::Word(0x92e9);
static _DELPHI_CONST System::Word GL_TESS_EVALUATION_SUBROUTINE = System::Word(0x92ea);
static _DELPHI_CONST System::Word GL_GEOMETRY_SUBROUTINE = System::Word(0x92eb);
static _DELPHI_CONST System::Word GL_FRAGMENT_SUBROUTINE = System::Word(0x92ec);
static _DELPHI_CONST System::Word GL_COMPUTE_SUBROUTINE = System::Word(0x92ed);
static _DELPHI_CONST System::Word GL_VERTEX_SUBROUTINE_UNIFORM = System::Word(0x92ee);
static _DELPHI_CONST System::Word GL_TESS_CONTROL_SUBROUTINE_UNIFORM = System::Word(0x92ef);
static _DELPHI_CONST System::Word GL_TESS_EVALUATION_SUBROUTINE_UNIFORM = System::Word(0x92f0);
static _DELPHI_CONST System::Word GL_GEOMETRY_SUBROUTINE_UNIFORM = System::Word(0x92f1);
static _DELPHI_CONST System::Word GL_FRAGMENT_SUBROUTINE_UNIFORM = System::Word(0x92f2);
static _DELPHI_CONST System::Word GL_COMPUTE_SUBROUTINE_UNIFORM = System::Word(0x92f3);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYING = System::Word(0x92f4);
static _DELPHI_CONST System::Word GL_ACTIVE_RESOURCES = System::Word(0x92f5);
static _DELPHI_CONST System::Word GL_MAX_NAME_LENGTH = System::Word(0x92f6);
static _DELPHI_CONST System::Word GL_MAX_NUM_ACTIVE_VARIABLES = System::Word(0x92f7);
static _DELPHI_CONST System::Word GL_MAX_NUM_COMPATIBLE_SUBROUTINES = System::Word(0x92f8);
static _DELPHI_CONST System::Word GL_NAME_LENGTH = System::Word(0x92f9);
static _DELPHI_CONST System::Word GL_TYPE = System::Word(0x92fa);
static _DELPHI_CONST System::Word GL_ARRAY_SIZE = System::Word(0x92fb);
static _DELPHI_CONST System::Word GL_OFFSET = System::Word(0x92fc);
static _DELPHI_CONST System::Word GL_BLOCK_INDEX = System::Word(0x92fd);
static _DELPHI_CONST System::Word GL_ARRAY_STRIDE = System::Word(0x92fe);
static _DELPHI_CONST System::Word GL_MATRIX_STRIDE = System::Word(0x92ff);
static _DELPHI_CONST System::Word GL_IS_ROW_MAJOR = System::Word(0x9300);
static _DELPHI_CONST System::Word GL_ATOMIC_COUNTER_BUFFER_INDEX = System::Word(0x9301);
static _DELPHI_CONST System::Word GL_BUFFER_BINDING = System::Word(0x9302);
static _DELPHI_CONST System::Word GL_BUFFER_DATA_SIZE = System::Word(0x9303);
static _DELPHI_CONST System::Word GL_NUM_ACTIVE_VARIABLES = System::Word(0x9304);
static _DELPHI_CONST System::Word GL_ACTIVE_VARIABLES = System::Word(0x9305);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_VERTEX_SHADER = System::Word(0x9306);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_TESS_CONTROL_SHADER = System::Word(0x9307);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_TESS_EVALUATION_SHADER = System::Word(0x9308);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_GEOMETRY_SHADER = System::Word(0x9309);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_FRAGMENT_SHADER = System::Word(0x930a);
static _DELPHI_CONST System::Word GL_REFERENCED_BY_COMPUTE_SHADER = System::Word(0x930b);
static _DELPHI_CONST System::Word GL_TOP_LEVEL_ARRAY_SIZE = System::Word(0x930c);
static _DELPHI_CONST System::Word GL_TOP_LEVEL_ARRAY_STRIDE = System::Word(0x930d);
static _DELPHI_CONST System::Word GL_LOCATION = System::Word(0x930e);
static _DELPHI_CONST System::Word GL_LOCATION_INDEX = System::Word(0x930f);
static _DELPHI_CONST System::Word GL_IS_PER_PATCH = System::Word(0x92e7);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BUFFER = System::Word(0x90d2);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BUFFER_BINDING = System::Word(0x90d3);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BUFFER_START = System::Word(0x90d4);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BUFFER_SIZE = System::Word(0x90d5);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS = System::Word(0x90d6);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS = System::Word(0x90d7);
static _DELPHI_CONST System::Word GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS = System::Word(0x90d8);
static _DELPHI_CONST System::Word GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS = System::Word(0x90d9);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS = System::Word(0x90da);
static _DELPHI_CONST System::Word GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS = System::Word(0x90db);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS = System::Word(0x90dc);
static _DELPHI_CONST System::Word GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS = System::Word(0x90dd);
static _DELPHI_CONST System::Word GL_MAX_SHADER_STORAGE_BLOCK_SIZE = System::Word(0x90de);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT = System::Word(0x90df);
static _DELPHI_CONST System::Word GL_SHADER_STORAGE_BARRIER_BIT = System::Word(0x2000);
static _DELPHI_CONST System::Word GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES = System::Word(0x8f39);
static _DELPHI_CONST System::Word GL_DEPTH_STENCIL_TEXTURE_MODE = System::Word(0x90ea);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_OFFSET = System::Word(0x919d);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_SIZE = System::Word(0x919e);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT = System::Word(0x919f);
static _DELPHI_CONST System::Word GL_TEXTURE_VIEW_MIN_LEVEL = System::Word(0x82db);
static _DELPHI_CONST System::Word GL_TEXTURE_VIEW_NUM_LEVELS = System::Word(0x82dc);
static _DELPHI_CONST System::Word GL_TEXTURE_VIEW_MIN_LAYER = System::Word(0x82dd);
static _DELPHI_CONST System::Word GL_TEXTURE_VIEW_NUM_LAYERS = System::Word(0x82de);
static _DELPHI_CONST System::Word GL_TEXTURE_IMMUTABLE_LEVELS = System::Word(0x82df);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_BINDING = System::Word(0x82d4);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_RELATIVE_OFFSET = System::Word(0x82d5);
static _DELPHI_CONST System::Word GL_VERTEX_BINDING_DIVISOR = System::Word(0x82d6);
static _DELPHI_CONST System::Word GL_VERTEX_BINDING_OFFSET = System::Word(0x82d7);
static _DELPHI_CONST System::Word GL_VERTEX_BINDING_STRIDE = System::Word(0x82d8);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET = System::Word(0x82d9);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATTRIB_BINDINGS = System::Word(0x82da);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ATTRIB_STRIDE = System::Word(0x82e5);
static _DELPHI_CONST System::Int8 GL_MAP_PERSISTENT_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte GL_MAP_COHERENT_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Word GL_DYNAMIC_STORAGE_BIT = System::Word(0x100);
static _DELPHI_CONST System::Word GL_CLIENT_STORAGE_BIT = System::Word(0x200);
static _DELPHI_CONST System::Word GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT = System::Word(0x4000);
static _DELPHI_CONST System::Word GL_BUFFER_IMMUTABLE_STORAGE = System::Word(0x821f);
static _DELPHI_CONST System::Word GL_BUFFER_STORAGE_FLAGS = System::Word(0x8220);
static _DELPHI_CONST System::Word GL_CLEAR_TEXTURE = System::Word(0x9365);
static _DELPHI_CONST System::Word GL_LOCATION_COMPONENT = System::Word(0x934a);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_INDEX = System::Word(0x934b);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE = System::Word(0x934c);
static _DELPHI_CONST System::Word GL_QUERY_BUFFER = System::Word(0x9192);
static _DELPHI_CONST System::Word GL_QUERY_BUFFER_BARRIER_BIT = System::Word(0x8000);
static _DELPHI_CONST System::Word GL_QUERY_BUFFER_BINDING = System::Word(0x9193);
static _DELPHI_CONST System::Word GL_QUERY_RESULT_NO_WAIT = System::Word(0x9194);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_TO_EDGE = System::Word(0x8743);
static _DELPHI_CONST System::Word GL_TEXTURE_RECTANGLE_EXT = System::Word(0x84f5);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_RECTANGLE_EXT = System::Word(0x84f6);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_RECTANGLE_EXT = System::Word(0x84f7);
static _DELPHI_CONST System::Word GL_MAX_RECTANGLE_TEXTURE_SIZE_EXT = System::Word(0x84f8);
static _DELPHI_CONST System::Word GLX_BIND_TO_TEXTURE_RGB_EXT = System::Word(0x20d0);
static _DELPHI_CONST System::Word GLX_BIND_TO_TEXTURE_RGBA_EXT = System::Word(0x20d1);
static _DELPHI_CONST System::Word GLX_BIND_TO_MIPMAP_TEXTURE_EXT = System::Word(0x20d2);
static _DELPHI_CONST System::Word GLX_BIND_TO_TEXTURE_TARGETS_EXT = System::Word(0x20d3);
static _DELPHI_CONST System::Word GLX_Y_INVERTED_EXT = System::Word(0x20d4);
static _DELPHI_CONST System::Word GLX_TEXTURE_FORMAT_EXT = System::Word(0x20d5);
static _DELPHI_CONST System::Word GLX_TEXTURE_TARGET_EXT = System::Word(0x20d6);
static _DELPHI_CONST System::Word GLX_MIPMAP_TEXTURE_EXT = System::Word(0x20d7);
static _DELPHI_CONST System::Word GLX_TEXTURE_FORMAT_NONE_EXT = System::Word(0x20d8);
static _DELPHI_CONST System::Word GLX_TEXTURE_FORMAT_RGB_EXT = System::Word(0x20d9);
static _DELPHI_CONST System::Word GLX_TEXTURE_FORMAT_RGBA_EXT = System::Word(0x20da);
static _DELPHI_CONST System::Word GLX_TEXTURE_1D_EXT = System::Word(0x20db);
static _DELPHI_CONST System::Word GLX_TEXTURE_2D_EXT = System::Word(0x20dc);
static _DELPHI_CONST System::Word GLX_TEXTURE_RECTANGLE_EXT = System::Word(0x20dd);
static _DELPHI_CONST System::Word GLX_FRONT_LEFT_EXT = System::Word(0x20de);
static _DELPHI_CONST System::Word GLX_FRONT_RIGHT_EXT = System::Word(0x20df);
static _DELPHI_CONST System::Word GLX_BACK_LEFT_EXT = System::Word(0x20e0);
static _DELPHI_CONST System::Word GLX_BACK_RIGHT_EXT = System::Word(0x20e1);
static _DELPHI_CONST System::Word GLX_FRONT_EXT = System::Word(0x20de);
static _DELPHI_CONST System::Word GLX_BACK_EXT = System::Word(0x20e0);
static _DELPHI_CONST System::Word GLX_AUX0_EXT = System::Word(0x20e2);
static _DELPHI_CONST System::Word GLX_AUX1_EXT = System::Word(0x20e3);
static _DELPHI_CONST System::Word GLX_AUX2_EXT = System::Word(0x20e4);
static const System::Extended GLX_AUX3_EXT = 4.200000E+07;
static _DELPHI_CONST System::Word GLX_AUX4_EXT = System::Word(0x20e6);
static _DELPHI_CONST System::Word GLX_AUX5_EXT = System::Word(0x20e7);
static _DELPHI_CONST System::Word GLX_AUX6_EXT = System::Word(0x20e8);
static _DELPHI_CONST System::Word GLX_AUX7_EXT = System::Word(0x20e9);
static _DELPHI_CONST System::Word GLX_AUX8_EXT = System::Word(0x20ea);
static _DELPHI_CONST System::Word GLX_AUX9_EXT = System::Word(0x20eb);
static _DELPHI_CONST System::Word GL_ABGR_EXT = System::Word(0x8000);
static _DELPHI_CONST System::Word GL_CONSTANT_COLOR_EXT = System::Word(0x8001);
static _DELPHI_CONST System::Word GL_ONE_MINUS_CONSTANT_COLOR_EXT = System::Word(0x8002);
static _DELPHI_CONST System::Word GL_CONSTANT_ALPHA_EXT = System::Word(0x8003);
static _DELPHI_CONST System::Word GL_ONE_MINUS_CONSTANT_ALPHA_EXT = System::Word(0x8004);
static _DELPHI_CONST System::Word GL_BLEND_COLOR_EXT = System::Word(0x8005);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_EXT = System::Word(0x8037);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_FACTOR_EXT = System::Word(0x8038);
static _DELPHI_CONST System::Word GL_POLYGON_OFFSET_BIAS_EXT = System::Word(0x8039);
static _DELPHI_CONST System::Word GL_ALPHA4_EXT = System::Word(0x803b);
static _DELPHI_CONST System::Word GL_ALPHA8_EXT = System::Word(0x803c);
static _DELPHI_CONST System::Word GL_ALPHA12_EXT = System::Word(0x803d);
static _DELPHI_CONST System::Word GL_ALPHA16_EXT = System::Word(0x803e);
static _DELPHI_CONST System::Word GL_LUMINANCE4_EXT = System::Word(0x803f);
static _DELPHI_CONST System::Word GL_LUMINANCE8_EXT = System::Word(0x8040);
static _DELPHI_CONST System::Word GL_LUMINANCE12_EXT = System::Word(0x8041);
static _DELPHI_CONST System::Word GL_LUMINANCE16_EXT = System::Word(0x8042);
static _DELPHI_CONST System::Word GL_LUMINANCE4_ALPHA4_EXT = System::Word(0x8043);
static _DELPHI_CONST System::Word GL_LUMINANCE6_ALPHA2_EXT = System::Word(0x8044);
static _DELPHI_CONST System::Word GL_LUMINANCE8_ALPHA8_EXT = System::Word(0x8045);
static _DELPHI_CONST System::Word GL_LUMINANCE12_ALPHA4_EXT = System::Word(0x8046);
static _DELPHI_CONST System::Word GL_LUMINANCE12_ALPHA12_EXT = System::Word(0x8047);
static _DELPHI_CONST System::Word GL_LUMINANCE16_ALPHA16_EXT = System::Word(0x8048);
static _DELPHI_CONST System::Word GL_INTENSITY_EXT = System::Word(0x8049);
static _DELPHI_CONST System::Word GL_INTENSITY4_EXT = System::Word(0x804a);
static _DELPHI_CONST System::Word GL_INTENSITY8_EXT = System::Word(0x804b);
static _DELPHI_CONST System::Word GL_INTENSITY12_EXT = System::Word(0x804c);
static _DELPHI_CONST System::Word GL_INTENSITY16_EXT = System::Word(0x804d);
static _DELPHI_CONST System::Word GL_RGB2_EXT = System::Word(0x804e);
static _DELPHI_CONST System::Word GL_RGB4_EXT = System::Word(0x804f);
static _DELPHI_CONST System::Word GL_RGB5_EXT = System::Word(0x8050);
static _DELPHI_CONST System::Word GL_RGB8_EXT = System::Word(0x8051);
static _DELPHI_CONST System::Word GL_RGB10_EXT = System::Word(0x8052);
static _DELPHI_CONST System::Word GL_RGB12_EXT = System::Word(0x8053);
static _DELPHI_CONST System::Word GL_RGB16_EXT = System::Word(0x8054);
static _DELPHI_CONST System::Word GL_RGBA2_EXT = System::Word(0x8055);
static _DELPHI_CONST System::Word GL_RGBA4_EXT = System::Word(0x8056);
static _DELPHI_CONST System::Word GL_RGB5_A1_EXT = System::Word(0x8057);
static _DELPHI_CONST System::Word GL_RGBA8_EXT = System::Word(0x8058);
static _DELPHI_CONST System::Word GL_RGB10_A2_EXT = System::Word(0x8059);
static _DELPHI_CONST System::Word GL_RGBA12_EXT = System::Word(0x805a);
static _DELPHI_CONST System::Word GL_RGBA16_EXT = System::Word(0x805b);
static _DELPHI_CONST System::Word GL_TEXTURE_RED_SIZE_EXT = System::Word(0x805c);
static _DELPHI_CONST System::Word GL_TEXTURE_GREEN_SIZE_EXT = System::Word(0x805d);
static _DELPHI_CONST System::Word GL_TEXTURE_BLUE_SIZE_EXT = System::Word(0x805e);
static _DELPHI_CONST System::Word GL_TEXTURE_ALPHA_SIZE_EXT = System::Word(0x805f);
static _DELPHI_CONST System::Word GL_TEXTURE_LUMINANCE_SIZE_EXT = System::Word(0x8060);
static _DELPHI_CONST System::Word GL_TEXTURE_INTENSITY_SIZE_EXT = System::Word(0x8061);
static _DELPHI_CONST System::Word GL_REPLACE_EXT = System::Word(0x8062);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_1D_EXT = System::Word(0x8063);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D_EXT = System::Word(0x8064);
static _DELPHI_CONST System::Word GL_TEXTURE_TOO_LARGE_EXT = System::Word(0x8065);
static _DELPHI_CONST System::Word GL_RGB_S3TC = System::Word(0x83a0);
static _DELPHI_CONST System::Word GL_RGB4_S3TC = System::Word(0x83a1);
static _DELPHI_CONST System::Word GL_RGBA_S3TC = System::Word(0x83a2);
static _DELPHI_CONST System::Word GL_RGBA4_S3TC = System::Word(0x83a3);
static _DELPHI_CONST System::Word GL_RGBA_DXT5_S3TC = System::Word(0x83a4);
static _DELPHI_CONST System::Word GL_RGBA4_DXT5_S3TC = System::Word(0x83a5);
static _DELPHI_CONST System::Word GL_PACK_SKIP_IMAGES_EXT = System::Word(0x806b);
static _DELPHI_CONST System::Word GL_PACK_IMAGE_HEIGHT_EXT = System::Word(0x806c);
static _DELPHI_CONST System::Word GL_UNPACK_SKIP_IMAGES_EXT = System::Word(0x806d);
static _DELPHI_CONST System::Word GL_UNPACK_IMAGE_HEIGHT_EXT = System::Word(0x806e);
static _DELPHI_CONST System::Word GL_TEXTURE_3D_EXT = System::Word(0x806f);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_3D_EXT = System::Word(0x8070);
static _DELPHI_CONST System::Word GL_TEXTURE_DEPTH_EXT = System::Word(0x8071);
static _DELPHI_CONST System::Word GL_TEXTURE_WRAP_R_EXT = System::Word(0x8072);
static _DELPHI_CONST System::Word GL_MAX_3D_TEXTURE_SIZE_EXT = System::Word(0x8073);
static _DELPHI_CONST System::Word GL_HISTOGRAM_EXT = System::Word(0x8024);
static _DELPHI_CONST System::Word GL_PROXY_HISTOGRAM_EXT = System::Word(0x8025);
static _DELPHI_CONST System::Word GL_HISTOGRAM_WIDTH_EXT = System::Word(0x8026);
static _DELPHI_CONST System::Word GL_HISTOGRAM_FORMAT_EXT = System::Word(0x8027);
static _DELPHI_CONST System::Word GL_HISTOGRAM_RED_SIZE_EXT = System::Word(0x8028);
static _DELPHI_CONST System::Word GL_HISTOGRAM_GREEN_SIZE_EXT = System::Word(0x8029);
static _DELPHI_CONST System::Word GL_HISTOGRAM_BLUE_SIZE_EXT = System::Word(0x802a);
static _DELPHI_CONST System::Word GL_HISTOGRAM_ALPHA_SIZE_EXT = System::Word(0x802b);
static _DELPHI_CONST System::Word GL_HISTOGRAM_LUMINANCE_SIZE_EXT = System::Word(0x802c);
static _DELPHI_CONST System::Word GL_HISTOGRAM_SINK_EXT = System::Word(0x802d);
static _DELPHI_CONST System::Word GL_MINMAX_EXT = System::Word(0x802e);
static _DELPHI_CONST System::Word GL_MINMAX_FORMAT_EXT = System::Word(0x802f);
static _DELPHI_CONST System::Word GL_MINMAX_SINK_EXT = System::Word(0x8030);
static _DELPHI_CONST System::Word GL_CONVOLUTION_1D_EXT = System::Word(0x8010);
static _DELPHI_CONST System::Word GL_CONVOLUTION_2D_EXT = System::Word(0x8011);
static _DELPHI_CONST System::Word GL_SEPARABLE_2D_EXT = System::Word(0x8012);
static _DELPHI_CONST System::Word GL_CONVOLUTION_BORDER_MODE_EXT = System::Word(0x8013);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FILTER_SCALE_EXT = System::Word(0x8014);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FILTER_BIAS_EXT = System::Word(0x8015);
static _DELPHI_CONST System::Word GL_REDUCE_EXT = System::Word(0x8016);
static _DELPHI_CONST System::Word GL_CONVOLUTION_FORMAT_EXT = System::Word(0x8017);
static _DELPHI_CONST System::Word GL_CONVOLUTION_WIDTH_EXT = System::Word(0x8018);
static _DELPHI_CONST System::Word GL_CONVOLUTION_HEIGHT_EXT = System::Word(0x8019);
static _DELPHI_CONST System::Word GL_MAX_CONVOLUTION_WIDTH_EXT = System::Word(0x801a);
static _DELPHI_CONST System::Word GL_MAX_CONVOLUTION_HEIGHT_EXT = System::Word(0x801b);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_RED_SCALE_EXT = System::Word(0x801c);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_GREEN_SCALE_EXT = System::Word(0x801d);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_BLUE_SCALE_EXT = System::Word(0x801e);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = System::Word(0x801f);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_RED_BIAS_EXT = System::Word(0x8020);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_GREEN_BIAS_EXT = System::Word(0x8021);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_BLUE_BIAS_EXT = System::Word(0x8022);
static _DELPHI_CONST System::Word GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = System::Word(0x8023);
static _DELPHI_CONST System::Word GL_COLOR_MATRIX_SGI = System::Word(0x80b1);
static _DELPHI_CONST System::Word GL_COLOR_MATRIX_STACK_DEPTH_SGI = System::Word(0x80b2);
static _DELPHI_CONST System::Word GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = System::Word(0x80b3);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_RED_SCALE_SGI = System::Word(0x80b4);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = System::Word(0x80b5);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = System::Word(0x80b6);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = System::Word(0x80b7);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_RED_BIAS_SGI = System::Word(0x80b8);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = System::Word(0x80b9);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = System::Word(0x80ba);
static _DELPHI_CONST System::Word GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = System::Word(0x80bb);
static _DELPHI_CONST System::Word GL_TEXTURE_PRIORITY_EXT = System::Word(0x8066);
static _DELPHI_CONST System::Word GL_TEXTURE_RESIDENT_EXT = System::Word(0x8067);
static _DELPHI_CONST System::Word GL_TEXTURE_1D_BINDING_EXT = System::Word(0x8068);
static _DELPHI_CONST System::Word GL_TEXTURE_2D_BINDING_EXT = System::Word(0x8069);
static _DELPHI_CONST System::Word GL_TEXTURE_3D_BINDING_EXT = System::Word(0x806a);
static _DELPHI_CONST System::Word GL_UNSIGNED_BYTE_3_3_2_EXT = System::Word(0x8032);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_4_4_4_4_EXT = System::Word(0x8033);
static _DELPHI_CONST System::Word GL_UNSIGNED_SHORT_5_5_5_1_EXT = System::Word(0x8034);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_8_8_8_8_EXT = System::Word(0x8035);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_10_10_10_2_EXT = System::Word(0x8036);
static _DELPHI_CONST System::Word GL_TEXTURE_MIN_LOD_SGIS = System::Word(0x813a);
static _DELPHI_CONST System::Word GL_TEXTURE_MAX_LOD_SGIS = System::Word(0x813b);
static _DELPHI_CONST System::Word GL_TEXTURE_BASE_LEVEL_SGIS = System::Word(0x813c);
static _DELPHI_CONST System::Word GL_TEXTURE_MAX_LEVEL_SGIS = System::Word(0x813d);
static _DELPHI_CONST System::Word GL_MULTISAMPLE_SGIS = System::Word(0x809d);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_MASK_SGIS = System::Word(0x809e);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_ONE_SGIS = System::Word(0x809f);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_SGIS = System::Word(0x80a0);
static _DELPHI_CONST System::Word GL_1PASS_SGIS = System::Word(0x80a1);
static _DELPHI_CONST System::Word GL_2PASS_0_SGIS = System::Word(0x80a2);
static _DELPHI_CONST System::Word GL_2PASS_1_SGIS = System::Word(0x80a3);
static _DELPHI_CONST System::Word GL_4PASS_0_SGIS = System::Word(0x80a4);
static _DELPHI_CONST System::Word GL_4PASS_1_SGIS = System::Word(0x80a5);
static _DELPHI_CONST System::Word GL_4PASS_2_SGIS = System::Word(0x80a6);
static _DELPHI_CONST System::Word GL_4PASS_3_SGIS = System::Word(0x80a7);
static _DELPHI_CONST System::Word GL_SAMPLE_BUFFERS_SGIS = System::Word(0x80a8);
static _DELPHI_CONST System::Word GL_SAMPLES_SGIS = System::Word(0x80a9);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_VALUE_SGIS = System::Word(0x80aa);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_INVERT_SGIS = System::Word(0x80ab);
static _DELPHI_CONST System::Word GL_SAMPLE_PATTERN_SGIS = System::Word(0x80ac);
static _DELPHI_CONST System::Word GL_RESCALE_NORMAL_EXT = System::Word(0x803a);
static _DELPHI_CONST System::Word GL_GENERATE_MIPMAP_SGIS = System::Word(0x8191);
static _DELPHI_CONST System::Word GL_GENERATE_MIPMAP_HINT_SGIS = System::Word(0x8192);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_SGIX = System::Word(0x819a);
static _DELPHI_CONST System::Word GL_TEXTURE_COMPARE_OPERATOR_SGIX = System::Word(0x819b);
static _DELPHI_CONST System::Word GL_TEXTURE_LEQUAL_R_SGIX = System::Word(0x819c);
static _DELPHI_CONST System::Word GL_TEXTURE_GEQUAL_R_SGIX = System::Word(0x819d);
static _DELPHI_CONST System::Word GL_CLAMP_TO_EDGE_SGIS = System::Word(0x812f);
static _DELPHI_CONST System::Word GL_CLAMP_TO_BORDER_SGIS = System::Word(0x812d);
static _DELPHI_CONST System::Word GL_FUNC_ADD_EXT = System::Word(0x8006);
static _DELPHI_CONST System::Word GL_MIN_EXT = System::Word(0x8007);
static _DELPHI_CONST System::Word GL_MAX_EXT = System::Word(0x8008);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION_EXT = System::Word(0x8009);
static _DELPHI_CONST System::Word GL_FUNC_SUBTRACT_EXT = System::Word(0x800a);
static _DELPHI_CONST System::Word GL_FUNC_REVERSE_SUBTRACT_EXT = System::Word(0x800b);
static _DELPHI_CONST int GLU_OBJECT_PARAMETRIC_ERROR_EXT = int(0x18770);
static _DELPHI_CONST int GLU_OBJECT_PATH_LENGTH_EXT = int(0x18771);
static _DELPHI_CONST System::Word GL_COLOR_INDEX1_EXT = System::Word(0x80e2);
static _DELPHI_CONST System::Word GL_COLOR_INDEX2_EXT = System::Word(0x80e3);
static _DELPHI_CONST System::Word GL_COLOR_INDEX4_EXT = System::Word(0x80e4);
static _DELPHI_CONST System::Word GL_COLOR_INDEX8_EXT = System::Word(0x80e5);
static _DELPHI_CONST System::Word GL_COLOR_INDEX12_EXT = System::Word(0x80e6);
static _DELPHI_CONST System::Word GL_COLOR_INDEX16_EXT = System::Word(0x80e7);
static _DELPHI_CONST System::Word GL_TEXTURE_INDEX_SIZE_EXT = System::Word(0x80ed);
static _DELPHI_CONST System::Word GL_CLIP_VOLUME_CLIPPING_HINT_EXT = System::Word(0x80f0);
static _DELPHI_CONST System::Word GL_SHADOW_AMBIENT_SGIX = System::Word(0x80bf);
static _DELPHI_CONST System::Word GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = System::Word(0x81a8);
static _DELPHI_CONST System::Word GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = System::Word(0x81a9);
static _DELPHI_CONST int GLU_NURBS_MODE_EXT = int(0x18740);
static _DELPHI_CONST int GLU_NURBS_TESSELLATOR_EXT = int(0x18741);
static _DELPHI_CONST int GLU_NURBS_RENDERER_EXT = int(0x18742);
static _DELPHI_CONST int GLU_NURBS_BEGIN_EXT = int(0x18744);
static _DELPHI_CONST int GLU_NURBS_VERTEX_EXT = int(0x18745);
static _DELPHI_CONST int GLU_NURBS_NORMAL_EXT = int(0x18746);
static _DELPHI_CONST int GLU_NURBS_COLOR_EXT = int(0x18747);
static _DELPHI_CONST int GLU_NURBS_TEX_COORD_EXT = int(0x18748);
static _DELPHI_CONST int GLU_NURBS_END_EXT = int(0x18749);
static _DELPHI_CONST int GLU_NURBS_BEGIN_DATA_EXT = int(0x1874a);
static _DELPHI_CONST int GLU_NURBS_VERTEX_DATA_EXT = int(0x1874b);
static _DELPHI_CONST int GLU_NURBS_NORMAL_DATA_EXT = int(0x1874c);
static _DELPHI_CONST int GLU_NURBS_COLOR_DATA_EXT = int(0x1874d);
static _DELPHI_CONST int GLU_NURBS_TEX_COORD_DATA_EXT = int(0x1874e);
static _DELPHI_CONST int GLU_NURBS_END_DATA_EXT = int(0x1874f);
static _DELPHI_CONST int GL_RASTER_POSITION_UNCLIPPED_IBM = int(0x19262);
static _DELPHI_CONST System::Word GL_MAX_ELEMENTS_VERTICES_EXT = System::Word(0x80e8);
static _DELPHI_CONST System::Word GL_MAX_ELEMENTS_INDICES_EXT = System::Word(0x80e9);
static _DELPHI_CONST System::Word GL_BGR_EXT = System::Word(0x80e0);
static _DELPHI_CONST System::Word GL_BGRA_EXT = System::Word(0x80e1);
static _DELPHI_CONST System::Word GL_OCCLUSION_TEST_HP = System::Word(0x8165);
static _DELPHI_CONST System::Word GL_OCCLUSION_TEST_RESULT_HP = System::Word(0x8166);
static _DELPHI_CONST System::Word GL_SHARED_TEXTURE_PALETTE_EXT = System::Word(0x81fb);
static _DELPHI_CONST System::Word GL_LIGHT_MODEL_COLOR_CONTROL_EXT = System::Word(0x81f8);
static _DELPHI_CONST System::Word GL_SINGLE_COLOR_EXT = System::Word(0x81f9);
static _DELPHI_CONST System::Word GL_SEPARATE_SPECULAR_COLOR_EXT = System::Word(0x81fa);
static _DELPHI_CONST System::Word GL_COLOR_SUM_EXT = System::Word(0x8458);
static _DELPHI_CONST System::Word GL_CURRENT_SECONDARY_COLOR_EXT = System::Word(0x8459);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = System::Word(0x845a);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = System::Word(0x845b);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = System::Word(0x845c);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = System::Word(0x845d);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_EXT = System::Word(0x845e);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_SOURCE_EXT = System::Word(0x8450);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_EXT = System::Word(0x8451);
static _DELPHI_CONST System::Word GL_FRAGMENT_DEPTH_EXT = System::Word(0x8452);
static _DELPHI_CONST System::Word GL_CURRENT_FOG_COORDINATE_EXT = System::Word(0x8453);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_TYPE_EXT = System::Word(0x8454);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = System::Word(0x8455);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_POINTER_EXT = System::Word(0x8456);
static _DELPHI_CONST System::Word GL_FOG_COORDINATE_ARRAY_EXT = System::Word(0x8457);
static _DELPHI_CONST System::Word GL_COMBINE_EXT = System::Word(0x8570);
static _DELPHI_CONST System::Word GL_COMBINE_RGB_EXT = System::Word(0x8571);
static _DELPHI_CONST System::Word GL_COMBINE_ALPHA_EXT = System::Word(0x8572);
static _DELPHI_CONST System::Word GL_RGB_SCALE_EXT = System::Word(0x8573);
static _DELPHI_CONST System::Word GL_ADD_SIGNED_EXT = System::Word(0x8574);
static _DELPHI_CONST System::Word GL_INTERPOLATE_EXT = System::Word(0x8575);
static _DELPHI_CONST System::Word GL_CONSTANT_EXT = System::Word(0x8576);
static _DELPHI_CONST System::Word GL_PRIMARY_COLOR_EXT = System::Word(0x8577);
static _DELPHI_CONST System::Word GL_PREVIOUS_EXT = System::Word(0x8578);
static _DELPHI_CONST System::Word GL_SOURCE0_RGB_EXT = System::Word(0x8580);
static _DELPHI_CONST System::Word GL_SOURCE1_RGB_EXT = System::Word(0x8581);
static _DELPHI_CONST System::Word GL_SOURCE2_RGB_EXT = System::Word(0x8582);
static _DELPHI_CONST System::Word GL_SOURCE0_ALPHA_EXT = System::Word(0x8588);
static _DELPHI_CONST System::Word GL_SOURCE1_ALPHA_EXT = System::Word(0x8589);
static _DELPHI_CONST System::Word GL_SOURCE2_ALPHA_EXT = System::Word(0x858a);
static _DELPHI_CONST System::Word GL_OPERAND0_RGB_EXT = System::Word(0x8590);
static _DELPHI_CONST System::Word GL_OPERAND1_RGB_EXT = System::Word(0x8591);
static _DELPHI_CONST System::Word GL_OPERAND2_RGB_EXT = System::Word(0x8592);
static _DELPHI_CONST System::Word GL_OPERAND0_ALPHA_EXT = System::Word(0x8598);
static _DELPHI_CONST System::Word GL_OPERAND1_ALPHA_EXT = System::Word(0x8599);
static _DELPHI_CONST System::Word GL_OPERAND2_ALPHA_EXT = System::Word(0x859a);
static _DELPHI_CONST System::Word GL_SOURCE3_RGB_EXT = System::Word(0x8583);
static _DELPHI_CONST System::Word GL_SOURCE4_RGB_EXT = System::Word(0x8584);
static _DELPHI_CONST System::Word GL_SOURCE5_RGB_EXT = System::Word(0x8585);
static _DELPHI_CONST System::Word GL_SOURCE6_RGB_EXT = System::Word(0x8586);
static _DELPHI_CONST System::Word GL_SOURCE7_RGB_EXT = System::Word(0x8587);
static _DELPHI_CONST System::Word GL_SOURCE3_ALPHA_EXT = System::Word(0x858b);
static _DELPHI_CONST System::Word GL_SOURCE4_ALPHA_EXT = System::Word(0x858c);
static _DELPHI_CONST System::Word GL_SOURCE5_ALPHA_EXT = System::Word(0x858d);
static _DELPHI_CONST System::Word GL_SOURCE6_ALPHA_EXT = System::Word(0x858e);
static _DELPHI_CONST System::Word GL_SOURCE7_ALPHA_EXT = System::Word(0x858f);
static _DELPHI_CONST System::Word GL_OPERAND3_RGB_EXT = System::Word(0x8593);
static _DELPHI_CONST System::Word GL_OPERAND4_RGB_EXT = System::Word(0x8594);
static _DELPHI_CONST System::Word GL_OPERAND5_RGB_EXT = System::Word(0x8595);
static _DELPHI_CONST System::Word GL_OPERAND6_RGB_EXT = System::Word(0x8596);
static _DELPHI_CONST System::Word GL_OPERAND7_RGB_EXT = System::Word(0x8597);
static _DELPHI_CONST System::Word GL_OPERAND3_ALPHA_EXT = System::Word(0x859b);
static _DELPHI_CONST System::Word GL_OPERAND4_ALPHA_EXT = System::Word(0x859c);
static _DELPHI_CONST System::Word GL_OPERAND5_ALPHA_EXT = System::Word(0x859d);
static _DELPHI_CONST System::Word GL_OPERAND6_ALPHA_EXT = System::Word(0x859e);
static _DELPHI_CONST System::Word GL_OPERAND7_ALPHA_EXT = System::Word(0x859f);
static _DELPHI_CONST System::Word GL_BLEND_DST_RGB_EXT = System::Word(0x80c8);
static _DELPHI_CONST System::Word GL_BLEND_SRC_RGB_EXT = System::Word(0x80c9);
static _DELPHI_CONST System::Word GL_BLEND_DST_ALPHA_EXT = System::Word(0x80ca);
static _DELPHI_CONST System::Word GL_BLEND_SRC_ALPHA_EXT = System::Word(0x80cb);
static _DELPHI_CONST System::Word GL_NORMAL_MAP_EXT = System::Word(0x8511);
static _DELPHI_CONST System::Word GL_REFLECTION_MAP_EXT = System::Word(0x8512);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_EXT = System::Word(0x8513);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_CUBE_MAP_EXT = System::Word(0x8514);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = System::Word(0x8515);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = System::Word(0x8516);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = System::Word(0x8517);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = System::Word(0x8518);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = System::Word(0x8519);
static _DELPHI_CONST System::Word GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = System::Word(0x851a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_CUBE_MAP_EXT = System::Word(0x851b);
static _DELPHI_CONST System::Word GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = System::Word(0x851c);
static _DELPHI_CONST System::Word GL_INCR_WRAP_EXT = System::Word(0x8507);
static _DELPHI_CONST System::Word GL_DECR_WRAP_EXT = System::Word(0x8508);
static _DELPHI_CONST System::Word GL_NORMAL_MAP_NV = System::Word(0x8511);
static _DELPHI_CONST System::Word GL_REFLECTION_MAP_NV = System::Word(0x8512);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_LOD_BIAS_EXT = System::Word(0x84fd);
static _DELPHI_CONST System::Word GL_TEXTURE_FILTER_CONTROL_EXT = System::Word(0x8500);
static _DELPHI_CONST System::Word GL_TEXTURE_LOD_BIAS_EXT = System::Word(0x8501);
static _DELPHI_CONST System::Word GL_TEXTURE_MAX_ANISOTROPY_EXT = System::Word(0x84fe);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = System::Word(0x84ff);
static _DELPHI_CONST System::Word GL_MAX_SHININESS_NV = System::Word(0x8504);
static _DELPHI_CONST System::Word GL_MAX_SPOT_EXPONENT_NV = System::Word(0x8505);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_RANGE_NV = System::Word(0x851d);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_RANGE_LENGTH_NV = System::Word(0x851e);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_RANGE_VALID_NV = System::Word(0x851f);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = System::Word(0x8520);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_RANGE_POINTER_NV = System::Word(0x8521);
static _DELPHI_CONST System::Word GL_REGISTER_COMBINERS_NV = System::Word(0x8522);
static _DELPHI_CONST System::Word GL_VARIABLE_A_NV = System::Word(0x8523);
static _DELPHI_CONST System::Word GL_VARIABLE_B_NV = System::Word(0x8524);
static _DELPHI_CONST System::Word GL_VARIABLE_C_NV = System::Word(0x8525);
static _DELPHI_CONST System::Word GL_VARIABLE_D_NV = System::Word(0x8526);
static _DELPHI_CONST System::Word GL_VARIABLE_E_NV = System::Word(0x8527);
static _DELPHI_CONST System::Word GL_VARIABLE_F_NV = System::Word(0x8528);
static _DELPHI_CONST System::Word GL_VARIABLE_G_NV = System::Word(0x8529);
static _DELPHI_CONST System::Word GL_CONSTANT_COLOR0_NV = System::Word(0x852a);
static _DELPHI_CONST System::Word GL_CONSTANT_COLOR1_NV = System::Word(0x852b);
static _DELPHI_CONST System::Word GL_PRIMARY_COLOR_NV = System::Word(0x852c);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_NV = System::Word(0x852d);
static _DELPHI_CONST System::Word GL_SPARE0_NV = System::Word(0x852e);
static _DELPHI_CONST System::Word GL_SPARE1_NV = System::Word(0x852f);
static _DELPHI_CONST System::Word GL_DISCARD_NV = System::Word(0x8530);
static _DELPHI_CONST System::Word GL_E_TIMES_F_NV = System::Word(0x8531);
static _DELPHI_CONST System::Word GL_SPARE0_PLUS_SECONDARY_COLOR_NV = System::Word(0x8532);
static _DELPHI_CONST System::Word GL_UNSIGNED_IDENTITY_NV = System::Word(0x8536);
static _DELPHI_CONST System::Word GL_UNSIGNED_INVERT_NV = System::Word(0x8537);
static _DELPHI_CONST System::Word GL_EXPAND_NORMAL_NV = System::Word(0x8538);
static _DELPHI_CONST System::Word GL_EXPAND_NEGATE_NV = System::Word(0x8539);
static _DELPHI_CONST System::Word GL_HALF_BIAS_NORMAL_NV = System::Word(0x853a);
static _DELPHI_CONST System::Word GL_HALF_BIAS_NEGATE_NV = System::Word(0x853b);
static _DELPHI_CONST System::Word GL_SIGNED_IDENTITY_NV = System::Word(0x853c);
static _DELPHI_CONST System::Word GL_SIGNED_NEGATE_NV = System::Word(0x853d);
static _DELPHI_CONST System::Word GL_SCALE_BY_TWO_NV = System::Word(0x853e);
static _DELPHI_CONST System::Word GL_SCALE_BY_FOUR_NV = System::Word(0x853f);
static _DELPHI_CONST System::Word GL_SCALE_BY_ONE_HALF_NV = System::Word(0x8540);
static _DELPHI_CONST System::Word GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = System::Word(0x8541);
static _DELPHI_CONST System::Word GL_COMBINER_INPUT_NV = System::Word(0x8542);
static _DELPHI_CONST System::Word GL_COMBINER_MAPPING_NV = System::Word(0x8543);
static _DELPHI_CONST System::Word GL_COMBINER_COMPONENT_USAGE_NV = System::Word(0x8544);
static _DELPHI_CONST System::Word GL_COMBINER_AB_DOT_PRODUCT_NV = System::Word(0x8545);
static _DELPHI_CONST System::Word GL_COMBINER_CD_DOT_PRODUCT_NV = System::Word(0x8546);
static _DELPHI_CONST System::Word GL_COMBINER_MUX_SUM_NV = System::Word(0x8547);
static _DELPHI_CONST System::Word GL_COMBINER_SCALE_NV = System::Word(0x8548);
static _DELPHI_CONST System::Word GL_COMBINER_BIAS_NV = System::Word(0x8549);
static _DELPHI_CONST System::Word GL_COMBINER_AB_OUTPUT_NV = System::Word(0x854a);
static _DELPHI_CONST System::Word GL_COMBINER_CD_OUTPUT_NV = System::Word(0x854b);
static _DELPHI_CONST System::Word GL_COMBINER_SUM_OUTPUT_NV = System::Word(0x854c);
static _DELPHI_CONST System::Word GL_MAX_GENERAL_COMBINERS_NV = System::Word(0x854d);
static _DELPHI_CONST System::Word GL_NUM_GENERAL_COMBINERS_NV = System::Word(0x854e);
static _DELPHI_CONST System::Word GL_COLOR_SUM_CLAMP_NV = System::Word(0x854f);
static _DELPHI_CONST System::Word GL_COMBINER0_NV = System::Word(0x8550);
static _DELPHI_CONST System::Word GL_COMBINER1_NV = System::Word(0x8551);
static _DELPHI_CONST System::Word GL_COMBINER2_NV = System::Word(0x8552);
static _DELPHI_CONST System::Word GL_COMBINER3_NV = System::Word(0x8553);
static _DELPHI_CONST System::Word GL_COMBINER4_NV = System::Word(0x8554);
static _DELPHI_CONST System::Word GL_COMBINER5_NV = System::Word(0x8555);
static _DELPHI_CONST System::Word GL_COMBINER6_NV = System::Word(0x8556);
static _DELPHI_CONST System::Word GL_COMBINER7_NV = System::Word(0x8557);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_COLOR_NV = System::Word(0x20c3);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_ALPHA_NV = System::Word(0x20c4);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_DEPTH_NV = System::Word(0x20c5);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_COLOR_AND_ALPHA_NV = System::Word(0x20c6);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_COLOR_AND_DEPTH_NV = System::Word(0x20c7);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_FRAME_NV = System::Word(0x20c8);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_FIELD_1_NV = System::Word(0x20c9);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_FIELD_2_NV = System::Word(0x20ca);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_STACKED_FIELDS_1_2_NV = System::Word(0x20cb);
static _DELPHI_CONST System::Word GLX_VIDEO_OUT_STACKED_FIELDS_2_1_NV = System::Word(0x20cc);
static _DELPHI_CONST System::Word GLX_NUM_VIDEO_SLOTS_NV = System::Word(0x20f0);
static _DELPHI_CONST System::Word GLX_SWAP_INTERVAL_EXT = System::Word(0x20f1);
static _DELPHI_CONST System::Word GLX_MAX_SWAP_INTERVAL_EXT = System::Word(0x20f2);
static _DELPHI_CONST System::Word GLX_DEVICE_ID_NV = System::Word(0x20cd);
static _DELPHI_CONST System::Word GLX_UNIQUE_ID_NV = System::Word(0x20ce);
static _DELPHI_CONST System::Word GLX_NUM_VIDEO_CAPTURE_SLOTS_NV = System::Word(0x20cf);
static _DELPHI_CONST System::Word GL_FOG_DISTANCE_MODE_NV = System::Word(0x855a);
static _DELPHI_CONST System::Word GL_EYE_RADIAL_NV = System::Word(0x855b);
static _DELPHI_CONST System::Word GL_EYE_PLANE_ABSOLUTE_NV = System::Word(0x855c);
static _DELPHI_CONST System::Word GL_COMBINE4_NV = System::Word(0x8503);
static _DELPHI_CONST System::Word GL_SOURCE3_RGB_NV = System::Word(0x8583);
static _DELPHI_CONST System::Word GL_SOURCE3_ALPHA_NV = System::Word(0x858b);
static _DELPHI_CONST System::Word GL_OPERAND3_RGB_NV = System::Word(0x8593);
static _DELPHI_CONST System::Word GL_OPERAND3_ALPHA_NV = System::Word(0x859b);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB_S3TC_DXT1_EXT = System::Word(0x83f0);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = System::Word(0x83f1);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = System::Word(0x83f2);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = System::Word(0x83f3);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGB_FXT1_3DFX = System::Word(0x86b0);
static _DELPHI_CONST System::Word GL_COMPRESSED_RGBA_FXT1_3DFX = System::Word(0x86b1);
static _DELPHI_CONST System::Word GL_MULTISAMPLE_3DFX = System::Word(0x86b2);
static _DELPHI_CONST System::Word GL_SAMPLE_BUFFERS_3DFX = System::Word(0x86b3);
static _DELPHI_CONST System::Word GL_SAMPLES_3DFX = System::Word(0x86b4);
static _DELPHI_CONST int GL_MULTISAMPLE_BIT_3DFX = int(0x20000000);
static _DELPHI_CONST System::Word GL_MULTISAMPLE_EXT = System::Word(0x809d);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_MASK_EXT = System::Word(0x809e);
static _DELPHI_CONST System::Word GL_SAMPLE_ALPHA_TO_ONE_EXT = System::Word(0x809f);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_EXT = System::Word(0x80a0);
static _DELPHI_CONST System::Word GL_1PASS_EXT = System::Word(0x80a1);
static _DELPHI_CONST System::Word GL_2PASS_0_EXT = System::Word(0x80a2);
static _DELPHI_CONST System::Word GL_2PASS_1_EXT = System::Word(0x80a3);
static _DELPHI_CONST System::Word GL_4PASS_0_EXT = System::Word(0x80a4);
static _DELPHI_CONST System::Word GL_4PASS_1_EXT = System::Word(0x80a5);
static _DELPHI_CONST System::Word GL_4PASS_2_EXT = System::Word(0x80a6);
static _DELPHI_CONST System::Word GL_4PASS_3_EXT = System::Word(0x80a7);
static _DELPHI_CONST System::Word GL_SAMPLE_BUFFERS_EXT = System::Word(0x80a8);
static _DELPHI_CONST System::Word GL_SAMPLES_EXT = System::Word(0x80a9);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_VALUE_EXT = System::Word(0x80aa);
static _DELPHI_CONST System::Word GL_SAMPLE_MASK_INVERT_EXT = System::Word(0x80ab);
static _DELPHI_CONST System::Word GL_SAMPLE_PATTERN_EXT = System::Word(0x80ac);
static _DELPHI_CONST System::Word WGL_SAMPLE_BUFFERS_EXT = System::Word(0x2041);
static _DELPHI_CONST System::Word WGL_SAMPLES_EXT = System::Word(0x2042);
static _DELPHI_CONST System::Word GL_TEXTURE_COLOR_WRITEMASK_SGIS = System::Word(0x81ef);
static _DELPHI_CONST System::Word GL_DOT3_RGB_EXT = System::Word(0x8740);
static _DELPHI_CONST System::Word GL_DOT3_RGBA_EXT = System::Word(0x8741);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_ATI = System::Word(0x8742);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_TO_EDGE_ATI = System::Word(0x8743);
static _DELPHI_CONST System::Word GL_ALL_COMPLETED_NV = System::Word(0x84f2);
static _DELPHI_CONST System::Word GL_FENCE_STATUS_NV = System::Word(0x84f3);
static _DELPHI_CONST System::Word GL_FENCE_CONDITION_NV = System::Word(0x84f4);
static _DELPHI_CONST System::Word GL_TEXTURE_RECTANGLE_NV = System::Word(0x84f5);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_RECTANGLE_NV = System::Word(0x84f6);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_RECTANGLE_NV = System::Word(0x84f7);
static _DELPHI_CONST System::Word GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = System::Word(0x84f8);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_RECTANGLE_NV = System::Word(0x864c);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = System::Word(0x864d);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = System::Word(0x864e);
static _DELPHI_CONST System::Word GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = System::Word(0x86d9);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_S8_S8_8_8_NV = System::Word(0x86da);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = System::Word(0x86db);
static _DELPHI_CONST System::Word GL_DSDT_MAG_INTENSITY_NV = System::Word(0x86dc);
static _DELPHI_CONST System::Word GL_SHADER_CONSISTENT_NV = System::Word(0x86dd);
static _DELPHI_CONST System::Word GL_TEXTURE_SHADER_NV = System::Word(0x86de);
static _DELPHI_CONST System::Word GL_SHADER_OPERATION_NV = System::Word(0x86df);
static _DELPHI_CONST System::Word GL_CULL_MODES_NV = System::Word(0x86e0);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_MATRIX_NV = System::Word(0x86e1);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_SCALE_NV = System::Word(0x86e2);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_BIAS_NV = System::Word(0x86e3);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_2D_MATRIX_NV = System::Word(0x86e1);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_2D_SCALE_NV = System::Word(0x86e2);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_2D_BIAS_NV = System::Word(0x86e3);
static _DELPHI_CONST System::Word GL_PREVIOUS_TEXTURE_INPUT_NV = System::Word(0x86e4);
static _DELPHI_CONST System::Word GL_CONST_EYE_NV = System::Word(0x86e5);
static _DELPHI_CONST System::Word GL_PASS_THROUGH_NV = System::Word(0x86e6);
static _DELPHI_CONST System::Word GL_CULL_FRAGMENT_NV = System::Word(0x86e7);
static _DELPHI_CONST System::Word GL_OFFSET_TEXTURE_2D_NV = System::Word(0x86e8);
static _DELPHI_CONST System::Word GL_DEPENDENT_AR_TEXTURE_2D_NV = System::Word(0x86e9);
static _DELPHI_CONST System::Word GL_DEPENDENT_GB_TEXTURE_2D_NV = System::Word(0x86ea);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_NV = System::Word(0x86ec);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_DEPTH_REPLACE_NV = System::Word(0x86ed);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_TEXTURE_2D_NV = System::Word(0x86ee);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = System::Word(0x86f0);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = System::Word(0x86f1);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = System::Word(0x86f2);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = System::Word(0x86f3);
static _DELPHI_CONST System::Word GL_HILO_NV = System::Word(0x86f4);
static _DELPHI_CONST System::Word GL_DSDT_NV = System::Word(0x86f5);
static _DELPHI_CONST System::Word GL_DSDT_MAG_NV = System::Word(0x86f6);
static _DELPHI_CONST System::Word GL_DSDT_MAG_VIB_NV = System::Word(0x86f7);
static _DELPHI_CONST System::Word GL_HILO16_NV = System::Word(0x86f8);
static _DELPHI_CONST System::Word GL_SIGNED_HILO_NV = System::Word(0x86f9);
static _DELPHI_CONST System::Word GL_SIGNED_HILO16_NV = System::Word(0x86fa);
static _DELPHI_CONST System::Word GL_SIGNED_RGBA_NV = System::Word(0x86fb);
static _DELPHI_CONST System::Word GL_SIGNED_RGBA8_NV = System::Word(0x86fc);
static _DELPHI_CONST System::Word GL_SIGNED_RGB_NV = System::Word(0x86fe);
static _DELPHI_CONST System::Word GL_SIGNED_RGB8_NV = System::Word(0x86ff);
static _DELPHI_CONST System::Word GL_SIGNED_LUMINANCE_NV = System::Word(0x8701);
static _DELPHI_CONST System::Word GL_SIGNED_LUMINANCE8_NV = System::Word(0x8702);
static _DELPHI_CONST System::Word GL_SIGNED_LUMINANCE_ALPHA_NV = System::Word(0x8703);
static _DELPHI_CONST System::Word GL_SIGNED_LUMINANCE8_ALPHA8_NV = System::Word(0x8704);
static _DELPHI_CONST System::Word GL_SIGNED_ALPHA_NV = System::Word(0x8705);
static _DELPHI_CONST System::Word GL_SIGNED_ALPHA8_NV = System::Word(0x8706);
static _DELPHI_CONST System::Word GL_SIGNED_INTENSITY_NV = System::Word(0x8707);
static _DELPHI_CONST System::Word GL_SIGNED_INTENSITY8_NV = System::Word(0x8708);
static _DELPHI_CONST System::Word GL_DSDT8_NV = System::Word(0x8709);
static _DELPHI_CONST System::Word GL_DSDT8_MAG8_NV = System::Word(0x870a);
static _DELPHI_CONST System::Word GL_DSDT8_MAG8_INTENSITY8_NV = System::Word(0x870b);
static _DELPHI_CONST System::Word GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = System::Word(0x870c);
static _DELPHI_CONST System::Word GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = System::Word(0x870d);
static _DELPHI_CONST System::Word GL_HI_SCALE_NV = System::Word(0x870e);
static _DELPHI_CONST System::Word GL_LO_SCALE_NV = System::Word(0x870f);
static _DELPHI_CONST System::Word GL_DS_SCALE_NV = System::Word(0x8710);
static _DELPHI_CONST System::Word GL_DT_SCALE_NV = System::Word(0x8711);
static _DELPHI_CONST System::Word GL_MAGNITUDE_SCALE_NV = System::Word(0x8712);
static _DELPHI_CONST System::Word GL_VIBRANCE_SCALE_NV = System::Word(0x8713);
static _DELPHI_CONST System::Word GL_HI_BIAS_NV = System::Word(0x8714);
static _DELPHI_CONST System::Word GL_LO_BIAS_NV = System::Word(0x8715);
static _DELPHI_CONST System::Word GL_DS_BIAS_NV = System::Word(0x8716);
static _DELPHI_CONST System::Word GL_DT_BIAS_NV = System::Word(0x8717);
static _DELPHI_CONST System::Word GL_MAGNITUDE_BIAS_NV = System::Word(0x8718);
static _DELPHI_CONST System::Word GL_VIBRANCE_BIAS_NV = System::Word(0x8719);
static _DELPHI_CONST System::Word GL_TEXTURE_BORDER_VALUES_NV = System::Word(0x871a);
static _DELPHI_CONST System::Word GL_TEXTURE_HI_SIZE_NV = System::Word(0x871b);
static _DELPHI_CONST System::Word GL_TEXTURE_LO_SIZE_NV = System::Word(0x871c);
static _DELPHI_CONST System::Word GL_TEXTURE_DS_SIZE_NV = System::Word(0x871d);
static _DELPHI_CONST System::Word GL_TEXTURE_DT_SIZE_NV = System::Word(0x871e);
static _DELPHI_CONST System::Word GL_TEXTURE_MAG_SIZE_NV = System::Word(0x871f);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_TEXTURE_3D_NV = System::Word(0x86ef);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = System::Word(0x8533);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_NV = System::Word(0x8620);
static _DELPHI_CONST System::Word GL_VERTEX_STATE_PROGRAM_NV = System::Word(0x8621);
static _DELPHI_CONST System::Word GL_ATTRIB_ARRAY_SIZE_NV = System::Word(0x8623);
static _DELPHI_CONST System::Word GL_ATTRIB_ARRAY_STRIDE_NV = System::Word(0x8624);
static _DELPHI_CONST System::Word GL_ATTRIB_ARRAY_TYPE_NV = System::Word(0x8625);
static _DELPHI_CONST System::Word GL_CURRENT_ATTRIB_NV = System::Word(0x8626);
static _DELPHI_CONST System::Word GL_PROGRAM_LENGTH_NV = System::Word(0x8627);
static _DELPHI_CONST System::Word GL_PROGRAM_STRING_NV = System::Word(0x8628);
static _DELPHI_CONST System::Word GL_MODELVIEW_PROJECTION_NV = System::Word(0x8629);
static _DELPHI_CONST System::Word GL_IDENTITY_NV = System::Word(0x862a);
static _DELPHI_CONST System::Word GL_INVERSE_NV = System::Word(0x862b);
static _DELPHI_CONST System::Word GL_TRANSPOSE_NV = System::Word(0x862c);
static _DELPHI_CONST System::Word GL_INVERSE_TRANSPOSE_NV = System::Word(0x862d);
static _DELPHI_CONST System::Word GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = System::Word(0x862e);
static _DELPHI_CONST System::Word GL_MAX_TRACK_MATRICES_NV = System::Word(0x862f);
static _DELPHI_CONST System::Word GL_MATRIX0_NV = System::Word(0x8630);
static _DELPHI_CONST System::Word GL_MATRIX1_NV = System::Word(0x8631);
static _DELPHI_CONST System::Word GL_MATRIX2_NV = System::Word(0x8632);
static _DELPHI_CONST System::Word GL_MATRIX3_NV = System::Word(0x8633);
static _DELPHI_CONST System::Word GL_MATRIX4_NV = System::Word(0x8634);
static _DELPHI_CONST System::Word GL_MATRIX5_NV = System::Word(0x8635);
static _DELPHI_CONST System::Word GL_MATRIX6_NV = System::Word(0x8636);
static _DELPHI_CONST System::Word GL_MATRIX7_NV = System::Word(0x8637);
static _DELPHI_CONST System::Word GL_CURRENT_MATRIX_STACK_DEPTH_NV = System::Word(0x8640);
static _DELPHI_CONST System::Word GL_CURRENT_MATRIX_NV = System::Word(0x8641);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_POINT_SIZE_NV = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_TWO_SIDE_NV = System::Word(0x8643);
static _DELPHI_CONST System::Word GL_PROGRAM_PARAMETER_NV = System::Word(0x8644);
static _DELPHI_CONST System::Word GL_ATTRIB_ARRAY_POINTER_NV = System::Word(0x8645);
static _DELPHI_CONST System::Word GL_PROGRAM_TARGET_NV = System::Word(0x8646);
static _DELPHI_CONST System::Word GL_PROGRAM_RESIDENT_NV = System::Word(0x8647);
static _DELPHI_CONST System::Word GL_TRACK_MATRIX_NV = System::Word(0x8648);
static _DELPHI_CONST System::Word GL_TRACK_MATRIX_TRANSFORM_NV = System::Word(0x8649);
static _DELPHI_CONST System::Word GL_VERTEX_PROGRAM_BINDING_NV = System::Word(0x864a);
static _DELPHI_CONST System::Word GL_PROGRAM_ERROR_POSITION_NV = System::Word(0x864b);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY0_NV = System::Word(0x8650);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY1_NV = System::Word(0x8651);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY2_NV = System::Word(0x8652);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY3_NV = System::Word(0x8653);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY4_NV = System::Word(0x8654);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY5_NV = System::Word(0x8655);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY6_NV = System::Word(0x8656);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY7_NV = System::Word(0x8657);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY8_NV = System::Word(0x8658);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY9_NV = System::Word(0x8659);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY10_NV = System::Word(0x865a);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY11_NV = System::Word(0x865b);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY12_NV = System::Word(0x865c);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY13_NV = System::Word(0x865d);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY14_NV = System::Word(0x865e);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY15_NV = System::Word(0x865f);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB0_4_NV = System::Word(0x8660);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB1_4_NV = System::Word(0x8661);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB2_4_NV = System::Word(0x8662);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB3_4_NV = System::Word(0x8663);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB4_4_NV = System::Word(0x8664);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB5_4_NV = System::Word(0x8665);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB6_4_NV = System::Word(0x8666);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB7_4_NV = System::Word(0x8667);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB8_4_NV = System::Word(0x8668);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB9_4_NV = System::Word(0x8669);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB10_4_NV = System::Word(0x866a);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB11_4_NV = System::Word(0x866b);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB12_4_NV = System::Word(0x866c);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB13_4_NV = System::Word(0x866d);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB14_4_NV = System::Word(0x866e);
static _DELPHI_CONST System::Word GL_MAP1_VERTEX_ATTRIB15_4_NV = System::Word(0x866f);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB0_4_NV = System::Word(0x8670);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB1_4_NV = System::Word(0x8671);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB2_4_NV = System::Word(0x8672);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB3_4_NV = System::Word(0x8673);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB4_4_NV = System::Word(0x8674);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB5_4_NV = System::Word(0x8675);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB6_4_NV = System::Word(0x8676);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB7_4_NV = System::Word(0x8677);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB8_4_NV = System::Word(0x8678);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB9_4_NV = System::Word(0x8679);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB10_4_NV = System::Word(0x867a);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB11_4_NV = System::Word(0x867b);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB12_4_NV = System::Word(0x867c);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB13_4_NV = System::Word(0x867d);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB14_4_NV = System::Word(0x867e);
static _DELPHI_CONST System::Word GL_MAP2_VERTEX_ATTRIB15_4_NV = System::Word(0x867f);
static _DELPHI_CONST System::Word GL_MULTISAMPLE_FILTER_HINT_NV = System::Word(0x8534);
static _DELPHI_CONST System::Word GL_PIXEL_COUNTER_BITS_NV = System::Word(0x8864);
static _DELPHI_CONST System::Word GL_CURRENT_OCCLUSION_QUERY_ID_NV = System::Word(0x8865);
static _DELPHI_CONST System::Word GL_PIXEL_COUNT_NV = System::Word(0x8866);
static _DELPHI_CONST System::Word GL_PIXEL_COUNT_AVAILABLE_NV = System::Word(0x8867);
static _DELPHI_CONST System::Word GL_POINT_SPRITE_NV = System::Word(0x8861);
static _DELPHI_CONST System::Word GL_COORD_REPLACE_NV = System::Word(0x8862);
static _DELPHI_CONST System::Word GL_POINT_SPRITE_R_MODE_NV = System::Word(0x8863);
static _DELPHI_CONST System::Word GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = System::Word(0x8850);
static _DELPHI_CONST System::Word GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = System::Word(0x8851);
static _DELPHI_CONST System::Word GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = System::Word(0x8852);
static _DELPHI_CONST System::Word GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = System::Word(0x8853);
static _DELPHI_CONST System::Word GL_OFFSET_HILO_TEXTURE_2D_NV = System::Word(0x8854);
static _DELPHI_CONST System::Word GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = System::Word(0x8855);
static _DELPHI_CONST System::Word GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = System::Word(0x8856);
static _DELPHI_CONST System::Word GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = System::Word(0x8857);
static _DELPHI_CONST System::Word GL_DEPENDENT_HILO_TEXTURE_2D_NV = System::Word(0x8858);
static _DELPHI_CONST System::Word GL_DEPENDENT_RGB_TEXTURE_3D_NV = System::Word(0x8859);
static _DELPHI_CONST System::Word GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = System::Word(0x885a);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_PASS_THROUGH_NV = System::Word(0x885b);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_TEXTURE_1D_NV = System::Word(0x885c);
static _DELPHI_CONST System::Word GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = System::Word(0x885d);
static _DELPHI_CONST System::Word GL_HILO8_NV = System::Word(0x885e);
static _DELPHI_CONST System::Word GL_SIGNED_HILO8_NV = System::Word(0x885f);
static _DELPHI_CONST System::Word GL_FORCE_BLUE_TO_ONE_NV = System::Word(0x8860);
static _DELPHI_CONST System::Word GL_STENCIL_TEST_TWO_SIDE_EXT = System::Word(0x8910);
static _DELPHI_CONST System::Word GL_ACTIVE_STENCIL_FACE_EXT = System::Word(0x8911);
static _DELPHI_CONST System::Word GL_MAX_DRAW_BUFFERS_ATI = System::Word(0x8824);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER0_ATI = System::Word(0x8825);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER1_ATI = System::Word(0x8826);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER2_ATI = System::Word(0x8827);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER3_ATI = System::Word(0x8828);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER4_ATI = System::Word(0x8829);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER5_ATI = System::Word(0x882a);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER6_ATI = System::Word(0x882b);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER7_ATI = System::Word(0x882c);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER8_ATI = System::Word(0x882d);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER9_ATI = System::Word(0x882e);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER10_ATI = System::Word(0x882f);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER11_ATI = System::Word(0x8830);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER12_ATI = System::Word(0x8831);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER13_ATI = System::Word(0x8832);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER14_ATI = System::Word(0x8833);
static _DELPHI_CONST System::Word GL_DRAW_BUFFER15_ATI = System::Word(0x8834);
static _DELPHI_CONST System::Word WGL_TYPE_RGBA_FLOAT_ATI = System::Word(0x21a0);
static _DELPHI_CONST System::Word GL_TYPE_RGBA_FLOAT_ATI = System::Word(0x8820);
static _DELPHI_CONST System::Word GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = System::Word(0x8835);
static _DELPHI_CONST System::Word GL_RGBA_FLOAT32_ATI = System::Word(0x8814);
static _DELPHI_CONST System::Word GL_RGB_FLOAT32_ATI = System::Word(0x8815);
static _DELPHI_CONST System::Word GL_ALPHA_FLOAT32_ATI = System::Word(0x8816);
static _DELPHI_CONST System::Word GL_INTENSITY_FLOAT32_ATI = System::Word(0x8817);
static _DELPHI_CONST System::Word GL_LUMINANCE_FLOAT32_ATI = System::Word(0x8818);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA_FLOAT32_ATI = System::Word(0x8819);
static _DELPHI_CONST System::Word GL_RGBA_FLOAT16_ATI = System::Word(0x881a);
static _DELPHI_CONST System::Word GL_RGB_FLOAT16_ATI = System::Word(0x881b);
static _DELPHI_CONST System::Word GL_ALPHA_FLOAT16_ATI = System::Word(0x881c);
static _DELPHI_CONST System::Word GL_INTENSITY_FLOAT16_ATI = System::Word(0x881d);
static _DELPHI_CONST System::Word GL_LUMINANCE_FLOAT16_ATI = System::Word(0x881e);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA_FLOAT16_ATI = System::Word(0x881f);
static _DELPHI_CONST System::Word GL_FLOAT_R_NV = System::Word(0x8880);
static _DELPHI_CONST System::Word GL_FLOAT_RG_NV = System::Word(0x8881);
static _DELPHI_CONST System::Word GL_FLOAT_RGB_NV = System::Word(0x8882);
static _DELPHI_CONST System::Word GL_FLOAT_RGBA_NV = System::Word(0x8883);
static _DELPHI_CONST System::Word GL_FLOAT_R16_NV = System::Word(0x8884);
static _DELPHI_CONST System::Word GL_FLOAT_R32_NV = System::Word(0x8885);
static _DELPHI_CONST System::Word GL_FLOAT_RG16_NV = System::Word(0x8886);
static _DELPHI_CONST System::Word GL_FLOAT_RG32_NV = System::Word(0x8887);
static _DELPHI_CONST System::Word GL_FLOAT_RGB16_NV = System::Word(0x8888);
static _DELPHI_CONST System::Word GL_FLOAT_RGB32_NV = System::Word(0x8889);
static _DELPHI_CONST System::Word GL_FLOAT_RGBA16_NV = System::Word(0x888a);
static _DELPHI_CONST System::Word GL_FLOAT_RGBA32_NV = System::Word(0x888b);
static _DELPHI_CONST System::Word GL_TEXTURE_FLOAT_COMPONENTS_NV = System::Word(0x888c);
static _DELPHI_CONST System::Word GL_FLOAT_CLEAR_COLOR_VALUE_NV = System::Word(0x888d);
static _DELPHI_CONST System::Word GL_FLOAT_RGBA_MODE_NV = System::Word(0x888e);
static _DELPHI_CONST System::Word WGL_FLOAT_COMPONENTS_NV = System::Word(0x20b0);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = System::Word(0x20b1);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = System::Word(0x20b2);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = System::Word(0x20b3);
static _DELPHI_CONST System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = System::Word(0x20b4);
static _DELPHI_CONST System::Word WGL_TEXTURE_FLOAT_R_NV = System::Word(0x20b5);
static _DELPHI_CONST System::Word WGL_TEXTURE_FLOAT_RG_NV = System::Word(0x20b6);
static _DELPHI_CONST System::Word WGL_TEXTURE_FLOAT_RGB_NV = System::Word(0x20b7);
static _DELPHI_CONST System::Word WGL_TEXTURE_FLOAT_RGBA_NV = System::Word(0x20b8);
static _DELPHI_CONST System::Word GLX_FLOAT_COMPONENTS_NV = System::Word(0x20b0);
static _DELPHI_CONST System::Word GL_PRIMITIVE_RESTART_NV = System::Word(0x8558);
static _DELPHI_CONST System::Word GL_PRIMITIVE_RESTART_INDEX_NV = System::Word(0x8559);
static _DELPHI_CONST System::Word GL_DEPTH_BOUNDS_TEST_EXT = System::Word(0x8890);
static _DELPHI_CONST System::Word GL_DEPTH_BOUNDS_EXT = System::Word(0x8891);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_EXT = System::Word(0x8742);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_TO_EDGE_EXT = System::Word(0x8743);
static _DELPHI_CONST System::Word GL_MIRROR_CLAMP_TO_BORDER_EXT = System::Word(0x8912);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION_RGB_EXT = System::Word(0x8009);
static _DELPHI_CONST System::Word GL_BLEND_EQUATION_ALPHA_EXT = System::Word(0x883d);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER_EXT = System::Word(0x88eb);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER_EXT = System::Word(0x88ec);
static _DELPHI_CONST System::Word GL_PIXEL_PACK_BUFFER_BINDING_EXT = System::Word(0x88ed);
static _DELPHI_CONST System::Word GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = System::Word(0x88ef);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_EXT = System::Word(0x8d40);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_EXT = System::Word(0x8d41);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX1_EXT = System::Word(0x8d46);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX4_EXT = System::Word(0x8d47);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX8_EXT = System::Word(0x8d48);
static _DELPHI_CONST System::Word GL_STENCIL_INDEX16_EXT = System::Word(0x8d49);
static _DELPHI_CONST System::Word GL_DEPTH24_STENCIL8_EXT = System::Word(0x88f0);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_WIDTH_EXT = System::Word(0x8d42);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_HEIGHT_EXT = System::Word(0x8d43);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = System::Word(0x8d44);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_RED_SIZE_EXT = System::Word(0x8d50);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_GREEN_SIZE_EXT = System::Word(0x8d51);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_BLUE_SIZE_EXT = System::Word(0x8d52);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_ALPHA_SIZE_EXT = System::Word(0x8d53);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_DEPTH_SIZE_EXT = System::Word(0x8d54);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_STENCIL_SIZE_EXT = System::Word(0x8d55);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = System::Word(0x8cd0);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = System::Word(0x8cd1);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = System::Word(0x8cd2);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = System::Word(0x8cd3);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = System::Word(0x8cd4);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT0_EXT = System::Word(0x8ce0);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT1_EXT = System::Word(0x8ce1);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT2_EXT = System::Word(0x8ce2);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT3_EXT = System::Word(0x8ce3);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT4_EXT = System::Word(0x8ce4);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT5_EXT = System::Word(0x8ce5);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT6_EXT = System::Word(0x8ce6);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT7_EXT = System::Word(0x8ce7);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT8_EXT = System::Word(0x8ce8);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT9_EXT = System::Word(0x8ce9);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT10_EXT = System::Word(0x8cea);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT11_EXT = System::Word(0x8ceb);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT12_EXT = System::Word(0x8cec);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT13_EXT = System::Word(0x8ced);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT14_EXT = System::Word(0x8cee);
static _DELPHI_CONST System::Word GL_COLOR_ATTACHMENT15_EXT = System::Word(0x8cef);
static _DELPHI_CONST System::Word GL_DEPTH_ATTACHMENT_EXT = System::Word(0x8d00);
static _DELPHI_CONST System::Word GL_STENCIL_ATTACHMENT_EXT = System::Word(0x8d20);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_COMPLETE_EXT = System::Word(0x8cd5);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = System::Word(0x8cd6);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = System::Word(0x8cd7);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = System::Word(0x8cd8);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = System::Word(0x8cd9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = System::Word(0x8cda);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = System::Word(0x8cdb);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = System::Word(0x8cdc);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_UNSUPPORTED_EXT = System::Word(0x8cdd);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_BINDING_EXT = System::Word(0x8ca6);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_BINDING_EXT = System::Word(0x8ca7);
static _DELPHI_CONST System::Word GL_MAX_COLOR_ATTACHMENTS_EXT = System::Word(0x8cdf);
static _DELPHI_CONST System::Word GL_MAX_RENDERBUFFER_SIZE_EXT = System::Word(0x84e8);
static _DELPHI_CONST System::Word GL_INVALID_FRAMEBUFFER_OPERATION_EXT = System::Word(0x506);
static _DELPHI_CONST System::Word GL_DEPTH_STENCIL_EXT = System::Word(0x84f9);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_24_8_EXT = System::Word(0x84fa);
static _DELPHI_CONST System::Word GL_TEXTURE_STENCIL_SIZE_EXT = System::Word(0x88f1);
static _DELPHI_CONST System::Word GL_STENCIL_TAG_BITS_EXT = System::Word(0x88f2);
static _DELPHI_CONST System::Word GL_STENCIL_CLEAR_TAG_VALUE_EXT = System::Word(0x88f3);
static _DELPHI_CONST System::Word GL_SRGB_EXT = System::Word(0x8c40);
static _DELPHI_CONST System::Word GL_SRGB8_EXT = System::Word(0x8c41);
static _DELPHI_CONST System::Word GL_SRGB_ALPHA_EXT = System::Word(0x8c42);
static _DELPHI_CONST System::Word GL_SRGB8_ALPHA8_EXT = System::Word(0x8c43);
static _DELPHI_CONST System::Word GL_SLUMINANCE_ALPHA_EXT = System::Word(0x8c44);
static _DELPHI_CONST System::Word GL_SLUMINANCE8_ALPHA8_EXT = System::Word(0x8c45);
static _DELPHI_CONST System::Word GL_SLUMINANCE_EXT = System::Word(0x8c46);
static _DELPHI_CONST System::Word GL_SLUMINANCE8_EXT = System::Word(0x8c47);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_EXT = System::Word(0x8c48);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA_EXT = System::Word(0x8c49);
static _DELPHI_CONST System::Word GL_COMPRESSED_SLUMINANCE_EXT = System::Word(0x8c4a);
static _DELPHI_CONST System::Word GL_COMPRESSED_SLUMINANCE_ALPHA_EXT = System::Word(0x8c4b);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = System::Word(0x8c4c);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = System::Word(0x8c4d);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = System::Word(0x8c4e);
static _DELPHI_CONST System::Word GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = System::Word(0x8c4f);
static _DELPHI_CONST System::Word GL_READ_FRAMEBUFFER_EXT = System::Word(0x8ca8);
static _DELPHI_CONST System::Word GL_DRAW_FRAMEBUFFER_EXT = System::Word(0x8ca9);
static _DELPHI_CONST System::Word GL_DRAW_FRAMEBUFFER_BINDING_EXT = System::Word(0x8ca6);
static _DELPHI_CONST System::Word GL_READ_FRAMEBUFFER_BINDING_EXT = System::Word(0x8caa);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_SAMPLES_EXT = System::Word(0x8cab);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT = System::Word(0x8d56);
static _DELPHI_CONST System::Word GL_MAX_SAMPLES_EXT = System::Word(0x8d57);
static _DELPHI_CONST System::Word GL_TIME_ELAPSED_EXT = System::Word(0x88bf);
static _DELPHI_CONST System::Word GL_GEOMETRY_PROGRAM_NV = System::Word(0x8c26);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_OUTPUT_VERTICES_NV = System::Word(0x8c27);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV = System::Word(0x8c28);
static _DELPHI_CONST System::Word GL_GEOMETRY_SHADER_EXT = System::Word(0x8dd9);
static _DELPHI_CONST System::Word GL_GEOMETRY_VERTICES_OUT_EXT = System::Word(0x8dda);
static _DELPHI_CONST System::Word GL_GEOMETRY_INPUT_TYPE_EXT = System::Word(0x8ddb);
static _DELPHI_CONST System::Word GL_GEOMETRY_OUTPUT_TYPE_EXT = System::Word(0x8ddc);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT = System::Word(0x8c29);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT = System::Word(0x8ddd);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_VARYING_COMPONENTS_EXT = System::Word(0x8dde);
static _DELPHI_CONST System::Word GL_MAX_VARYING_COMPONENTS_EXT = System::Word(0x8b4b);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT = System::Word(0x8ddf);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT = System::Word(0x8de0);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT = System::Word(0x8de1);
static _DELPHI_CONST System::Int8 GL_LINES_ADJACENCY_EXT = System::Int8(0xa);
static _DELPHI_CONST System::Int8 GL_LINE_STRIP_ADJACENCY_EXT = System::Int8(0xb);
static _DELPHI_CONST System::Int8 GL_TRIANGLES_ADJACENCY_EXT = System::Int8(0xc);
static _DELPHI_CONST System::Int8 GL_TRIANGLE_STRIP_ADJACENCY_EXT = System::Int8(0xd);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT = System::Word(0x8da8);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT = System::Word(0x8da9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT = System::Word(0x8da7);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT = System::Word(0x8cd4);
static _DELPHI_CONST System::Word GL_PROGRAM_POINT_SIZE_EXT = System::Word(0x8642);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT = System::Word(0x88fd);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_ARRAY_EXT = System::Word(0x8dc0);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_ARRAY_EXT = System::Word(0x8dc1);
static _DELPHI_CONST System::Word GL_SAMPLER_BUFFER_EXT = System::Word(0x8dc2);
static _DELPHI_CONST System::Word GL_SAMPLER_1D_ARRAY_SHADOW_EXT = System::Word(0x8dc3);
static _DELPHI_CONST System::Word GL_SAMPLER_2D_ARRAY_SHADOW_EXT = System::Word(0x8dc4);
static _DELPHI_CONST System::Word GL_SAMPLER_CUBE_SHADOW_EXT = System::Word(0x8dc5);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC2_EXT = System::Word(0x8dc6);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC3_EXT = System::Word(0x8dc7);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_VEC4_EXT = System::Word(0x8dc8);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_1D_EXT = System::Word(0x8dc9);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_EXT = System::Word(0x8dca);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_3D_EXT = System::Word(0x8dcb);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_CUBE_EXT = System::Word(0x8dcc);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_RECT_EXT = System::Word(0x8dcd);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_1D_ARRAY_EXT = System::Word(0x8dce);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_2D_ARRAY_EXT = System::Word(0x8dcf);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_BUFFER_EXT = System::Word(0x8dd0);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_1D_EXT = System::Word(0x8dd1);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_EXT = System::Word(0x8dd2);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_3D_EXT = System::Word(0x8dd3);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_CUBE_EXT = System::Word(0x8dd4);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT = System::Word(0x8dd5);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT = System::Word(0x8dd6);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT = System::Word(0x8dd7);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT = System::Word(0x8dd8);
static _DELPHI_CONST System::Word GL_MIN_PROGRAM_TEXEL_OFFSET_EXT = System::Word(0x8904);
static _DELPHI_CONST System::Word GL_MAX_PROGRAM_TEXEL_OFFSET_EXT = System::Word(0x8905);
static _DELPHI_CONST System::Word GL_R11F_G11F_B10F_EXT = System::Word(0x8c3a);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_10F_11F_11F_REV_EXT = System::Word(0x8c3b);
static _DELPHI_CONST System::Word GL_RGBA_SIGNED_COMPONENTS_EXT = System::Word(0x8c3c);
static _DELPHI_CONST System::Word WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT = System::Word(0x20a8);
static _DELPHI_CONST System::Word GLX_RGBA_UNSIGNED_FLOAT_TYPE_EXT = System::Word(0x20b1);
static _DELPHI_CONST System::Int8 GLX_RGBA_UNSIGNED_FLOAT_BIT_EXT = System::Int8(0x8);
static _DELPHI_CONST System::Word GL_TEXTURE_1D_ARRAY_EXT = System::Word(0x8c18);
static _DELPHI_CONST System::Word GL_TEXTURE_2D_ARRAY_EXT = System::Word(0x8c1a);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_2D_ARRAY_EXT = System::Word(0x8c1b);
static _DELPHI_CONST System::Word GL_PROXY_TEXTURE_1D_ARRAY_EXT = System::Word(0x8c19);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_1D_ARRAY_EXT = System::Word(0x8c1c);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_2D_ARRAY_EXT = System::Word(0x8c1d);
static _DELPHI_CONST System::Word GL_MAX_ARRAY_TEXTURE_LAYERS_EXT = System::Word(0x88ff);
static _DELPHI_CONST System::Word GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT = System::Word(0x884e);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_EXT = System::Word(0x8c2a);
static _DELPHI_CONST System::Word GL_MAX_TEXTURE_BUFFER_SIZE_EXT = System::Word(0x8c2b);
static _DELPHI_CONST System::Word GL_TEXTURE_BINDING_BUFFER_EXT = System::Word(0x8c2c);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT = System::Word(0x8c2d);
static _DELPHI_CONST System::Word GL_TEXTURE_BUFFER_FORMAT_EXT = System::Word(0x8c2e);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_LATC1_EXT = System::Word(0x8c70);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = System::Word(0x8c71);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = System::Word(0x8c72);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = System::Word(0x8c73);
static _DELPHI_CONST System::Word GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI = System::Word(0x8837);
static _DELPHI_CONST System::Word GL_COMPRESSED_RED_RGTC1_EXT = System::Word(0x8dbb);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_RED_RGTC1_EXT = System::Word(0x8dbc);
static _DELPHI_CONST System::Word GL_COMPRESSED_RED_GREEN_RGTC2_EXT = System::Word(0x8dbd);
static _DELPHI_CONST System::Word GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = System::Word(0x8dbe);
static _DELPHI_CONST System::Word GL_RGB9_E5_EXT = System::Word(0x8c3d);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_5_9_9_9_REV_EXT = System::Word(0x8c3e);
static _DELPHI_CONST System::Word GL_TEXTURE_SHARED_SIZE_EXT = System::Word(0x8c3f);
static _DELPHI_CONST System::Word GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = System::Word(0x20b2);
static _DELPHI_CONST System::Word WGL_FRAMEBUFFER_SRGB_CAPABLE_EXT = System::Word(0x20a9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_SRGB_EXT = System::Word(0x8db9);
static _DELPHI_CONST System::Word GL_FRAMEBUFFER_SRGB_CAPABLE_EXT = System::Word(0x8dba);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_NV = System::Word(0x8c8e);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_START_NV = System::Word(0x8c84);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV = System::Word(0x8c85);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_RECORD_NV = System::Word(0x8c86);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV = System::Word(0x8c8f);
static _DELPHI_CONST System::Word GL_INTERLEAVED_ATTRIBS_NV = System::Word(0x8c8c);
static _DELPHI_CONST System::Word GL_SEPARATE_ATTRIBS_NV = System::Word(0x8c8d);
static _DELPHI_CONST System::Word GL_PRIMITIVES_GENERATED_NV = System::Word(0x8c87);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV = System::Word(0x8c88);
static _DELPHI_CONST System::Word GL_RASTERIZER_DISCARD_NV = System::Word(0x8c89);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV = System::Word(0x8c8a);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV = System::Word(0x8c8b);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV = System::Word(0x8c80);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_ATTRIBS_NV = System::Word(0x8c7e);
static _DELPHI_CONST System::Word GL_ACTIVE_VARYINGS_NV = System::Word(0x8c81);
static _DELPHI_CONST System::Word GL_ACTIVE_VARYING_MAX_LENGTH_NV = System::Word(0x8c82);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYINGS_NV = System::Word(0x8c83);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV = System::Word(0x8c7f);
static _DELPHI_CONST System::Word GL_BACK_PRIMARY_COLOR_NV = System::Word(0x8c77);
static _DELPHI_CONST System::Word GL_BACK_SECONDARY_COLOR_NV = System::Word(0x8c78);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_NV = System::Word(0x8c79);
static _DELPHI_CONST System::Word GL_CLIP_DISTANCE_NV = System::Word(0x8c7a);
static _DELPHI_CONST System::Word GL_VERTEX_ID_NV = System::Word(0x8c7b);
static _DELPHI_CONST System::Word GL_PRIMITIVE_ID_NV = System::Word(0x8c7c);
static _DELPHI_CONST System::Word GL_GENERIC_ATTRIB_NV = System::Word(0x8c7d);
static _DELPHI_CONST System::Word GL_LAYER_NV = System::Word(0x8daa);
static _DELPHI_CONST System::Word GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT = System::Word(0x8de2);
static _DELPHI_CONST System::Word GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT = System::Word(0x8de3);
static _DELPHI_CONST System::Word GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT = System::Word(0x8de4);
static _DELPHI_CONST System::Word GL_MAX_BINDABLE_UNIFORM_SIZE_EXT = System::Word(0x8ded);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_BINDING_EXT = System::Word(0x8def);
static _DELPHI_CONST System::Word GL_UNIFORM_BUFFER_EXT = System::Word(0x8dee);
static _DELPHI_CONST System::Word GL_RGBA_INTEGER_MODE_EXT = System::Word(0x8d9e);
static _DELPHI_CONST System::Word GL_RGBA32UI_EXT = System::Word(0x8d70);
static _DELPHI_CONST System::Word GL_RGB32UI_EXT = System::Word(0x8d71);
static _DELPHI_CONST System::Word GL_ALPHA32UI_EXT = System::Word(0x8d72);
static _DELPHI_CONST System::Word GL_INTENSITY32UI_EXT = System::Word(0x8d73);
static _DELPHI_CONST System::Word GL_LUMINANCE32UI_EXT = System::Word(0x8d74);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA32UI_EXT = System::Word(0x8d75);
static _DELPHI_CONST System::Word GL_RGBA16UI_EXT = System::Word(0x8d76);
static _DELPHI_CONST System::Word GL_RGB16UI_EXT = System::Word(0x8d77);
static _DELPHI_CONST System::Word GL_ALPHA16UI_EXT = System::Word(0x8d78);
static _DELPHI_CONST System::Word GL_INTENSITY16UI_EXT = System::Word(0x8d79);
static _DELPHI_CONST System::Word GL_LUMINANCE16UI_EXT = System::Word(0x8d7a);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA16UI_EXT = System::Word(0x8d7b);
static _DELPHI_CONST System::Word GL_RGBA8UI_EXT = System::Word(0x8d7c);
static _DELPHI_CONST System::Word GL_RGB8UI_EXT = System::Word(0x8d7d);
static _DELPHI_CONST System::Word GL_ALPHA8UI_EXT = System::Word(0x8d7e);
static _DELPHI_CONST System::Word GL_INTENSITY8UI_EXT = System::Word(0x8d7f);
static _DELPHI_CONST System::Word GL_LUMINANCE8UI_EXT = System::Word(0x8d80);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA8UI_EXT = System::Word(0x8d81);
static _DELPHI_CONST System::Word GL_RGBA32I_EXT = System::Word(0x8d82);
static _DELPHI_CONST System::Word GL_RGB32I_EXT = System::Word(0x8d83);
static _DELPHI_CONST System::Word GL_ALPHA32I_EXT = System::Word(0x8d84);
static _DELPHI_CONST System::Word GL_INTENSITY32I_EXT = System::Word(0x8d85);
static _DELPHI_CONST System::Word GL_LUMINANCE32I_EXT = System::Word(0x8d86);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA32I_EXT = System::Word(0x8d87);
static _DELPHI_CONST System::Word GL_RGBA16I_EXT = System::Word(0x8d88);
static _DELPHI_CONST System::Word GL_RGB16I_EXT = System::Word(0x8d89);
static _DELPHI_CONST System::Word GL_ALPHA16I_EXT = System::Word(0x8d8a);
static _DELPHI_CONST System::Word GL_INTENSITY16I_EXT = System::Word(0x8d8b);
static _DELPHI_CONST System::Word GL_LUMINANCE16I_EXT = System::Word(0x8d8c);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA16I_EXT = System::Word(0x8d8d);
static _DELPHI_CONST System::Word GL_RGBA8I_EXT = System::Word(0x8d8e);
static _DELPHI_CONST System::Word GL_RGB8I_EXT = System::Word(0x8d8f);
static _DELPHI_CONST System::Word GL_ALPHA8I_EXT = System::Word(0x8d90);
static _DELPHI_CONST System::Word GL_INTENSITY8I_EXT = System::Word(0x8d91);
static _DELPHI_CONST System::Word GL_LUMINANCE8I_EXT = System::Word(0x8d92);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA8I_EXT = System::Word(0x8d93);
static _DELPHI_CONST System::Word GL_RED_INTEGER_EXT = System::Word(0x8d94);
static _DELPHI_CONST System::Word GL_GREEN_INTEGER_EXT = System::Word(0x8d95);
static _DELPHI_CONST System::Word GL_BLUE_INTEGER_EXT = System::Word(0x8d96);
static _DELPHI_CONST System::Word GL_ALPHA_INTEGER_EXT = System::Word(0x8d97);
static _DELPHI_CONST System::Word GL_RGB_INTEGER_EXT = System::Word(0x8d98);
static _DELPHI_CONST System::Word GL_RGBA_INTEGER_EXT = System::Word(0x8d99);
static _DELPHI_CONST System::Word GL_BGR_INTEGER_EXT = System::Word(0x8d9a);
static _DELPHI_CONST System::Word GL_BGRA_INTEGER_EXT = System::Word(0x8d9b);
static _DELPHI_CONST System::Word GL_LUMINANCE_INTEGER_EXT = System::Word(0x8d9c);
static _DELPHI_CONST System::Word GL_LUMINANCE_ALPHA_INTEGER_EXT = System::Word(0x8d9d);
static _DELPHI_CONST System::Word GL_QUERY_WAIT_NV = System::Word(0x8e13);
static _DELPHI_CONST System::Word GL_QUERY_NO_WAIT_NV = System::Word(0x8e14);
static _DELPHI_CONST System::Word GL_QUERY_BY_REGION_WAIT_NV = System::Word(0x8e15);
static _DELPHI_CONST System::Word GL_QUERY_BY_REGION_NO_WAIT_NV = System::Word(0x8e16);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_EXT = System::Word(0x8c8e);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT = System::Word(0x8c84);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT = System::Word(0x8c85);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT = System::Word(0x8c8f);
static _DELPHI_CONST System::Word GL_INTERLEAVED_ATTRIBS_EXT = System::Word(0x8c8c);
static _DELPHI_CONST System::Word GL_SEPARATE_ATTRIBS_EXT = System::Word(0x8c8d);
static _DELPHI_CONST System::Word GL_PRIMITIVES_GENERATED_EXT = System::Word(0x8c87);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT = System::Word(0x8c88);
static _DELPHI_CONST System::Word GL_RASTERIZER_DISCARD_EXT = System::Word(0x8c89);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT = System::Word(0x8c8a);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT = System::Word(0x8c8b);
static _DELPHI_CONST System::Word GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT = System::Word(0x8c80);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYINGS_EXT = System::Word(0x8c83);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT = System::Word(0x8c7f);
static _DELPHI_CONST System::Word GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT = System::Word(0x8c76);
static _DELPHI_CONST System::Word GL_VBO_FREE_MEMORY_ATI = System::Word(0x87fb);
static _DELPHI_CONST System::Word GL_TEXTURE_FREE_MEMORY_ATI = System::Word(0x87fc);
static _DELPHI_CONST System::Word GL_RENDERBUFFER_FREE_MEMORY_ATI = System::Word(0x87fd);
static _DELPHI_CONST System::Word GL_SAMPLER_BUFFER_AMD = System::Word(0x9001);
static _DELPHI_CONST System::Word GL_INT_SAMPLER_BUFFER_AMD = System::Word(0x9002);
static _DELPHI_CONST System::Word GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD = System::Word(0x9003);
static _DELPHI_CONST System::Word GL_DISCRETE_AMD = System::Word(0x9006);
static _DELPHI_CONST System::Word GL_CONTINUOUS_AMD = System::Word(0x9007);
static _DELPHI_CONST System::Word GL_TESSELLATION_MODE_AMD = System::Word(0x9004);
static _DELPHI_CONST System::Word GL_TESSELLATION_FACTOR_AMD = System::Word(0x9005);
static _DELPHI_CONST System::Word GL_BUFFER_GPU_ADDRESS_NV = System::Word(0x8f1d);
static _DELPHI_CONST System::Word GL_GPU_ADDRESS_NV = System::Word(0x8f34);
static _DELPHI_CONST System::Word GL_MAX_SHADER_BUFFER_ADDRESS_NV = System::Word(0x8f35);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV = System::Word(0x8f1e);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_UNIFIED_NV = System::Word(0x8f1f);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV = System::Word(0x8f20);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_ADDRESS_NV = System::Word(0x8f21);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_ADDRESS_NV = System::Word(0x8f22);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_ADDRESS_NV = System::Word(0x8f23);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_ADDRESS_NV = System::Word(0x8f24);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_ADDRESS_NV = System::Word(0x8f25);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_ADDRESS_NV = System::Word(0x8f26);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV = System::Word(0x8f27);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_ADDRESS_NV = System::Word(0x8f28);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_ADDRESS_NV = System::Word(0x8f29);
static _DELPHI_CONST System::Word GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV = System::Word(0x8f2a);
static _DELPHI_CONST System::Word GL_VERTEX_ARRAY_LENGTH_NV = System::Word(0x8f2b);
static _DELPHI_CONST System::Word GL_NORMAL_ARRAY_LENGTH_NV = System::Word(0x8f2c);
static _DELPHI_CONST System::Word GL_COLOR_ARRAY_LENGTH_NV = System::Word(0x8f2d);
static _DELPHI_CONST System::Word GL_INDEX_ARRAY_LENGTH_NV = System::Word(0x8f2e);
static _DELPHI_CONST System::Word GL_TEXTURE_COORD_ARRAY_LENGTH_NV = System::Word(0x8f2f);
static _DELPHI_CONST System::Word GL_EDGE_FLAG_ARRAY_LENGTH_NV = System::Word(0x8f30);
static _DELPHI_CONST System::Word GL_SECONDARY_COLOR_ARRAY_LENGTH_NV = System::Word(0x8f31);
static _DELPHI_CONST System::Word GL_FOG_COORD_ARRAY_LENGTH_NV = System::Word(0x8f32);
static _DELPHI_CONST System::Word GL_ELEMENT_ARRAY_LENGTH_NV = System::Word(0x8f33);
static _DELPHI_CONST System::Word GL_SURFACE_STATE_NV = System::Word(0x86eb);
static _DELPHI_CONST System::Word GL_SURFACE_REGISTERED_NV = System::Word(0x86fd);
static _DELPHI_CONST System::Word GL_SURFACE_MAPPED_NV = System::Word(0x8700);
static _DELPHI_CONST System::Word GL_WRITE_DISCARD_NV = System::Word(0x88be);
static _DELPHI_CONST System::Word WGL_COLOR_SAMPLES_NV = System::Word(0x20b9);
static _DELPHI_CONST System::Word GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX = System::Word(0x9047);
static _DELPHI_CONST System::Word GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX = System::Word(0x9048);
static _DELPHI_CONST System::Word GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = System::Word(0x9049);
static _DELPHI_CONST System::Word GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX = System::Word(0x904a);
static _DELPHI_CONST System::Word GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX = System::Word(0x904b);
static _DELPHI_CONST System::Word GL_TEXTURE_SRGB_DECODE_EXT = System::Word(0x8a48);
static _DELPHI_CONST System::Word GL_DECODE_EXT = System::Word(0x8a49);
static _DELPHI_CONST System::Word GL_SKIP_DECODE_EXT = System::Word(0x8a4a);
static _DELPHI_CONST System::Int8 GL_CLOSE_PATH_NV = System::Int8(0x0);
static _DELPHI_CONST System::Int8 GL_MOVE_TO_NV = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_RELATIVE_MOVE_TO_NV = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GL_LINE_TO_NV = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_RELATIVE_LINE_TO_NV = System::Int8(0x5);
static _DELPHI_CONST System::Int8 GL_HORIZONTAL_LINE_TO_NV = System::Int8(0x6);
static _DELPHI_CONST System::Int8 GL_RELATIVE_HORIZONTAL_LINE_TO_NV = System::Int8(0x7);
static _DELPHI_CONST System::Int8 GL_VERTICAL_LINE_TO_NV = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_RELATIVE_VERTICAL_LINE_TO_NV = System::Int8(0x9);
static _DELPHI_CONST System::Int8 GL_QUADRATIC_CURVE_TO_NV = System::Int8(0xa);
static _DELPHI_CONST System::Int8 GL_RELATIVE_QUADRATIC_CURVE_TO_NV = System::Int8(0xb);
static _DELPHI_CONST System::Int8 GL_CUBIC_CURVE_TO_NV = System::Int8(0xc);
static _DELPHI_CONST System::Int8 GL_RELATIVE_CUBIC_CURVE_TO_NV = System::Int8(0xd);
static _DELPHI_CONST System::Int8 GL_SMOOTH_QUADRATIC_CURVE_TO_NV = System::Int8(0xe);
static _DELPHI_CONST System::Int8 GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV = System::Int8(0xf);
static _DELPHI_CONST System::Int8 GL_SMOOTH_CUBIC_CURVE_TO_NV = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV = System::Int8(0x11);
static _DELPHI_CONST System::Int8 GL_SMALL_CCW_ARC_TO_NV = System::Int8(0x12);
static _DELPHI_CONST System::Int8 GL_RELATIVE_SMALL_CCW_ARC_TO_NV = System::Int8(0x13);
static _DELPHI_CONST System::Int8 GL_SMALL_CW_ARC_TO_NV = System::Int8(0x14);
static _DELPHI_CONST System::Int8 GL_RELATIVE_SMALL_CW_ARC_TO_NV = System::Int8(0x15);
static _DELPHI_CONST System::Int8 GL_LARGE_CCW_ARC_TO_NV = System::Int8(0x16);
static _DELPHI_CONST System::Int8 GL_RELATIVE_LARGE_CCW_ARC_TO_NV = System::Int8(0x17);
static _DELPHI_CONST System::Int8 GL_LARGE_CW_ARC_TO_NV = System::Int8(0x18);
static _DELPHI_CONST System::Int8 GL_RELATIVE_LARGE_CW_ARC_TO_NV = System::Int8(0x19);
static _DELPHI_CONST System::Byte GL_CIRCULAR_CCW_ARC_TO_NV = System::Byte(0xf8);
static _DELPHI_CONST System::Byte GL_CIRCULAR_CW_ARC_TO_NV = System::Byte(0xfa);
static _DELPHI_CONST System::Byte GL_CIRCULAR_TANGENT_ARC_TO_NV = System::Byte(0xfc);
static _DELPHI_CONST System::Byte GL_ARC_TO_NV = System::Byte(0xfe);
static _DELPHI_CONST System::Byte GL_RELATIVE_ARC_TO_NV = System::Byte(0xff);
static _DELPHI_CONST System::Word GL_PATH_FORMAT_SVG_NV = System::Word(0x9070);
static _DELPHI_CONST System::Word GL_PATH_FORMAT_PS_NV = System::Word(0x9071);
static _DELPHI_CONST System::Word GL_STANDARD_FONT_NAME_NV = System::Word(0x9072);
static _DELPHI_CONST System::Word GL_SYSTEM_FONT_NAME_NV = System::Word(0x9073);
static _DELPHI_CONST System::Word GL_FILE_NAME_NV = System::Word(0x9074);
static _DELPHI_CONST System::Word GL_PATH_STROKE_WIDTH_NV = System::Word(0x9075);
static _DELPHI_CONST System::Word GL_PATH_END_CAPS_NV = System::Word(0x9076);
static _DELPHI_CONST System::Word GL_PATH_INITIAL_END_CAP_NV = System::Word(0x9077);
static _DELPHI_CONST System::Word GL_PATH_TERMINAL_END_CAP_NV = System::Word(0x9078);
static _DELPHI_CONST System::Word GL_PATH_JOIN_STYLE_NV = System::Word(0x9079);
static _DELPHI_CONST System::Word GL_PATH_MITER_LIMIT_NV = System::Word(0x907a);
static _DELPHI_CONST System::Word GL_PATH_DASH_CAPS_NV = System::Word(0x907b);
static _DELPHI_CONST System::Word GL_PATH_INITIAL_DASH_CAP_NV = System::Word(0x907c);
static _DELPHI_CONST System::Word GL_PATH_TERMINAL_DASH_CAP_NV = System::Word(0x907d);
static _DELPHI_CONST System::Word GL_PATH_DASH_OFFSET_NV = System::Word(0x907e);
static _DELPHI_CONST System::Word GL_PATH_CLIENT_LENGTH_NV = System::Word(0x907f);
static _DELPHI_CONST System::Word GL_PATH_FILL_MODE_NV = System::Word(0x9080);
static _DELPHI_CONST System::Word GL_PATH_FILL_MASK_NV = System::Word(0x9081);
static _DELPHI_CONST System::Word GL_PATH_FILL_COVER_MODE_NV = System::Word(0x9082);
static _DELPHI_CONST System::Word GL_PATH_STROKE_COVER_MODE_NV = System::Word(0x9083);
static _DELPHI_CONST System::Word GL_PATH_STROKE_MASK_NV = System::Word(0x9084);
static _DELPHI_CONST System::Word GL_PATH_SAMPLE_QUALITY_NV = System::Word(0x9085);
static _DELPHI_CONST System::Word GL_COUNT_UP_NV = System::Word(0x9088);
static _DELPHI_CONST System::Word GL_COUNT_DOWN_NV = System::Word(0x9089);
static _DELPHI_CONST System::Word GL_PATH_OBJECT_BOUNDING_BOX_NV = System::Word(0x908a);
static _DELPHI_CONST System::Word GL_CONVEX_HULL_NV = System::Word(0x908b);
static _DELPHI_CONST System::Word GL_BOUNDING_BOX_NV = System::Word(0x908d);
static _DELPHI_CONST System::Word GL_TRANSLATE_X_NV = System::Word(0x908e);
static _DELPHI_CONST System::Word GL_TRANSLATE_Y_NV = System::Word(0x908f);
static _DELPHI_CONST System::Word GL_TRANSLATE_2D_NV = System::Word(0x9090);
static _DELPHI_CONST System::Word GL_TRANSLATE_3D_NV = System::Word(0x9091);
static _DELPHI_CONST System::Word GL_AFFINE_2D_NV = System::Word(0x9092);
static _DELPHI_CONST System::Word GL_AFFINE_3D_NV = System::Word(0x9094);
static _DELPHI_CONST System::Word GL_TRANSPOSE_AFFINE_2D_NV = System::Word(0x9096);
static _DELPHI_CONST System::Word GL_TRANSPOSE_AFFINE_3D_NV = System::Word(0x9098);
static _DELPHI_CONST System::Word GL_UTF8_NV = System::Word(0x909a);
static _DELPHI_CONST System::Word GL_UTF16_NV = System::Word(0x909b);
static _DELPHI_CONST System::Word GL_BOUNDING_BOX_OF_BOUNDING_BOXES_NV = System::Word(0x909c);
static _DELPHI_CONST System::Word GL_PATH_COMMAND_COUNT_NV = System::Word(0x909d);
static _DELPHI_CONST System::Word GL_PATH_COORD_COUNT_NV = System::Word(0x909e);
static _DELPHI_CONST System::Word GL_PATH_DASH_ARRAY_COUNT_NV = System::Word(0x909f);
static _DELPHI_CONST System::Word GL_PATH_COMPUTED_LENGTH_NV = System::Word(0x90a0);
static _DELPHI_CONST System::Word GL_PATH_FILL_BOUNDING_BOX_NV = System::Word(0x90a1);
static _DELPHI_CONST System::Word GL_PATH_STROKE_BOUNDING_BOX_NV = System::Word(0x90a2);
static _DELPHI_CONST System::Word GL_SQUARE_NV = System::Word(0x90a3);
static _DELPHI_CONST System::Word GL_ROUND_NV = System::Word(0x90a4);
static _DELPHI_CONST System::Word GL_TRIANGULAR_NV = System::Word(0x90a5);
static _DELPHI_CONST System::Word GL_BEVEL_NV = System::Word(0x90a6);
static _DELPHI_CONST System::Word GL_MITER_REVERT_NV = System::Word(0x90a7);
static _DELPHI_CONST System::Word GL_MITER_TRUNCATE_NV = System::Word(0x90a8);
static _DELPHI_CONST System::Word GL_SKIP_MISSING_GLYPH_NV = System::Word(0x90a9);
static _DELPHI_CONST System::Word GL_USE_MISSING_GLYPH_NV = System::Word(0x90aa);
static _DELPHI_CONST System::Word GL_PATH_DASH_OFFSET_RESET_NV = System::Word(0x90b4);
static _DELPHI_CONST System::Word GL_MOVE_TO_RESETS_NV = System::Word(0x90b5);
static _DELPHI_CONST System::Word GL_MOVE_TO_CONTINUES_NV = System::Word(0x90b6);
static _DELPHI_CONST System::Int8 GL_BOLD_BIT_NV = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_ITALIC_BIT_NV = System::Int8(0x2);
static _DELPHI_CONST System::Word GL_PATH_ERROR_POSITION_NV = System::Word(0x90ab);
static _DELPHI_CONST System::Word GL_PATH_FOG_GEN_MODE_NV = System::Word(0x90ac);
static _DELPHI_CONST System::Int8 GL_GLYPH_WIDTH_BIT_NV = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GL_GLYPH_HEIGHT_BIT_NV = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GL_GLYPH_VERTICAL_BEARING_X_BIT_NV = System::Int8(0x20);
static _DELPHI_CONST System::Int8 GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV = System::Int8(0x40);
static _DELPHI_CONST System::Byte GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV = System::Byte(0x80);
static _DELPHI_CONST System::Word GL_GLYPH_HAS_KERNING_NV = System::Word(0x100);
static _DELPHI_CONST int GL_FONT_X_MIN_BOUNDS_NV = int(0x10000);
static _DELPHI_CONST int GL_FONT_Y_MIN_BOUNDS_NV = int(0x20000);
static _DELPHI_CONST int GL_FONT_X_MAX_BOUNDS_NV = int(0x40000);
static _DELPHI_CONST int GL_FONT_Y_MAX_BOUNDS_NV = int(0x80000);
static _DELPHI_CONST int GL_FONT_UNITS_PER_EM_NV = int(0x100000);
static _DELPHI_CONST int GL_FONT_ASCENDER_NV = int(0x200000);
static _DELPHI_CONST int GL_FONT_DESCENDER_NV = int(0x400000);
static _DELPHI_CONST int GL_FONT_HEIGHT_NV = int(0x800000);
static _DELPHI_CONST int GL_FONT_MAX_ADVANCE_WIDTH_NV = int(0x1000000);
static _DELPHI_CONST int GL_FONT_MAX_ADVANCE_HEIGHT_NV = int(0x2000000);
static _DELPHI_CONST int GL_FONT_UNDERLINE_POSITION_NV = int(0x4000000);
static _DELPHI_CONST int GL_FONT_UNDERLINE_THICKNESS_NV = int(0x8000000);
static _DELPHI_CONST int GL_FONT_HAS_KERNING_NV = int(0x10000000);
static _DELPHI_CONST System::Word GL_ACCUM_ADJACENT_PAIRS_NV = System::Word(0x90ad);
static _DELPHI_CONST System::Word GL_ADJACENT_PAIRS_NV = System::Word(0x90ae);
static _DELPHI_CONST System::Word GL_FIRST_TO_REST_NV = System::Word(0x90af);
static _DELPHI_CONST System::Word GL_PATH_GEN_MODE_NV = System::Word(0x90b0);
static _DELPHI_CONST System::Word GL_PATH_GEN_COEFF_NV = System::Word(0x90b1);
static _DELPHI_CONST System::Word GL_PATH_GEN_COLOR_FORMAT_NV = System::Word(0x90b2);
static _DELPHI_CONST System::Word GL_PATH_GEN_COMPONENTS_NV = System::Word(0x90b3);
static _DELPHI_CONST System::Word GL_PATH_STENCIL_FUNC_NV = System::Word(0x90b7);
static _DELPHI_CONST System::Word GL_PATH_STENCIL_REF_NV = System::Word(0x90b8);
static _DELPHI_CONST System::Word GL_PATH_STENCIL_VALUE_MASK_NV = System::Word(0x90b9);
static _DELPHI_CONST System::Word GL_PATH_STENCIL_DEPTH_OFFSET_FACTOR_NV = System::Word(0x90bd);
static _DELPHI_CONST System::Word GL_PATH_STENCIL_DEPTH_OFFSET_UNITS_NV = System::Word(0x90be);
static _DELPHI_CONST System::Word GL_PATH_COVER_DEPTH_FUNC_NV = System::Word(0x90bf);
static _DELPHI_CONST System::Int8 WGL_ACCESS_READ_ONLY_NV = System::Int8(0x0);
static _DELPHI_CONST System::Int8 WGL_ACCESS_READ_WRITE_NV = System::Int8(0x1);
static _DELPHI_CONST System::Int8 WGL_ACCESS_WRITE_DISCARD_NV = System::Int8(0x2);
#define GLX_EXTENSION_NAME L"GLX"
static _DELPHI_CONST System::Int8 GLX_USE_GL = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_BUFFER_SIZE = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_LEVEL = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GLX_RGBA = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_DOUBLEBUFFER = System::Int8(0x5);
static _DELPHI_CONST System::Int8 GLX_STEREO = System::Int8(0x6);
static _DELPHI_CONST System::Int8 GLX_AUX_BUFFERS = System::Int8(0x7);
static _DELPHI_CONST System::Int8 GLX_RED_SIZE = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GLX_GREEN_SIZE = System::Int8(0x9);
static _DELPHI_CONST System::Int8 GLX_BLUE_SIZE = System::Int8(0xa);
static _DELPHI_CONST System::Int8 GLX_ALPHA_SIZE = System::Int8(0xb);
static _DELPHI_CONST System::Int8 GLX_DEPTH_SIZE = System::Int8(0xc);
static _DELPHI_CONST System::Int8 GLX_STENCIL_SIZE = System::Int8(0xd);
static _DELPHI_CONST System::Int8 GLX_ACCUM_RED_SIZE = System::Int8(0xe);
static _DELPHI_CONST System::Int8 GLX_ACCUM_GREEN_SIZE = System::Int8(0xf);
static _DELPHI_CONST System::Int8 GLX_ACCUM_BLUE_SIZE = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GLX_ACCUM_ALPHA_SIZE = System::Int8(0x11);
static _DELPHI_CONST System::Int8 GLX_BAD_SCREEN = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_BAD_ATTRIBUTE = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_NO_EXTENSION = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GLX_BAD_VISUAL = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_BAD_CONTEXT = System::Int8(0x5);
static _DELPHI_CONST System::Int8 GLX_BAD_VALUE = System::Int8(0x6);
static _DELPHI_CONST System::Int8 GLX_BAD_ENUM = System::Int8(0x7);
static _DELPHI_CONST System::Int8 GLX_BAD_HYPERPIPE_CONFIG_SGIX = System::Int8(0x5b);
static _DELPHI_CONST System::Int8 GLX_BAD_HYPERPIPE_SGIX = System::Int8(0x5c);
static _DELPHI_CONST System::Int8 GLX_VENDOR = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_VERSION = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_EXTENSIONS = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GLX_CONFIG_CAVEAT = System::Int8(0x20);
static _DELPHI_CONST System::Word GLX_NONE = System::Word(0x8000);
static _DELPHI_CONST unsigned GLX_DONT_CARE = unsigned(0xffffffff);
static _DELPHI_CONST System::Word GLX_SLOW_CONFIG = System::Word(0x8001);
static _DELPHI_CONST System::Word GLX_NON_CONFORMANT_CONFIG = System::Word(0x800d);
static _DELPHI_CONST System::Int8 GLX_X_VISUAL_TYPE = System::Int8(0x22);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_TYPE = System::Int8(0x23);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_INDEX_VALUE = System::Int8(0x24);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_RED_VALUE = System::Int8(0x25);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_GREEN_VALUE = System::Int8(0x26);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_BLUE_VALUE = System::Int8(0x27);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_ALPHA_VALUE = System::Int8(0x28);
static _DELPHI_CONST System::Word GLX_TRUE_COLOR = System::Word(0x8002);
static _DELPHI_CONST System::Word GLX_DIRECT_COLOR = System::Word(0x8003);
static _DELPHI_CONST System::Word GLX_PSEUDO_COLOR = System::Word(0x8004);
static _DELPHI_CONST System::Word GLX_STATIC_COLOR = System::Word(0x8005);
static _DELPHI_CONST System::Word GLX_GRAY_SCALE = System::Word(0x8006);
static _DELPHI_CONST System::Word GLX_STATIC_GRAY = System::Word(0x8007);
static _DELPHI_CONST System::Word GLX_TRANSPARENT_RGB = System::Word(0x8008);
static _DELPHI_CONST System::Word GLX_TRANSPARENT_INDEX = System::Word(0x8009);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_WIDTH = System::Word(0x8016);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_HEIGHT = System::Word(0x8017);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_PIXELS = System::Word(0x8018);
static _DELPHI_CONST System::Word GLX_PRESERVED_CONTENTS = System::Word(0x801b);
static _DELPHI_CONST System::Word GLX_LARGEST_BUFFER = System::Word(0x801c);
static _DELPHI_CONST System::Word GLX_WIDTH = System::Word(0x801d);
static _DELPHI_CONST System::Word GLX_HEIGHT = System::Word(0x801e);
static _DELPHI_CONST System::Word GLX_EVENT_MASK = System::Word(0x801f);
static _DELPHI_CONST System::Word GLX_DRAWABLE_TYPE = System::Word(0x8010);
static _DELPHI_CONST System::Word GLX_FBCONFIG_ID = System::Word(0x8013);
static _DELPHI_CONST System::Word GLX_VISUAL_ID = System::Word(0x800b);
static _DELPHI_CONST System::Int8 GLX_WINDOW_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_PIXMAP_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_PBUFFER_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_WINDOW_BIT_SGIX = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_PIXMAP_BIT_SGIX = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_PBUFFER_BIT_SGIX = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_AUX_BUFFERS_BIT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GLX_FRONT_LEFT_BUFFER_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_FRONT_RIGHT_BUFFER_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_BACK_LEFT_BUFFER_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_BACK_RIGHT_BUFFER_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GLX_DEPTH_BUFFER_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Int8 GLX_STENCIL_BUFFER_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte GLX_ACCUM_BUFFER_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Int8 GLX_FRONT_LEFT_BUFFER_BIT_SGIX = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_FRONT_RIGHT_BUFFER_BIT_SGIX = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_BACK_LEFT_BUFFER_BIT_SGIX = System::Int8(0x4);
static _DELPHI_CONST System::Int8 GLX_BACK_RIGHT_BUFFER_BIT_SGIX = System::Int8(0x8);
static _DELPHI_CONST System::Int8 GLX_AUX_BUFFERS_BIT_SGIX = System::Int8(0x10);
static _DELPHI_CONST System::Int8 GLX_DEPTH_BUFFER_BIT_SGIX = System::Int8(0x20);
static _DELPHI_CONST System::Int8 GLX_STENCIL_BUFFER_BIT_SGIX = System::Int8(0x40);
static _DELPHI_CONST System::Byte GLX_ACCUM_BUFFER_BIT_SGIX = System::Byte(0x80);
static _DELPHI_CONST System::Word GLX_SAMPLE_BUFFERS_BIT_SGIX = System::Word(0x100);
static _DELPHI_CONST System::Word GLX_RENDER_TYPE = System::Word(0x8011);
static _DELPHI_CONST System::Word GLX_X_RENDERABLE = System::Word(0x8012);
static _DELPHI_CONST System::Word GLX_RGBA_TYPE = System::Word(0x8014);
static _DELPHI_CONST System::Int8 GLX_RGBA_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Word GLX_COLOR_INDEX_TYPE = System::Word(0x8015);
static _DELPHI_CONST System::Int8 GLX_COLOR_INDEX_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_RGBA_BIT_SGIX = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_COLOR_INDEX_BIT_SGIX = System::Int8(0x2);
static _DELPHI_CONST System::Word GLX_SCREEN = System::Word(0x800c);
static _DELPHI_CONST int GLX_PBUFFER_CLOBBER_MASK = int(0x8000000);
static _DELPHI_CONST System::Word GLX_DAMAGED = System::Word(0x8020);
static _DELPHI_CONST System::Word GLX_SAVED = System::Word(0x8021);
static _DELPHI_CONST System::Word GLX_WINDOW = System::Word(0x8022);
static _DELPHI_CONST System::Word GLX_PBUFFER = System::Word(0x8023);
static _DELPHI_CONST System::Word GLX_PBUFFER_HEIGHT = System::Word(0x8040);
static _DELPHI_CONST System::Word GLX_PBUFFER_WIDTH = System::Word(0x8041);
static _DELPHI_CONST System::Int8 GLX_X_VISUAL_TYPE_EXT = System::Int8(0x22);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_TYPE_EXT = System::Int8(0x23);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_INDEX_VALUE_EXT = System::Int8(0x24);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_RED_VALUE_EXT = System::Int8(0x25);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_GREEN_VALUE_EXT = System::Int8(0x26);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_BLUE_VALUE_EXT = System::Int8(0x27);
static _DELPHI_CONST System::Int8 GLX_TRANSPARENT_ALPHA_VALUE_EXT = System::Int8(0x28);
static _DELPHI_CONST System::Word GLX_TRUE_COLOR_EXT = System::Word(0x8002);
static _DELPHI_CONST System::Word GLX_DIRECT_COLOR_EXT = System::Word(0x8003);
static _DELPHI_CONST System::Word GLX_PSEUDO_COLOR_EXT = System::Word(0x8004);
static _DELPHI_CONST System::Word GLX_STATIC_COLOR_EXT = System::Word(0x8005);
static _DELPHI_CONST System::Word GLX_GRAY_SCALE_EXT = System::Word(0x8006);
static _DELPHI_CONST System::Word GLX_STATIC_GRAY_EXT = System::Word(0x8007);
static _DELPHI_CONST System::Word GLX_TRANSPARENT_RGB_EXT = System::Word(0x8008);
static _DELPHI_CONST System::Word GLX_TRANSPARENT_INDEX_EXT = System::Word(0x8009);
static _DELPHI_CONST System::Int8 GLX_VISUAL_CAVEAT_EXT = System::Int8(0x20);
static _DELPHI_CONST System::Word GLX_NONE_EXT = System::Word(0x8000);
static _DELPHI_CONST System::Word GLX_SLOW_VISUAL_EXT = System::Word(0x8001);
static _DELPHI_CONST System::Word GLX_NON_CONFORMANT_VISUAL_EXT = System::Word(0x800d);
static _DELPHI_CONST System::Word GLX_SHARE_CONTEXT_EXT = System::Word(0x800a);
static _DELPHI_CONST System::Word GLX_VISUAL_ID_EXT = System::Word(0x800b);
static _DELPHI_CONST System::Word GLX_SCREEN_EXT = System::Word(0x800c);
static _DELPHI_CONST System::Int8 GLX_3DFX_WINDOW_MODE_MESA = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_3DFX_FULLSCREEN_MODE_MESA = System::Int8(0x2);
static _DELPHI_CONST System::Word GLX_DRAWABLE_TYPE_SGIX = System::Word(0x8010);
static _DELPHI_CONST System::Word GLX_RENDER_TYPE_SGIX = System::Word(0x8011);
static _DELPHI_CONST System::Word GLX_X_RENDERABLE_SGIX = System::Word(0x8012);
static _DELPHI_CONST System::Word GLX_FBCONFIG_ID_SGIX = System::Word(0xbb8d);
static _DELPHI_CONST System::Word GLX_RGBA_TYPE_SGIX = System::Word(0x8014);
static _DELPHI_CONST System::Word GLX_COLOR_INDEX_TYPE_SGIX = System::Word(0x8015);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_WIDTH_SGIX = System::Word(0x8016);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_HEIGHT_SGIX = System::Word(0x8017);
static _DELPHI_CONST System::Word GLX_MAX_PBUFFER_PIXELS_SGIX = System::Word(0x8018);
static _DELPHI_CONST System::Word GLX_OPTIMAL_PBUFFER_WIDTH_SGIX = System::Word(0x8019);
static _DELPHI_CONST System::Word GLX_OPTIMAL_PBUFFER_HEIGHT_SGIX = System::Word(0x801a);
static _DELPHI_CONST System::Word GLX_PRESERVED_CONTENTS_SGIX = System::Word(0x801b);
static _DELPHI_CONST System::Word GLX_LARGEST_PBUFFER_SGIX = System::Word(0x801c);
static _DELPHI_CONST System::Word GLX_WIDTH_SGIX = System::Word(0x801d);
static _DELPHI_CONST System::Word GLX_HEIGHT_SGIX = System::Word(0x801e);
static _DELPHI_CONST System::Word GLX_EVENT_MASK_SGIX = System::Word(0x801f);
static _DELPHI_CONST System::Word GLX_DAMAGED_SGIX = System::Word(0x8020);
static _DELPHI_CONST System::Word GLX_SAVED_SGIX = System::Word(0x8021);
static _DELPHI_CONST System::Word GLX_WINDOW_SGIX = System::Word(0x8022);
static _DELPHI_CONST System::Word GLX_PBUFFER_SGIX = System::Word(0x8023);
static _DELPHI_CONST System::Word GLX_DIGITAL_MEDIA_PBUFFER_SGIX = System::Word(0x8024);
static _DELPHI_CONST System::Word GLX_BLENDED_RGBA_SGIS = System::Word(0x8025);
static _DELPHI_CONST System::Word GLX_MULTISAMPLE_SUB_RECT_WIDTH_SGIS = System::Word(0x8026);
static _DELPHI_CONST System::Word GLX_MULTISAMPLE_SUB_RECT_HEIGHT_SGIS = System::Word(0x8027);
static _DELPHI_CONST System::Word GLX_VISUAL_SELECT_GROUP_SGIX = System::Word(0x8028);
static _DELPHI_CONST System::Word GLX_HYPERPIPE_ID_SGIX = System::Word(0x8030);
static _DELPHI_CONST System::Int8 GLX_SYNC_FRAME_SGIX = System::Int8(0x0);
static _DELPHI_CONST System::Int8 GLX_SYNC_SWAP_SGIX = System::Int8(0x1);
static _DELPHI_CONST int GLX_BUFFER_CLOBBER_MASK_SGIX = int(0x8000000);
static _DELPHI_CONST int GLX_BUFFER_SWAP_COMPLETE_INTEL_MASK = int(0x4000000);
static _DELPHI_CONST System::Int8 GLX_HYPERPIPE_DISPLAY_PIPE_SGIX = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_HYPERPIPE_RENDER_PIPE_SGIX = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_PIPE_RECT_SGIX = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_PIPE_RECT_LIMITS_SGIX = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_HYPERPIPE_STEREO_SGIX = System::Int8(0x3);
static _DELPHI_CONST System::Int8 GLX_HYPERPIPE_PIXEL_AVERAGE_SGIX = System::Int8(0x4);
static _DELPHI_CONST System::Byte GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX = System::Byte(0x80);
static _DELPHI_CONST System::Int8 GLX_TEXTURE_1D_BIT_EXT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLX_TEXTURE_2D_BIT_EXT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 GLX_TEXTURE_RECTANGLE_BIT_EXT = System::Int8(0x4);
static _DELPHI_CONST System::Word GLX_SWAP_METHOD_OML = System::Word(0x8060);
static _DELPHI_CONST System::Word GLX_SWAP_EXCHANGE_OML = System::Word(0x8061);
static _DELPHI_CONST System::Word GLX_SWAP_COPY_OML = System::Word(0x8062);
static _DELPHI_CONST System::Word GLX_SWAP_UNDEFINED_OML = System::Word(0x8063);
static _DELPHI_CONST System::Word GLX_EXCHANGE_COMPLETE_INTEL = System::Word(0x8180);
static _DELPHI_CONST System::Word GLX_COPY_COMPLETE_INTEL = System::Word(0x8181);
static _DELPHI_CONST System::Word GLX_FLIP_COMPLETE_INTEL = System::Word(0x8182);
static _DELPHI_CONST System::Word GLX_COLOR_SAMPLES_NV = System::Word(0x20b3);
static _DELPHI_CONST System::Int8 AGL_NONE = System::Int8(0x0);
static _DELPHI_CONST System::Int8 AGL_ALL_RENDERERS = System::Int8(0x1);
static _DELPHI_CONST System::Int8 AGL_BUFFER_SIZE = System::Int8(0x2);
static _DELPHI_CONST System::Int8 AGL_LEVEL = System::Int8(0x3);
static _DELPHI_CONST System::Int8 AGL_RGBA = System::Int8(0x4);
static _DELPHI_CONST System::Int8 AGL_DOUBLEBUFFER = System::Int8(0x5);
static _DELPHI_CONST System::Int8 AGL_STEREO = System::Int8(0x6);
static _DELPHI_CONST System::Int8 AGL_AUX_BUFFERS = System::Int8(0x7);
static _DELPHI_CONST System::Int8 AGL_RED_SIZE = System::Int8(0x8);
static _DELPHI_CONST System::Int8 AGL_GREEN_SIZE = System::Int8(0x9);
static _DELPHI_CONST System::Int8 AGL_BLUE_SIZE = System::Int8(0xa);
static _DELPHI_CONST System::Int8 AGL_ALPHA_SIZE = System::Int8(0xb);
static _DELPHI_CONST System::Int8 AGL_DEPTH_SIZE = System::Int8(0xc);
static _DELPHI_CONST System::Int8 AGL_STENCIL_SIZE = System::Int8(0xd);
static _DELPHI_CONST System::Int8 AGL_ACCUM_RED_SIZE = System::Int8(0xe);
static _DELPHI_CONST System::Int8 AGL_ACCUM_GREEN_SIZE = System::Int8(0xf);
static _DELPHI_CONST System::Int8 AGL_ACCUM_BLUE_SIZE = System::Int8(0x10);
static _DELPHI_CONST System::Int8 AGL_ACCUM_ALPHA_SIZE = System::Int8(0x11);
static _DELPHI_CONST System::Int8 AGL_PIXEL_SIZE = System::Int8(0x32);
static _DELPHI_CONST System::Int8 AGL_MINIMUM_POLICY = System::Int8(0x33);
static _DELPHI_CONST System::Int8 AGL_MAXIMUM_POLICY = System::Int8(0x34);
static _DELPHI_CONST System::Int8 AGL_OFFSCREEN = System::Int8(0x35);
static _DELPHI_CONST System::Int8 AGL_FULLSCREEN = System::Int8(0x36);
static _DELPHI_CONST System::Int8 AGL_SAMPLE_BUFFERS_ARB = System::Int8(0x37);
static _DELPHI_CONST System::Int8 AGL_SAMPLES_ARB = System::Int8(0x38);
static _DELPHI_CONST System::Int8 AGL_AUX_DEPTH_STENCIL = System::Int8(0x39);
static _DELPHI_CONST System::Int8 AGL_COLOR_FLOAT = System::Int8(0x3a);
static _DELPHI_CONST System::Int8 AGL_MULTISAMPLE = System::Int8(0x3b);
static _DELPHI_CONST System::Int8 AGL_SUPERSAMPLE = System::Int8(0x3c);
static _DELPHI_CONST System::Int8 AGL_SAMPLE_ALPHA = System::Int8(0x3d);
static _DELPHI_CONST System::Int8 AGL_RENDERER_ID = System::Int8(0x46);
static _DELPHI_CONST System::Int8 AGL_SINGLE_RENDERER = System::Int8(0x47);
static _DELPHI_CONST System::Int8 AGL_NO_RECOVERY = System::Int8(0x48);
static _DELPHI_CONST System::Int8 AGL_ACCELERATED = System::Int8(0x49);
static _DELPHI_CONST System::Int8 AGL_CLOSEST_POLICY = System::Int8(0x4a);
static _DELPHI_CONST System::Int8 AGL_ROBUST = System::Int8(0x4b);
static _DELPHI_CONST System::Int8 AGL_BACKING_STORE = System::Int8(0x4c);
static _DELPHI_CONST System::Int8 AGL_MP_SAFE = System::Int8(0x4e);
static _DELPHI_CONST System::Int8 AGL_WINDOW = System::Int8(0x50);
static _DELPHI_CONST System::Int8 AGL_MULTISCREEN = System::Int8(0x51);
static _DELPHI_CONST System::Int8 AGL_VIRTUAL_SCREEN = System::Int8(0x52);
static _DELPHI_CONST System::Int8 AGL_COMPLIANT = System::Int8(0x53);
static _DELPHI_CONST System::Int8 AGL_PBUFFER = System::Int8(0x5a);
static _DELPHI_CONST System::Int8 AGL_REMOTE_PBUFFER = System::Int8(0x5b);
static _DELPHI_CONST System::Int8 AGL_BUFFER_MODES = System::Int8(0x64);
static _DELPHI_CONST System::Int8 AGL_MIN_LEVEL = System::Int8(0x65);
static _DELPHI_CONST System::Int8 AGL_MAX_LEVEL = System::Int8(0x66);
static _DELPHI_CONST System::Int8 AGL_COLOR_MODES = System::Int8(0x67);
static _DELPHI_CONST System::Int8 AGL_ACCUM_MODES = System::Int8(0x68);
static _DELPHI_CONST System::Int8 AGL_DEPTH_MODES = System::Int8(0x69);
static _DELPHI_CONST System::Int8 AGL_STENCIL_MODES = System::Int8(0x6a);
static _DELPHI_CONST System::Int8 AGL_MAX_AUX_BUFFERS = System::Int8(0x6b);
static _DELPHI_CONST System::Int8 AGL_VIDEO_MEMORY = System::Int8(0x78);
static _DELPHI_CONST System::Int8 AGL_TEXTURE_MEMORY = System::Int8(0x79);
static _DELPHI_CONST System::Byte AGL_RENDERER_COUNT = System::Byte(0x80);
static _DELPHI_CONST System::Byte AGL_SWAP_RECT = System::Byte(0xc8);
static _DELPHI_CONST System::Byte AGL_BUFFER_RECT = System::Byte(0xca);
static _DELPHI_CONST System::Byte AGL_SWAP_LIMIT = System::Byte(0xcb);
static _DELPHI_CONST System::Byte AGL_COLORMAP_TRACKING = System::Byte(0xd2);
static _DELPHI_CONST System::Byte AGL_COLORMAP_ENTRY = System::Byte(0xd4);
static _DELPHI_CONST System::Byte AGL_RASTERIZATION = System::Byte(0xdc);
static _DELPHI_CONST System::Byte AGL_SWAP_INTERVAL = System::Byte(0xde);
static _DELPHI_CONST System::Byte AGL_STATE_VALIDATION = System::Byte(0xe6);
static _DELPHI_CONST System::Byte AGL_BUFFER_NAME = System::Byte(0xe7);
static _DELPHI_CONST System::Byte AGL_ORDER_CONTEXT_TO_FRONT = System::Byte(0xe8);
static _DELPHI_CONST System::Byte AGL_CONTEXT_SURFACE_ID = System::Byte(0xe9);
static _DELPHI_CONST System::Byte AGL_CONTEXT_DISPLAY_ID = System::Byte(0xea);
static _DELPHI_CONST System::Byte AGL_SURFACE_ORDER = System::Byte(0xeb);
static _DELPHI_CONST System::Byte AGL_SURFACE_OPACITY = System::Byte(0xec);
static _DELPHI_CONST System::Byte AGL_CLIP_REGION = System::Byte(0xfe);
static _DELPHI_CONST System::Byte AGL_FS_CAPTURE_SINGLE = System::Byte(0xff);
static _DELPHI_CONST System::Word AGL_SURFACE_BACKING_SIZE = System::Word(0x130);
static _DELPHI_CONST System::Word AGL_ENABLE_SURFACE_BACKING_SIZE = System::Word(0x131);
static _DELPHI_CONST System::Word AGL_SURFACE_VOLATILE = System::Word(0x132);
static _DELPHI_CONST System::Word AGL_FORMAT_CACHE_SIZE = System::Word(0x1f5);
static _DELPHI_CONST System::Word AGL_CLEAR_FORMAT_CACHE = System::Word(0x1f6);
static _DELPHI_CONST System::Word AGL_RETAIN_RENDERERS = System::Word(0x1f7);
static _DELPHI_CONST System::Int8 AGL_MONOSCOPIC_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 AGL_STEREOSCOPIC_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 AGL_SINGLEBUFFER_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 AGL_DOUBLEBUFFER_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 AGL_0_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 AGL_1_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 AGL_2_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 AGL_3_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 AGL_4_BIT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 AGL_5_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Int8 AGL_6_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte AGL_8_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Word AGL_10_BIT = System::Word(0x100);
static _DELPHI_CONST System::Word AGL_12_BIT = System::Word(0x200);
static _DELPHI_CONST System::Word AGL_16_BIT = System::Word(0x400);
static _DELPHI_CONST System::Word AGL_24_BIT = System::Word(0x800);
static _DELPHI_CONST System::Word AGL_32_BIT = System::Word(0x1000);
static _DELPHI_CONST System::Word AGL_48_BIT = System::Word(0x2000);
static _DELPHI_CONST System::Word AGL_64_BIT = System::Word(0x4000);
static _DELPHI_CONST System::Word AGL_96_BIT = System::Word(0x8000);
static _DELPHI_CONST int AGL_128_BIT = int(0x10000);
static _DELPHI_CONST System::Int8 AGL_RGB8_BIT = System::Int8(0x1);
static _DELPHI_CONST System::Int8 AGL_RGB8_A8_BIT = System::Int8(0x2);
static _DELPHI_CONST System::Int8 AGL_BGR233_BIT = System::Int8(0x4);
static _DELPHI_CONST System::Int8 AGL_BGR233_A8_BIT = System::Int8(0x8);
static _DELPHI_CONST System::Int8 AGL_RGB332_BIT = System::Int8(0x10);
static _DELPHI_CONST System::Int8 AGL_RGB332_A8_BIT = System::Int8(0x20);
static _DELPHI_CONST System::Int8 AGL_RGB444_BIT = System::Int8(0x40);
static _DELPHI_CONST System::Byte AGL_ARGB4444_BIT = System::Byte(0x80);
static _DELPHI_CONST System::Word AGL_RGB444_A8_BIT = System::Word(0x100);
static _DELPHI_CONST System::Word AGL_RGB555_BIT = System::Word(0x200);
static _DELPHI_CONST System::Word AGL_ARGB1555_BIT = System::Word(0x400);
static _DELPHI_CONST System::Word AGL_RGB555_A8_BIT = System::Word(0x800);
static _DELPHI_CONST System::Word AGL_RGB565_BIT = System::Word(0x1000);
static _DELPHI_CONST System::Word AGL_RGB565_A8_BIT = System::Word(0x2000);
static _DELPHI_CONST System::Word AGL_RGB888_BIT = System::Word(0x4000);
static _DELPHI_CONST System::Word AGL_ARGB8888_BIT = System::Word(0x8000);
static _DELPHI_CONST int AGL_RGB888_A8_BIT = int(0x10000);
static _DELPHI_CONST int AGL_RGB101010_BIT = int(0x20000);
static _DELPHI_CONST int AGL_ARGB2101010_BIT = int(0x40000);
static _DELPHI_CONST int AGL_RGB101010_A8_BIT = int(0x80000);
static _DELPHI_CONST int AGL_RGB121212_BIT = int(0x100000);
static _DELPHI_CONST int AGL_ARGB12121212_BIT = int(0x200000);
static _DELPHI_CONST int AGL_RGB161616_BIT = int(0x400000);
static _DELPHI_CONST int AGL_ARGB16161616_BIT = int(0x800000);
static _DELPHI_CONST int AGL_INDEX8_BIT = int(0x20000000);
static _DELPHI_CONST int AGL_INDEX16_BIT = int(0x40000000);
static _DELPHI_CONST int AGL_RGBFLOAT64_BIT = int(0x1000000);
static _DELPHI_CONST int AGL_RGBAFLOAT64_BIT = int(0x2000000);
static _DELPHI_CONST int AGL_RGBFLOAT128_BIT = int(0x4000000);
static _DELPHI_CONST int AGL_RGBAFLOAT128_BIT = int(0x8000000);
static _DELPHI_CONST int AGL_RGBFLOAT256_BIT = int(0x10000000);
static _DELPHI_CONST int AGL_RGBAFLOAT256_BIT = int(0x20000000);
static _DELPHI_CONST System::Int8 AGL_NO_ERROR = System::Int8(0x0);
static _DELPHI_CONST System::Word AGL_BAD_ATTRIBUTE = System::Word(0x2710);
static _DELPHI_CONST System::Word AGL_BAD_PROPERTY = System::Word(0x2711);
static _DELPHI_CONST System::Word AGL_BAD_PIXELFMT = System::Word(0x2712);
static _DELPHI_CONST System::Word AGL_BAD_RENDINFO = System::Word(0x2713);
static _DELPHI_CONST System::Word AGL_BAD_CONTEXT = System::Word(0x2714);
static _DELPHI_CONST System::Word AGL_BAD_DRAWABLE = System::Word(0x2715);
static _DELPHI_CONST System::Word AGL_BAD_GDEV = System::Word(0x2716);
static _DELPHI_CONST System::Word AGL_BAD_STATE = System::Word(0x2717);
static _DELPHI_CONST System::Word AGL_BAD_VALUE = System::Word(0x2718);
static _DELPHI_CONST System::Word AGL_BAD_MATCH = System::Word(0x2719);
static _DELPHI_CONST System::Word AGL_BAD_ENUM = System::Word(0x271a);
static _DELPHI_CONST System::Word AGL_BAD_OFFSCREEN = System::Word(0x271b);
static _DELPHI_CONST System::Word AGL_BAD_FULLSCREEN = System::Word(0x271c);
static _DELPHI_CONST System::Word AGL_BAD_WINDOW = System::Word(0x271d);
static _DELPHI_CONST System::Word AGL_BAD_POINTER = System::Word(0x271e);
static _DELPHI_CONST System::Word AGL_BAD_MODULE = System::Word(0x271f);
static _DELPHI_CONST System::Word AGL_BAD_ALLOC = System::Word(0x2720);
static _DELPHI_CONST System::Word AGL_BAD_CONNECTION = System::Word(0x2721);
static _DELPHI_CONST int GLU_INVALID_ENUM = int(0x18a24);
static _DELPHI_CONST int GLU_INVALID_VALUE = int(0x18a25);
static _DELPHI_CONST int GLU_OUT_OF_MEMORY = int(0x18a26);
static _DELPHI_CONST int GLU_INCOMPATIBLE_GL_VERSION = int(0x18a27);
static _DELPHI_CONST int GLU_VERSION = int(0x189c0);
static _DELPHI_CONST int GLU_EXTENSIONS = int(0x189c1);
static _DELPHI_CONST System::Int8 GLU_TRUE = System::Int8(0x1);
static _DELPHI_CONST System::Int8 GLU_FALSE = System::Int8(0x0);
static _DELPHI_CONST int GLU_SMOOTH = int(0x186a0);
static _DELPHI_CONST int GLU_FLAT = int(0x186a1);
static _DELPHI_CONST int GLU_NONE = int(0x186a2);
static _DELPHI_CONST int GLU_POINT = int(0x186aa);
static _DELPHI_CONST int GLU_LINE = int(0x186ab);
static _DELPHI_CONST int GLU_FILL = int(0x186ac);
static _DELPHI_CONST int GLU_SILHOUETTE = int(0x186ad);
static _DELPHI_CONST int GLU_OUTSIDE = int(0x186b4);
static _DELPHI_CONST int GLU_INSIDE = int(0x186b5);
static const System::Extended GLU_TESS_MAX_COORD = 1.000000E+150;
static _DELPHI_CONST int GLU_TESS_WINDING_RULE = int(0x1872c);
static _DELPHI_CONST int GLU_TESS_BOUNDARY_ONLY = int(0x1872d);
static _DELPHI_CONST int GLU_TESS_TOLERANCE = int(0x1872e);
static _DELPHI_CONST int GLU_TESS_WINDING_ODD = int(0x18722);
static _DELPHI_CONST int GLU_TESS_WINDING_NONZERO = int(0x18723);
static _DELPHI_CONST int GLU_TESS_WINDING_POSITIVE = int(0x18724);
static _DELPHI_CONST int GLU_TESS_WINDING_NEGATIVE = int(0x18725);
static _DELPHI_CONST int GLU_TESS_WINDING_ABS_GEQ_TWO = int(0x18726);
static _DELPHI_CONST int GLU_TESS_BEGIN = int(0x18704);
static _DELPHI_CONST int GLU_TESS_VERTEX = int(0x18705);
static _DELPHI_CONST int GLU_TESS_END = int(0x18706);
static _DELPHI_CONST int GLU_TESS_ERROR = int(0x18707);
static _DELPHI_CONST int GLU_TESS_EDGE_FLAG = int(0x18708);
static _DELPHI_CONST int GLU_TESS_COMBINE = int(0x18709);
static _DELPHI_CONST int GLU_TESS_BEGIN_DATA = int(0x1870a);
static _DELPHI_CONST int GLU_TESS_VERTEX_DATA = int(0x1870b);
static _DELPHI_CONST int GLU_TESS_END_DATA = int(0x1870c);
static _DELPHI_CONST int GLU_TESS_ERROR_DATA = int(0x1870d);
static _DELPHI_CONST int GLU_TESS_EDGE_FLAG_DATA = int(0x1870e);
static _DELPHI_CONST int GLU_TESS_COMBINE_DATA = int(0x1870f);
static _DELPHI_CONST int GLU_TESS_ERROR1 = int(0x18737);
static _DELPHI_CONST int GLU_TESS_ERROR2 = int(0x18738);
static _DELPHI_CONST int GLU_TESS_ERROR3 = int(0x18739);
static _DELPHI_CONST int GLU_TESS_ERROR4 = int(0x1873a);
static _DELPHI_CONST int GLU_TESS_ERROR5 = int(0x1873b);
static _DELPHI_CONST int GLU_TESS_ERROR6 = int(0x1873c);
static _DELPHI_CONST int GLU_TESS_ERROR7 = int(0x1873d);
static _DELPHI_CONST int GLU_TESS_ERROR8 = int(0x1873e);
static _DELPHI_CONST int GLU_TESS_MISSING_BEGIN_POLYGON = int(0x18737);
static _DELPHI_CONST int GLU_TESS_MISSING_BEGIN_CONTOUR = int(0x18738);
static _DELPHI_CONST int GLU_TESS_MISSING_END_POLYGON = int(0x18739);
static _DELPHI_CONST int GLU_TESS_MISSING_END_CONTOUR = int(0x1873a);
static _DELPHI_CONST int GLU_TESS_COORD_TOO_LARGE = int(0x1873b);
static _DELPHI_CONST int GLU_TESS_NEED_COMBINE_CALLBACK = int(0x1873c);
static _DELPHI_CONST int GLU_AUTO_LOAD_MATRIX = int(0x18768);
static _DELPHI_CONST int GLU_CULLING = int(0x18769);
static _DELPHI_CONST int GLU_SAMPLING_TOLERANCE = int(0x1876b);
static _DELPHI_CONST int GLU_DISPLAY_MODE = int(0x1876c);
static _DELPHI_CONST int GLU_PARAMETRIC_TOLERANCE = int(0x1876a);
static _DELPHI_CONST int GLU_SAMPLING_METHOD = int(0x1876d);
static _DELPHI_CONST int GLU_U_STEP = int(0x1876e);
static _DELPHI_CONST int GLU_V_STEP = int(0x1876f);
static _DELPHI_CONST int GLU_PATH_LENGTH = int(0x18777);
static _DELPHI_CONST int GLU_PARAMETRIC_ERROR = int(0x18778);
static _DELPHI_CONST int GLU_DOMAIN_DISTANCE = int(0x18779);
static _DELPHI_CONST int GLU_MAP1_TRIM_2 = int(0x18772);
static _DELPHI_CONST int GLU_MAP1_TRIM_3 = int(0x18773);
static _DELPHI_CONST int GLU_OUTLINE_POLYGON = int(0x18790);
static _DELPHI_CONST int GLU_OUTLINE_PATCH = int(0x18791);
static _DELPHI_CONST int GLU_NURBS_ERROR1 = int(0x1879b);
static _DELPHI_CONST int GLU_NURBS_ERROR2 = int(0x1879c);
static _DELPHI_CONST int GLU_NURBS_ERROR3 = int(0x1879d);
static _DELPHI_CONST int GLU_NURBS_ERROR4 = int(0x1879e);
static _DELPHI_CONST int GLU_NURBS_ERROR5 = int(0x1879f);
static _DELPHI_CONST int GLU_NURBS_ERROR6 = int(0x187a0);
static _DELPHI_CONST int GLU_NURBS_ERROR7 = int(0x187a1);
static _DELPHI_CONST int GLU_NURBS_ERROR8 = int(0x187a2);
static _DELPHI_CONST int GLU_NURBS_ERROR9 = int(0x187a3);
static _DELPHI_CONST int GLU_NURBS_ERROR10 = int(0x187a4);
static _DELPHI_CONST int GLU_NURBS_ERROR11 = int(0x187a5);
static _DELPHI_CONST int GLU_NURBS_ERROR12 = int(0x187a6);
static _DELPHI_CONST int GLU_NURBS_ERROR13 = int(0x187a7);
static _DELPHI_CONST int GLU_NURBS_ERROR14 = int(0x187a8);
static _DELPHI_CONST int GLU_NURBS_ERROR15 = int(0x187a9);
static _DELPHI_CONST int GLU_NURBS_ERROR16 = int(0x187aa);
static _DELPHI_CONST int GLU_NURBS_ERROR17 = int(0x187ab);
static _DELPHI_CONST int GLU_NURBS_ERROR18 = int(0x187ac);
static _DELPHI_CONST int GLU_NURBS_ERROR19 = int(0x187ad);
static _DELPHI_CONST int GLU_NURBS_ERROR20 = int(0x187ae);
static _DELPHI_CONST int GLU_NURBS_ERROR21 = int(0x187af);
static _DELPHI_CONST int GLU_NURBS_ERROR22 = int(0x187b0);
static _DELPHI_CONST int GLU_NURBS_ERROR23 = int(0x187b1);
static _DELPHI_CONST int GLU_NURBS_ERROR24 = int(0x187b2);
static _DELPHI_CONST int GLU_NURBS_ERROR25 = int(0x187b3);
static _DELPHI_CONST int GLU_NURBS_ERROR26 = int(0x187b4);
static _DELPHI_CONST int GLU_NURBS_ERROR27 = int(0x187b5);
static _DELPHI_CONST int GLU_NURBS_ERROR28 = int(0x187b6);
static _DELPHI_CONST int GLU_NURBS_ERROR29 = int(0x187b7);
static _DELPHI_CONST int GLU_NURBS_ERROR30 = int(0x187b8);
static _DELPHI_CONST int GLU_NURBS_ERROR31 = int(0x187b9);
static _DELPHI_CONST int GLU_NURBS_ERROR32 = int(0x187ba);
static _DELPHI_CONST int GLU_NURBS_ERROR33 = int(0x187bb);
static _DELPHI_CONST int GLU_NURBS_ERROR34 = int(0x187bc);
static _DELPHI_CONST int GLU_NURBS_ERROR35 = int(0x187bd);
static _DELPHI_CONST int GLU_NURBS_ERROR36 = int(0x187be);
static _DELPHI_CONST int GLU_NURBS_ERROR37 = int(0x187bf);
static _DELPHI_CONST int GLU_CW = int(0x18718);
static _DELPHI_CONST int GLU_CCW = int(0x18719);
static _DELPHI_CONST int GLU_INTERIOR = int(0x1871a);
static _DELPHI_CONST int GLU_EXTERIOR = int(0x1871b);
static _DELPHI_CONST int GLU_UNKNOWN = int(0x1871c);
static _DELPHI_CONST int GLU_BEGIN = int(0x18704);
static _DELPHI_CONST int GLU_VERTEX = int(0x18705);
static _DELPHI_CONST int GLU_END = int(0x18706);
static _DELPHI_CONST int GLU_ERROR = int(0x18707);
static _DELPHI_CONST int GLU_EDGE_FLAG = int(0x18708);
}	/* namespace Opengltokens */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OPENGLTOKENS)
using namespace Opengltokens;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OpengltokensHPP
