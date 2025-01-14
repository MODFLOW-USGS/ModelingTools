// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLWin32Context.pas' rev: 36.00 (Windows)

#ifndef Glwin32contextHPP
#define Glwin32contextHPP

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
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <OpenGLTokens.hpp>
#include <OpenGLAdapter.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLContext.hpp>
#include <GLState.hpp>
#include <GLSLog.hpp>
#include <GLStrings.hpp>
#include <GLVectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glwin32context
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLWin32Context;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLWin32Context : public Glcontext::TGLContext
{
	typedef Glcontext::TGLContext inherited;
	
	
private:
	typedef System::DynamicArray<int> _TGLWin32Context__1;
	
	typedef System::DynamicArray<float> _TGLWin32Context__2;
	
	
protected:
	HDC FDC;
	HGLRC FRC;
	TGLWin32Context* FShareContext;
	int FHPBUFFER;
	_TGLWin32Context__1 FiAttribs;
	_TGLWin32Context__2 FfAttribs;
	bool FLegacyContextsOnly;
	bool FSwapBufferSupported;
	void __fastcall SpawnLegacyContext(HDC aDC);
	virtual void __fastcall CreateOldContext(HDC aDC);
	virtual void __fastcall CreateNewContext(HDC aDC);
	void __fastcall ClearIAttribs();
	void __fastcall AddIAttrib(int attrib, int value);
	void __fastcall ChangeIAttrib(int attrib, int newValue);
	void __fastcall DropIAttrib(int attrib);
	void __fastcall ClearFAttribs();
	void __fastcall AddFAttrib(float attrib, float value);
	void __fastcall DestructionEarlyWarning(System::TObject* sender);
	void __fastcall ChooseWGLFormat(HDC DC, unsigned nMaxFormats, System::PInteger piFormats, int &nNumFormats, int BufferCount = 0x1);
	virtual void __fastcall DoCreateContext(HDC ADeviceHandle);
	virtual void __fastcall DoCreateMemoryContext(HWND outputDevice, int width, int height, int BufferCount);
	virtual bool __fastcall DoShareLists(Glcontext::TGLContext* aContext);
	virtual void __fastcall DoDestroyContext();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoDeactivate();
	
public:
	__fastcall virtual TGLWin32Context();
	__fastcall virtual ~TGLWin32Context();
	virtual bool __fastcall IsValid();
	virtual void __fastcall SwapBuffers();
	virtual void * __fastcall RenderOutputDevice();
	__property HDC DC = {read=FDC, nodefault};
	__property HGLRC RC = {read=FRC, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vUseWindowTrackingHook;
extern DELPHI_PACKAGE HWND __fastcall CreateTempWnd(void);
}	/* namespace Glwin32context */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWIN32CONTEXT)
using namespace Glwin32context;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glwin32contextHPP
