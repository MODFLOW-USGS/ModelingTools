﻿// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Glwin32viewer.pas' rev: 36.00 (Windows)

#ifndef Glwin32viewerHPP
#define Glwin32viewerHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Glscene.hpp>
#include <Glwin32context.hpp>
#include <Glcontext.hpp>
#include <System.Uitypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glwin32viewer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSceneViewer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTouchEvent)(int X, int Y, int TouchWidth, int TouchHeight, unsigned TouchID, int TouchCount, bool FromPen);

class PASCALIMPLEMENTATION TGLSceneViewer : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
private:
	Glscene::TGLSceneBuffer* FBuffer;
	Glcontext::TGLVSyncMode FVSync;
	HDC FOwnDC;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	bool FMouseInControl;
	System::Types::TPoint FLastScreenPos;
	bool FPenAsTouch;
	TTouchEvent FOnTouchMove;
	TTouchEvent FOnTouchUp;
	TTouchEvent FOnTouchDown;
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall WMGetDglCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMDestroy &Message);
	MESSAGE void __fastcall WMTouch(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &msg);
	float __fastcall GetFieldOfView();
	void __fastcall SetFieldOfView(const float Value);
	bool __fastcall GetIsRenderingContextAvailable();
	
protected:
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender();
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender();
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender();
	void __fastcall SetCamera(Glscene::TGLCamera* const val);
	Glscene::TGLCamera* __fastcall GetCamera();
	void __fastcall SetBuffer(Glscene::TGLSceneBuffer* const val);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall Loaded();
	virtual void __fastcall DoBeforeRender(System::TObject* Sender);
	void __fastcall DoBufferChange(System::TObject* Sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSceneViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSceneViewer();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall RecreateWnd();
	__property bool IsRenderingContextAvailable = {read=GetIsRenderingContextAvailable, nodefault};
	float __fastcall LastFrameTime();
	float __fastcall FramesPerSecond();
	System::UnicodeString __fastcall FramesPerSecondText(int decimals = 0x1);
	void __fastcall ResetPerformanceMonitor();
	Vcl::Graphics::TBitmap* __fastcall CreateSnapShotBitmap();
	void __fastcall RegisterTouch();
	void __fastcall UnregisterTouch();
	__property HDC RenderDC = {read=FOwnDC};
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	
__published:
	__property Glscene::TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property Glcontext::TGLVSyncMode VSync = {read=FVSync, write=FVSync, default=1};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property Glscene::TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
	__property float FieldOfView = {read=GetFieldOfView, write=SetFieldOfView};
	__property bool PenAsTouch = {read=FPenAsTouch, write=FPenAsTouch, nodefault};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property TTouchEvent OnTouchMove = {read=FOnTouchMove, write=FOnTouchMove};
	__property TTouchEvent OnTouchUp = {read=FOnTouchUp, write=FOnTouchUp};
	__property TTouchEvent OnTouchDown = {read=FOnTouchDown, write=FOnTouchDown};
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property HelpContext = {default=0};
	__property Hint = {default=0};
	__property PopupMenu;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnContextPopup;
	__property TabStop = {default=0};
	__property TabOrder = {default=-1};
	__property OnEnter;
	__property OnExit;
	__property OnGesture;
	__property Touch;
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneViewer(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetupVSync(const Glcontext::TGLVSyncMode AVSyncMode);
}	/* namespace Glwin32viewer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWIN32VIEWER)
using namespace Glwin32viewer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glwin32viewerHPP
