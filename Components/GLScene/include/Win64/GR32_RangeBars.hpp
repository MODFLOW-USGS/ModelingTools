// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_RangeBars.pas' rev: 35.00 (Windows)

#ifndef Gr32_rangebarsHPP
#define Gr32_rangebarsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <GR32.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_rangebars
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TArrowBar;
class DELPHICLASS TCustomRangeBar;
class DELPHICLASS TRangeBar;
class DELPHICLASS TCustomGaugeBar;
class DELPHICLASS TGaugeBar;
class DELPHICLASS TArrowBarAccess;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TRBDirection : unsigned char { drLeft, drUp, drRight, drDown };

typedef System::Set<TRBDirection, TRBDirection::drLeft, TRBDirection::drDown> TRBDirections;

enum DECLSPEC_DENUM TRBZone : unsigned char { zNone, zBtnPrev, zTrackPrev, zHandle, zTrackNext, zBtnNext };

enum DECLSPEC_DENUM TRBStyle : unsigned char { rbsDefault, rbsMac };

enum DECLSPEC_DENUM TRBBackgnd : unsigned char { bgPattern, bgSolid };

typedef void __fastcall (__closure *TRBGetSizeEvent)(System::TObject* Sender, int &Size);

class PASCALIMPLEMENTATION TArrowBar : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	TRBBackgnd FBackgnd;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	int FButtonSize;
	System::Uitypes::TColor FHandleColor;
	System::Uitypes::TColor FButtoncolor;
	System::Uitypes::TColor FHighLightColor;
	System::Uitypes::TColor FShadowColor;
	System::Uitypes::TColor FBorderColor;
	Vcl::Forms::TScrollBarKind FKind;
	bool FShowArrows;
	bool FShowHandleGrip;
	TRBStyle FStyle;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnUserChange;
	void __fastcall SetButtonSize(int Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetHandleColor(System::Uitypes::TColor Value);
	void __fastcall SetHighLightColor(System::Uitypes::TColor Value);
	void __fastcall SetShadowColor(System::Uitypes::TColor Value);
	void __fastcall SetButtonColor(System::Uitypes::TColor Value);
	void __fastcall SetBorderColor(System::Uitypes::TColor Value);
	void __fastcall SetKind(Vcl::Forms::TScrollBarKind Value);
	void __fastcall SetShowArrows(bool Value);
	void __fastcall SetShowHandleGrip(bool Value);
	void __fastcall SetStyle(TRBStyle Value);
	void __fastcall SetBackgnd(TRBBackgnd Value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TWMNCPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	
protected:
	bool GenChange;
	TRBZone DragZone;
	TRBZone HotZone;
	Vcl::Extctrls::TTimer* Timer;
	int TimerMode;
	int StoredX;
	int StoredY;
	float PosBeforeDrag;
	virtual void __fastcall DoChange();
	virtual void __fastcall DoDrawButton(const System::Types::TRect &R, TRBDirection Direction, bool Pushed, bool Enabled, bool Hot);
	virtual void __fastcall DoDrawHandle(const System::Types::TRect &R, bool Horz, bool Pushed, bool Hot);
	virtual void __fastcall DoDrawTrack(const System::Types::TRect &R, TRBDirection Direction, bool Pushed, bool Enabled, bool Hot);
	virtual bool __fastcall DrawEnabled();
	int __fastcall GetBorderSize();
	virtual System::Types::TRect __fastcall GetHandleRect();
	int __fastcall GetButtonSize();
	System::Types::TRect __fastcall GetTrackBoundary();
	TRBZone __fastcall GetZone(int X, int Y);
	System::Types::TRect __fastcall GetZoneRect(TRBZone Zone);
	virtual void __fastcall MouseLeft();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint();
	void __fastcall StartDragTracking();
	void __fastcall StartHotTracking();
	void __fastcall StopDragTracking();
	void __fastcall StopHotTracking();
	virtual void __fastcall TimerHandler(System::TObject* Sender);
	
public:
	__fastcall virtual TArrowBar(System::Classes::TComponent* AOwner);
	__property Color = {default=-16777216};
	__property TRBBackgnd Backgnd = {read=FBackgnd, write=SetBackgnd, nodefault};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property int ButtonSize = {read=FButtonSize, write=SetButtonSize, default=0};
	__property System::Uitypes::TColor HandleColor = {read=FHandleColor, write=SetHandleColor, default=-16777200};
	__property System::Uitypes::TColor ButtonColor = {read=FButtoncolor, write=SetButtonColor, default=-16777201};
	__property System::Uitypes::TColor HighLightColor = {read=FHighLightColor, write=SetHighLightColor, default=-16777196};
	__property System::Uitypes::TColor ShadowColor = {read=FShadowColor, write=SetShadowColor, default=-16777200};
	__property System::Uitypes::TColor BorderColor = {read=FBorderColor, write=SetBorderColor, default=-16777210};
	__property Vcl::Forms::TScrollBarKind Kind = {read=FKind, write=SetKind, default=0};
	__property bool ShowArrows = {read=FShowArrows, write=SetShowArrows, default=1};
	__property bool ShowHandleGrip = {read=FShowHandleGrip, write=SetShowHandleGrip, nodefault};
	__property TRBStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnUserChange = {read=FOnUserChange, write=FOnUserChange};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TArrowBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TArrowBar(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


typedef System::Word TRBIncrement;

class PASCALIMPLEMENTATION TCustomRangeBar : public TArrowBar
{
	typedef TArrowBar inherited;
	
private:
	bool FCentered;
	int FEffectiveWindow;
	TRBIncrement FIncrement;
	float FPosition;
	int FRange;
	int FWindow;
	bool __fastcall IsPositionStored();
	void __fastcall SetPosition(float Value);
	void __fastcall SetRange(int Value);
	void __fastcall SetWindow(int Value);
	
protected:
	void __fastcall AdjustPosition();
	DYNAMIC bool __fastcall DoMouseWheel(System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos);
	virtual bool __fastcall DrawEnabled();
	virtual System::Types::TRect __fastcall GetHandleRect();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TimerHandler(System::TObject* Sender);
	void __fastcall UpdateEffectiveWindow();
	__property int EffectiveWindow = {read=FEffectiveWindow, nodefault};
	
public:
	__fastcall virtual TCustomRangeBar(System::Classes::TComponent* AOwner);
	DYNAMIC void __fastcall Resize();
	void __fastcall SetParams(int NewRange, int NewWindow);
	__property bool Centered = {read=FCentered, write=FCentered, nodefault};
	__property TRBIncrement Increment = {read=FIncrement, write=FIncrement, default=8};
	__property float Position = {read=FPosition, write=SetPosition, stored=IsPositionStored};
	__property int Range = {read=FRange, write=SetRange, default=0};
	__property int Window = {read=FWindow, write=SetWindow, default=0};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TCustomRangeBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomRangeBar(HWND ParentWindow) : TArrowBar(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TRangeBar : public TCustomRangeBar
{
	typedef TCustomRangeBar inherited;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property Constraints;
	__property Color = {default=-16777216};
	__property Backgnd;
	__property BorderStyle = {default=1};
	__property ButtonSize = {default=0};
	__property Enabled = {default=1};
	__property HandleColor = {default=-16777200};
	__property ButtonColor = {default=-16777201};
	__property HighLightColor = {default=-16777196};
	__property ShadowColor = {default=-16777200};
	__property BorderColor = {default=-16777210};
	__property Increment = {default=8};
	__property Kind = {default=0};
	__property Range = {default=0};
	__property Style = {default=0};
	__property Visible = {default=1};
	__property Window = {default=0};
	__property ShowArrows = {default=1};
	__property ShowHandleGrip;
	__property Position = {default=0};
	__property OnChange;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheelUp;
	__property OnMouseWheelDown;
	__property OnStartDrag;
	__property OnUserChange;
public:
	/* TCustomRangeBar.Create */ inline __fastcall virtual TRangeBar(System::Classes::TComponent* AOwner) : TCustomRangeBar(AOwner) { }
	
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TRangeBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TRangeBar(HWND ParentWindow) : TCustomRangeBar(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TCustomGaugeBar : public TArrowBar
{
	typedef TArrowBar inherited;
	
private:
	int FHandleSize;
	int FLargeChange;
	int FMax;
	int FMin;
	int FPosition;
	int FSmallChange;
	void __fastcall SetHandleSize(int Value);
	void __fastcall SetMax(int Value);
	void __fastcall SetMin(int Value);
	void __fastcall SetPosition(int Value);
	void __fastcall SetLargeChange(int Value);
	void __fastcall SetSmallChange(int Value);
	
protected:
	void __fastcall AdjustPosition();
	DYNAMIC bool __fastcall DoMouseWheel(System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos);
	virtual System::Types::TRect __fastcall GetHandleRect();
	int __fastcall GetHandleSize();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TimerHandler(System::TObject* Sender);
	
public:
	__fastcall virtual TCustomGaugeBar(System::Classes::TComponent* AOwner);
	__property int HandleSize = {read=FHandleSize, write=SetHandleSize, default=0};
	__property int LargeChange = {read=FLargeChange, write=SetLargeChange, default=1};
	__property int Max = {read=FMax, write=SetMax, default=100};
	__property int Min = {read=FMin, write=SetMin, default=0};
	__property int Position = {read=FPosition, write=SetPosition, nodefault};
	__property int SmallChange = {read=FSmallChange, write=SetSmallChange, default=1};
	__property OnChange;
	__property OnUserChange;
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TCustomGaugeBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomGaugeBar(HWND ParentWindow) : TArrowBar(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TGaugeBar : public TCustomGaugeBar
{
	typedef TCustomGaugeBar inherited;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property Constraints;
	__property Color = {default=-16777216};
	__property Backgnd;
	__property BorderStyle = {default=1};
	__property ButtonSize = {default=0};
	__property Enabled = {default=1};
	__property HandleColor = {default=-16777200};
	__property ButtonColor = {default=-16777201};
	__property HighLightColor = {default=-16777196};
	__property ShadowColor = {default=-16777200};
	__property BorderColor = {default=-16777210};
	__property HandleSize = {default=0};
	__property Kind = {default=0};
	__property LargeChange = {default=1};
	__property Max = {default=100};
	__property Min = {default=0};
	__property ShowArrows = {default=1};
	__property ShowHandleGrip;
	__property Style = {default=0};
	__property SmallChange = {default=1};
	__property Visible = {default=1};
	__property Position;
	__property OnChange;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnUserChange;
public:
	/* TCustomGaugeBar.Create */ inline __fastcall virtual TGaugeBar(System::Classes::TComponent* AOwner) : TCustomGaugeBar(AOwner) { }
	
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TGaugeBar() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGaugeBar(HWND ParentWindow) : TCustomGaugeBar(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TArrowBarAccess : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TArrowBar* FMaster;
	TArrowBar* FSlave;
	TRBBackgnd __fastcall GetBackgnd();
	int __fastcall GetButtonSize();
	System::Uitypes::TColor __fastcall GetColor();
	System::Uitypes::TColor __fastcall GetHandleColor();
	System::Uitypes::TColor __fastcall GetHighLightColor();
	System::Uitypes::TColor __fastcall GetButtonColor();
	System::Uitypes::TColor __fastcall GetBorderColor();
	System::Uitypes::TColor __fastcall GetShadowColor();
	bool __fastcall GetShowArrows();
	bool __fastcall GetShowHandleGrip();
	TRBStyle __fastcall GetStyle();
	void __fastcall SetBackgnd(TRBBackgnd Value);
	void __fastcall SetButtonSize(int Value);
	void __fastcall SetColor(System::Uitypes::TColor Value);
	void __fastcall SetHandleColor(System::Uitypes::TColor Value);
	void __fastcall SetShowArrows(bool Value);
	void __fastcall SetShowHandleGrip(bool Value);
	void __fastcall SetStyle(TRBStyle Value);
	void __fastcall SetHighLightColor(System::Uitypes::TColor Value);
	void __fastcall SetShadowColor(System::Uitypes::TColor Value);
	void __fastcall SetButtonColor(System::Uitypes::TColor Value);
	void __fastcall SetBorderColor(System::Uitypes::TColor Value);
	
public:
	__property TArrowBar* Master = {read=FMaster, write=FMaster};
	__property TArrowBar* Slave = {read=FSlave, write=FSlave};
	
__published:
	__property System::Uitypes::TColor Color = {read=GetColor, write=SetColor, default=-16777216};
	__property TRBBackgnd Backgnd = {read=GetBackgnd, write=SetBackgnd, default=0};
	__property int ButtonSize = {read=GetButtonSize, write=SetButtonSize, default=0};
	__property System::Uitypes::TColor HandleColor = {read=GetHandleColor, write=SetHandleColor, default=-16777200};
	__property System::Uitypes::TColor ButtonColor = {read=GetButtonColor, write=SetButtonColor, default=-16777201};
	__property System::Uitypes::TColor HighLightColor = {read=GetHighLightColor, write=SetHighLightColor, default=-16777196};
	__property System::Uitypes::TColor ShadowColor = {read=GetShadowColor, write=SetShadowColor, default=-16777200};
	__property System::Uitypes::TColor BorderColor = {read=GetBorderColor, write=SetBorderColor, default=-16777210};
	__property bool ShowArrows = {read=GetShowArrows, write=SetShowArrows, default=1};
	__property bool ShowHandleGrip = {read=GetShowHandleGrip, write=SetShowHandleGrip, nodefault};
	__property TRBStyle Style = {read=GetStyle, write=SetStyle, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TArrowBarAccess() { }
	
public:
	/* TObject.Create */ inline __fastcall TArrowBarAccess() : System::Classes::TPersistent() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gr32_rangebars */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_RANGEBARS)
using namespace Gr32_rangebars;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_rangebarsHPP
