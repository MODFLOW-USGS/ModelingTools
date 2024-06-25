// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Layers.pas' rev: 36.00 (Windows)

#ifndef Gr32_layersHPP
#define Gr32_layersHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GR32.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_layers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLayerCollection;
class DELPHICLASS TCustomLayer;
class DELPHICLASS TPositionedLayer;
class DELPHICLASS TBitmapLayer;
class DELPHICLASS TRubberbandLayer;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TLayerClass;

typedef void __fastcall (__closure *TLayerUpdateEvent)(System::TObject* Sender, TCustomLayer* Layer);

typedef Gr32::TAreaChangedEvent TAreaUpdateEvent;

enum DECLSPEC_DENUM TLayerListNotification : unsigned char { lnLayerAdded, lnLayerInserted, lnLayerDeleted, lnCleared };

typedef void __fastcall (__closure *TLayerListNotifyEvent)(TLayerCollection* Sender, TLayerListNotification Action, TCustomLayer* Layer, int Index);

typedef void __fastcall (__closure *TGetScaleEvent)(System::TObject* Sender, /* out */ Gr32::TFloat &ScaleX, /* out */ Gr32::TFloat &ScaleY);

typedef void __fastcall (__closure *TGetShiftEvent)(System::TObject* Sender, /* out */ Gr32::TFloat &ShiftX, /* out */ Gr32::TFloat &ShiftY);

class PASCALIMPLEMENTATION TLayerCollection : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TCustomLayer* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TList* FItems;
	bool FMouseEvents;
	TCustomLayer* FMouseListener;
	int FUpdateCount;
	System::Classes::TPersistent* FOwner;
	System::Classes::TNotifyEvent FOnChanging;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnGDIUpdate;
	TLayerListNotifyEvent FOnListNotify;
	TLayerUpdateEvent FOnLayerUpdated;
	TAreaUpdateEvent FOnAreaUpdated;
	TGetScaleEvent FOnGetViewportScale;
	TGetShiftEvent FOnGetViewportShift;
	int __fastcall GetCount();
	void __fastcall InsertItem(TCustomLayer* Item);
	void __fastcall RemoveItem(TCustomLayer* Item);
	void __fastcall SetMouseEvents(bool Value);
	void __fastcall SetMouseListener(TCustomLayer* Value);
	
protected:
	void __fastcall BeginUpdate();
	void __fastcall Changed();
	void __fastcall Changing();
	void __fastcall EndUpdate();
	TCustomLayer* __fastcall FindLayerAtPos(int X, int Y, unsigned OptionsMask);
	TCustomLayer* __fastcall GetItem(int Index);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall GDIUpdate();
	void __fastcall DoUpdateLayer(TCustomLayer* Layer);
	void __fastcall DoUpdateArea(const Gr32::TRect &Rect);
	void __fastcall Notify(TLayerListNotification Action, TCustomLayer* Layer, int Index);
	void __fastcall SetItem(int Index, TCustomLayer* Value);
	TCustomLayer* __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	TCustomLayer* __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	TCustomLayer* __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TLayerListNotifyEvent OnListNotify = {read=FOnListNotify, write=FOnListNotify};
	__property System::Classes::TNotifyEvent OnGDIUpdate = {read=FOnGDIUpdate, write=FOnGDIUpdate};
	__property TLayerUpdateEvent OnLayerUpdated = {read=FOnLayerUpdated, write=FOnLayerUpdated};
	__property TAreaUpdateEvent OnAreaUpdated = {read=FOnAreaUpdated, write=FOnAreaUpdated};
	__property TGetScaleEvent OnGetViewportScale = {read=FOnGetViewportScale, write=FOnGetViewportScale};
	__property TGetShiftEvent OnGetViewportShift = {read=FOnGetViewportShift, write=FOnGetViewportShift};
	
public:
	__fastcall TLayerCollection(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TLayerCollection();
	TCustomLayer* __fastcall Add(TLayerClass ItemClass);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear();
	void __fastcall Delete(int Index);
	TCustomLayer* __fastcall Insert(int Index, TLayerClass ItemClass);
	Gr32::TFloatPoint __fastcall LocalToViewport(const Gr32::TFloatPoint &APoint, bool AScaled);
	Gr32::TFloatPoint __fastcall ViewportToLocal(const Gr32::TFloatPoint &APoint, bool AScaled);
	virtual void __fastcall GetViewportScale(/* out */ Gr32::TFloat &ScaleX, /* out */ Gr32::TFloat &ScaleY);
	virtual void __fastcall GetViewportShift(/* out */ Gr32::TFloat &ShiftX, /* out */ Gr32::TFloat &ShiftY);
	__property int Count = {read=GetCount, nodefault};
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	__property TCustomLayer* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TCustomLayer* MouseListener = {read=FMouseListener, write=SetMouseListener};
	__property bool MouseEvents = {read=FMouseEvents, write=SetMouseEvents, nodefault};
};


enum DECLSPEC_DENUM TLayerState : unsigned char { lsMouseLeft, lsMouseRight, lsMouseMiddle };

typedef System::Set<TLayerState, TLayerState::lsMouseLeft, TLayerState::lsMouseMiddle> TLayerStates;

typedef void __fastcall (__closure *TPaintLayerEvent)(System::TObject* Sender, Gr32::TBitmap32* Buffer);

typedef void __fastcall (__closure *THitTestEvent)(System::TObject* Sender, int X, int Y, bool &Passed);

class PASCALIMPLEMENTATION TCustomLayer : public Gr32::TNotifiablePersistent
{
	typedef Gr32::TNotifiablePersistent inherited;
	
private:
	System::Uitypes::TCursor FCursor;
	System::Classes::TList* FFreeNotifies;
	TLayerCollection* FLayerCollection;
	TLayerStates FLayerStates;
	unsigned FLayerOptions;
	THitTestEvent FOnHitTest;
	Vcl::Controls::TMouseEvent FOnMouseDown;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	Vcl::Controls::TMouseEvent FOnMouseUp;
	TPaintLayerEvent FOnPaint;
	int FTag;
	System::Classes::TNotifyEvent FOnDestroy;
	int __fastcall GetIndex();
	bool __fastcall GetMouseEvents();
	bool __fastcall GetVisible();
	void __fastcall SetMouseEvents(bool Value);
	void __fastcall SetVisible(bool Value);
	bool __fastcall GetInvalid();
	void __fastcall SetInvalid(bool Value);
	bool __fastcall GetForceUpdate();
	void __fastcall SetForceUpdate(bool Value);
	
protected:
	void __fastcall AddNotification(TCustomLayer* ALayer);
	void __fastcall Changing();
	virtual bool __fastcall DoHitTest(int X, int Y);
	void __fastcall DoPaint(Gr32::TBitmap32* Buffer);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(TCustomLayer* ALayer);
	virtual void __fastcall Paint(Gr32::TBitmap32* Buffer);
	virtual void __fastcall PaintGDI(Vcl::Graphics::TCanvas* Canvas);
	void __fastcall RemoveNotification(TCustomLayer* ALayer);
	virtual void __fastcall SetIndex(int Value);
	virtual void __fastcall SetCursor(System::Uitypes::TCursor Value);
	virtual void __fastcall SetLayerCollection(TLayerCollection* Value);
	virtual void __fastcall SetLayerOptions(unsigned Value);
	__property bool Invalid = {read=GetInvalid, write=SetInvalid, nodefault};
	__property bool ForceUpdate = {read=GetForceUpdate, write=SetForceUpdate, nodefault};
	
public:
	__fastcall virtual TCustomLayer(TLayerCollection* ALayerCollection);
	__fastcall virtual ~TCustomLayer();
	virtual void __fastcall BeforeDestruction();
	void __fastcall BringToFront();
	virtual void __fastcall Changed()/* overload */;
	HIDESBASE void __fastcall Changed(const Gr32::TRect &Rect)/* overload */;
	void __fastcall Update()/* overload */;
	void __fastcall Update(const Gr32::TRect &Rect)/* overload */;
	bool __fastcall HitTest(int X, int Y);
	void __fastcall SendToBack();
	void __fastcall SetAsMouseListener();
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property int Index = {read=GetIndex, write=SetIndex, nodefault};
	__property TLayerCollection* LayerCollection = {read=FLayerCollection, write=SetLayerCollection};
	__property unsigned LayerOptions = {read=FLayerOptions, write=SetLayerOptions, nodefault};
	__property TLayerStates LayerStates = {read=FLayerStates, nodefault};
	__property bool MouseEvents = {read=GetMouseEvents, write=SetMouseEvents, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property System::Classes::TNotifyEvent OnDestroy = {read=FOnDestroy, write=FOnDestroy};
	__property THitTestEvent OnHitTest = {read=FOnHitTest, write=FOnHitTest};
	__property TPaintLayerEvent OnPaint = {read=FOnPaint, write=FOnPaint};
	__property Vcl::Controls::TMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property Vcl::Controls::TMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
};


class PASCALIMPLEMENTATION TPositionedLayer : public TCustomLayer
{
	typedef TCustomLayer inherited;
	
private:
	Gr32::TFloatRect FLocation;
	bool FScaled;
	void __fastcall SetLocation(const Gr32::TFloatRect &Value);
	void __fastcall SetScaled(bool Value);
	
protected:
	virtual bool __fastcall DoHitTest(int X, int Y);
	virtual void __fastcall DoSetLocation(const Gr32::TFloatRect &NewLocation);
	
public:
	__fastcall virtual TPositionedLayer(TLayerCollection* ALayerCollection);
	virtual Gr32::TFloatRect __fastcall GetAdjustedRect(const Gr32::TFloatRect &R);
	Gr32::TFloatRect __fastcall GetAdjustedLocation();
	__property Gr32::TFloatRect Location = {read=FLocation, write=SetLocation};
	__property bool Scaled = {read=FScaled, write=SetScaled, nodefault};
public:
	/* TCustomLayer.Destroy */ inline __fastcall virtual ~TPositionedLayer() { }
	
};


class PASCALIMPLEMENTATION TBitmapLayer : public TPositionedLayer
{
	typedef TPositionedLayer inherited;
	
private:
	Gr32::TBitmap32* FBitmap;
	bool FAlphaHit;
	bool FCropped;
	void __fastcall BitmapAreaChanged(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	void __fastcall SetBitmap(Gr32::TBitmap32* Value);
	void __fastcall SetCropped(bool Value);
	
protected:
	virtual bool __fastcall DoHitTest(int X, int Y);
	virtual void __fastcall Paint(Gr32::TBitmap32* Buffer);
	
public:
	__fastcall virtual TBitmapLayer(TLayerCollection* ALayerCollection);
	__fastcall virtual ~TBitmapLayer();
	__property bool AlphaHit = {read=FAlphaHit, write=FAlphaHit, nodefault};
	__property Gr32::TBitmap32* Bitmap = {read=FBitmap, write=SetBitmap};
	__property bool Cropped = {read=FCropped, write=SetCropped, nodefault};
};


enum DECLSPEC_DENUM TDragState : unsigned char { dsNone, dsMove, dsSizeL, dsSizeT, dsSizeR, dsSizeB, dsSizeTL, dsSizeTR, dsSizeBL, dsSizeBR };

enum DECLSPEC_DENUM Gr32_layers__5 : unsigned char { rhCenter, rhSides, rhCorners, rhFrame, rhNotLeftSide, rhNotRightSide, rhNotTopSide, rhNotBottomSide, rhNotTLCorner, rhNotTRCorner, rhNotBLCorner, rhNotBRCorner };

typedef System::Set<Gr32_layers__5, Gr32_layers__5::rhCenter, Gr32_layers__5::rhNotBRCorner> TRBHandles;

enum DECLSPEC_DENUM Gr32_layers__6 : unsigned char { roProportional, roConstrained };

typedef System::Set<Gr32_layers__6, Gr32_layers__6::roProportional, Gr32_layers__6::roConstrained> TRBOptions;

typedef void __fastcall (__closure *TRBResizingEvent)(System::TObject* Sender, const Gr32::TFloatRect &OldLocation, Gr32::TFloatRect &NewLocation, TDragState DragState, System::Classes::TShiftState Shift);

typedef TRBResizingEvent TRBConstrainEvent;

class PASCALIMPLEMENTATION TRubberbandLayer : public TPositionedLayer
{
	typedef TPositionedLayer inherited;
	
private:
	TPositionedLayer* FChildLayer;
	Gr32::TArrayOfColor32 FFrameStipplePattern;
	Gr32::TFloat FFrameStippleStep;
	Gr32::TFloat FFrameStippleCounter;
	Gr32::TColor32 FHandleFrame;
	Gr32::TColor32 FHandleFill;
	TRBHandles FHandles;
	int FHandleSize;
	Gr32::TFloat FMinWidth;
	Gr32::TFloat FMaxHeight;
	Gr32::TFloat FMinHeight;
	Gr32::TFloat FMaxWidth;
	System::Classes::TNotifyEvent FOnUserChange;
	TRBResizingEvent FOnResizing;
	TRBConstrainEvent FOnConstrain;
	TRBOptions FOptions;
	void __fastcall SetFrameStippleStep(const Gr32::TFloat Value);
	void __fastcall SetFrameStippleCounter(const Gr32::TFloat Value);
	void __fastcall SetChildLayer(TPositionedLayer* Value);
	void __fastcall SetHandleFill(Gr32::TColor32 Value);
	void __fastcall SetHandleFrame(Gr32::TColor32 Value);
	void __fastcall SetHandles(TRBHandles Value);
	void __fastcall SetHandleSize(int Value);
	void __fastcall SetOptions(const TRBOptions Value);
	
protected:
	bool IsDragging;
	TDragState DragState;
	Gr32::TFloatRect OldLocation;
	Gr32::TFloatPoint MouseShift;
	virtual bool __fastcall DoHitTest(int X, int Y);
	virtual void __fastcall DoResizing(Gr32::TFloatRect &OldLocation, Gr32::TFloatRect &NewLocation, TDragState DragState, System::Classes::TShiftState Shift);
	virtual void __fastcall DoConstrain(Gr32::TFloatRect &OldLocation, Gr32::TFloatRect &NewLocation, TDragState DragState, System::Classes::TShiftState Shift);
	virtual void __fastcall DoSetLocation(const Gr32::TFloatRect &NewLocation);
	virtual TDragState __fastcall GetDragState(int X, int Y);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(TCustomLayer* ALayer);
	virtual void __fastcall Paint(Gr32::TBitmap32* Buffer);
	virtual void __fastcall SetLayerOptions(unsigned Value);
	void __fastcall UpdateChildLayer();
	
public:
	__fastcall virtual TRubberbandLayer(TLayerCollection* ALayerCollection);
	void __fastcall SetFrameStipple(const Gr32::TColor32 *Value, const System::NativeInt Value_High);
	__property TPositionedLayer* ChildLayer = {read=FChildLayer, write=SetChildLayer};
	__property TRBOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property TRBHandles Handles = {read=FHandles, write=SetHandles, nodefault};
	__property int HandleSize = {read=FHandleSize, write=SetHandleSize, nodefault};
	__property Gr32::TColor32 HandleFill = {read=FHandleFill, write=SetHandleFill, nodefault};
	__property Gr32::TColor32 HandleFrame = {read=FHandleFrame, write=SetHandleFrame, nodefault};
	__property Gr32::TFloat FrameStippleStep = {read=FFrameStippleStep, write=SetFrameStippleStep};
	__property Gr32::TFloat FrameStippleCounter = {read=FFrameStippleCounter, write=SetFrameStippleCounter};
	__property Gr32::TFloat MaxHeight = {read=FMaxHeight, write=FMaxHeight};
	__property Gr32::TFloat MaxWidth = {read=FMaxWidth, write=FMaxWidth};
	__property Gr32::TFloat MinHeight = {read=FMinHeight, write=FMinHeight};
	__property Gr32::TFloat MinWidth = {read=FMinWidth, write=FMinWidth};
	__property System::Classes::TNotifyEvent OnUserChange = {read=FOnUserChange, write=FOnUserChange};
	__property TRBConstrainEvent OnConstrain = {read=FOnConstrain, write=FOnConstrain};
	__property TRBResizingEvent OnResizing = {read=FOnResizing, write=FOnResizing};
public:
	/* TCustomLayer.Destroy */ inline __fastcall virtual ~TRubberbandLayer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const unsigned LOB_VISIBLE = unsigned(0x80000000);
static const int LOB_GDI_OVERLAY = int(0x40000000);
static const int LOB_MOUSE_EVENTS = int(0x20000000);
static const int LOB_NO_UPDATE = int(0x10000000);
static const int LOB_NO_CAPTURE = int(0x8000000);
static const int LOB_INVALID = int(0x4000000);
static const int LOB_FORCE_UPDATE = int(0x2000000);
static const int LOB_RESERVED_24 = int(0x1000000);
static const unsigned LOB_RESERVED_MASK = unsigned(0xff000000);
}	/* namespace Gr32_layers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_LAYERS)
using namespace Gr32_layers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_layersHPP
