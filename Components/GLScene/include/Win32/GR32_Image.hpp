// CodeGear C++Builder
// Copyright (c) 1995, 2023 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Image.pas' rev: 36.00 (Windows)

#ifndef Gr32_imageHPP
#define Gr32_imageHPP

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
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>
#include <GR32_Layers.hpp>
#include <GR32_RangeBars.hpp>
#include <GR32_LowLevel.hpp>
#include <GR32_System.hpp>
#include <GR32_Containers.hpp>
#include <GR32_RepaintOpt.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_image
{
//-- forward type declarations -----------------------------------------------
struct TPaintStage;
class DELPHICLASS TPaintStages;
class DELPHICLASS TCustomPaintBox32;
class DELPHICLASS TPaintBox32;
class DELPHICLASS TCustomImage32;
class DELPHICLASS TImage32;
class DELPHICLASS TIVScrollProperties;
class DELPHICLASS TCustomImgView32;
class DELPHICLASS TImgView32;
class DELPHICLASS TBitmap32Item;
class DELPHICLASS TBitmap32Collection;
class DELPHICLASS TBitmap32List;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TPaintStageEvent)(System::TObject* Sender, Gr32::TBitmap32* Buffer, unsigned StageNum);

typedef TPaintStage *PPaintStage;

struct DECLSPEC_DRECORD TPaintStage
{
public:
	bool DsgnTime;
	bool RunTime;
	unsigned Stage;
	unsigned Parameter;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TPaintStages : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TPaintStage> _TPaintStages__1;
	
	
public:
	PPaintStage operator[](int Index) { return this->Items[Index]; }
	
private:
	_TPaintStages__1 FItems;
	PPaintStage __fastcall GetItem(int Index);
	
public:
	__fastcall virtual ~TPaintStages();
	PPaintStage __fastcall Add();
	void __fastcall Clear();
	int __fastcall Count();
	void __fastcall Delete(int Index);
	PPaintStage __fastcall Insert(int Index);
	__property PPaintStage Items[int Index] = {read=GetItem/*, default*/};
public:
	/* TObject.Create */ inline __fastcall TPaintStages() : System::TObject() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TBitmapAlign : unsigned char { baTopLeft, baCenter, baTile, baCustom };

enum DECLSPEC_DENUM TScaleMode : unsigned char { smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled };

enum DECLSPEC_DENUM Gr32_image__2 : unsigned char { pboWantArrowKeys, pboAutoFocus };

typedef System::Set<Gr32_image__2, Gr32_image__2::pboWantArrowKeys, Gr32_image__2::pboAutoFocus> TPaintBoxOptions;

enum DECLSPEC_DENUM TRepaintMode : unsigned char { rmFull, rmDirect, rmOptimizer };

class PASCALIMPLEMENTATION TCustomPaintBox32 : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	Gr32::TBitmap32* FBuffer;
	int FBufferOversize;
	bool FBufferValid;
	TRepaintMode FRepaintMode;
	Gr32_containers::TRectList* FInvalidRects;
	bool FForceFullRepaint;
	Gr32_repaintopt::TCustomRepaintOptimizer* FRepaintOptimizer;
	TPaintBoxOptions FOptions;
	System::Classes::TNotifyEvent FOnGDIOverlay;
	bool FMouseInControl;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	void __fastcall SetBufferOversize(int Value);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMGetDlgCode &Msg);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMInvalidate(Winapi::Messages::TMessage &Message);
	void __fastcall DirectAreaUpdateHandler(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	
protected:
	virtual void __fastcall SetRepaintMode(const TRepaintMode Value);
	virtual bool __fastcall CustomRepaint();
	virtual bool __fastcall InvalidRectsAvailable();
	virtual void __fastcall DoPrepareInvalidRects();
	virtual void __fastcall DoPaintBuffer();
	virtual void __fastcall DoPaintGDIOverlay();
	virtual void __fastcall DoBufferResized(const int OldWidth, const int OldHeight);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseEnter();
	virtual void __fastcall MouseLeave();
	virtual void __fastcall Paint();
	void __fastcall ResetInvalidRects();
	void __fastcall ResizeBuffer();
	__property Gr32_repaintopt::TCustomRepaintOptimizer* RepaintOptimizer = {read=FRepaintOptimizer};
	__property bool BufferValid = {read=FBufferValid, write=FBufferValid, nodefault};
	__property Gr32_containers::TRectList* InvalidRects = {read=FInvalidRects};
	
public:
	__fastcall virtual TCustomPaintBox32(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomPaintBox32();
	virtual Gr32::TRect __fastcall GetViewportRect();
	void __fastcall Flush()/* overload */;
	void __fastcall Flush(const Gr32::TRect &SrcRect)/* overload */;
	virtual void __fastcall Invalidate();
	virtual void __fastcall ForceFullInvalidate();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	__property Gr32::TBitmap32* Buffer = {read=FBuffer};
	__property int BufferOversize = {read=FBufferOversize, write=SetBufferOversize, nodefault};
	__property TPaintBoxOptions Options = {read=FOptions, write=FOptions, default=0};
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	__property TRepaintMode RepaintMode = {read=FRepaintMode, write=SetRepaintMode, default=0};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Classes::TNotifyEvent OnGDIOverlay = {read=FOnGDIOverlay, write=FOnGDIOverlay};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomPaintBox32(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TPaintBox32 : public TCustomPaintBox32
{
	typedef TCustomPaintBox32 inherited;
	
private:
	System::Classes::TNotifyEvent FOnPaintBuffer;
	
protected:
	virtual void __fastcall DoPaintBuffer();
	
public:
	__property Canvas;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property AutoSize = {default=0};
	__property Constraints;
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Options = {default=0};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RepaintMode = {default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property OnCanResize;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnGDIOverlay;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property System::Classes::TNotifyEvent OnPaintBuffer = {read=FOnPaintBuffer, write=FOnPaintBuffer};
	__property OnResize;
	__property OnStartDrag;
public:
	/* TCustomPaintBox32.Create */ inline __fastcall virtual TPaintBox32(System::Classes::TComponent* AOwner) : TCustomPaintBox32(AOwner) { }
	/* TCustomPaintBox32.Destroy */ inline __fastcall virtual ~TPaintBox32() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TPaintBox32(HWND ParentWindow) : TCustomPaintBox32(ParentWindow) { }
	
};


typedef void __fastcall (__closure *TImgMouseEvent)(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer);

typedef void __fastcall (__closure *TImgMouseMoveEvent)(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer);

typedef void __fastcall (__closure *TPaintStageHandler)(Gr32::TBitmap32* Dest, int StageNum);

class PASCALIMPLEMENTATION TCustomImage32 : public TCustomPaintBox32
{
	typedef TCustomPaintBox32 inherited;
	
	
private:
	typedef System::DynamicArray<TPaintStageHandler> _TCustomImage32__1;
	
	typedef System::DynamicArray<int> _TCustomImage32__2;
	
	
private:
	Gr32::TBitmap32* FBitmap;
	TBitmapAlign FBitmapAlign;
	Gr32_layers::TLayerCollection* FLayers;
	Gr32::TFloat FOffsetHorz;
	Gr32::TFloat FOffsetVert;
	TPaintStages* FPaintStages;
	_TCustomImage32__1 FPaintStageHandlers;
	_TCustomImage32__2 FPaintStageNum;
	Gr32::TFloat FScaleX;
	Gr32::TFloat FScaleY;
	TScaleMode FScaleMode;
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnBitmapResize;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnInitStages;
	TImgMouseEvent FOnMouseDown;
	TImgMouseMoveEvent FOnMouseMove;
	TImgMouseEvent FOnMouseUp;
	TPaintStageEvent FOnPaintStage;
	System::Classes::TNotifyEvent FOnScaleChange;
	void __fastcall BitmapResizeHandler(System::TObject* Sender);
	void __fastcall BitmapChangeHandler(System::TObject* Sender);
	void __fastcall BitmapAreaChangeHandler(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	void __fastcall BitmapDirectAreaChangeHandler(System::TObject* Sender, const Gr32::TRect &Area, const unsigned Info);
	void __fastcall LayerCollectionChangeHandler(System::TObject* Sender);
	void __fastcall LayerCollectionGDIUpdateHandler(System::TObject* Sender);
	void __fastcall LayerCollectionGetViewportScaleHandler(System::TObject* Sender, /* out */ Gr32::TFloat &ScaleX, /* out */ Gr32::TFloat &ScaleY);
	void __fastcall LayerCollectionGetViewportShiftHandler(System::TObject* Sender, /* out */ Gr32::TFloat &ShiftX, /* out */ Gr32::TFloat &ShiftY);
	Gr32::TPixelCombineEvent __fastcall GetOnPixelCombine();
	void __fastcall SetBitmap(Gr32::TBitmap32* Value);
	void __fastcall SetBitmapAlign(TBitmapAlign Value);
	void __fastcall SetLayers(Gr32_layers::TLayerCollection* Value);
	void __fastcall SetOffsetHorz(Gr32::TFloat Value);
	void __fastcall SetOffsetVert(Gr32::TFloat Value);
	void __fastcall SetScale(Gr32::TFloat Value);
	void __fastcall SetScaleX(Gr32::TFloat Value);
	void __fastcall SetScaleY(Gr32::TFloat Value);
	void __fastcall SetOnPixelCombine(Gr32::TPixelCombineEvent Value);
	
protected:
	Gr32::TRect CachedBitmapRect;
	Gr32::TFloat CachedShiftX;
	Gr32::TFloat CachedShiftY;
	Gr32::TFloat CachedScaleX;
	Gr32::TFloat CachedScaleY;
	Gr32::TFloat CachedRecScaleX;
	Gr32::TFloat CachedRecScaleY;
	bool CacheValid;
	int OldSzX;
	int OldSzY;
	bool PaintToMode;
	virtual void __fastcall BitmapResized();
	virtual void __fastcall BitmapChanged(const Gr32::TRect &Area);
	virtual bool __fastcall CanAutoSize(int &NewWidth, int &NewHeight);
	virtual void __fastcall DoInitStages();
	virtual void __fastcall DoPaintBuffer();
	virtual void __fastcall DoPaintGDIOverlay();
	virtual void __fastcall DoScaleChange();
	virtual void __fastcall InitDefaultStages();
	void __fastcall InvalidateCache();
	virtual bool __fastcall InvalidRectsAvailable();
	DYNAMIC void __fastcall DblClick();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y)/* overload */;
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y)/* overload */;
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y)/* overload */;
	HIDESBASE virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer)/* overload */;
	HIDESBASE virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer)/* overload */;
	HIDESBASE virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer)/* overload */;
	virtual void __fastcall MouseLeave();
	virtual void __fastcall SetRepaintMode(const TRepaintMode Value);
	virtual void __fastcall SetScaleMode(TScaleMode Value);
	void __fastcall SetXForm(Gr32::TFloat ShiftX, Gr32::TFloat ShiftY, Gr32::TFloat ScaleX, Gr32::TFloat ScaleY);
	virtual void __fastcall UpdateCache();
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	
public:
	__fastcall virtual TCustomImage32(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomImage32();
	virtual void __fastcall BeginUpdate();
	Gr32::TPoint __fastcall BitmapToControl(const Gr32::TPoint &APoint)/* overload */;
	Gr32::TFloatPoint __fastcall BitmapToControl(const Gr32::TFloatPoint &APoint)/* overload */;
	HIDESBASE virtual void __fastcall Changed();
	HIDESBASE virtual void __fastcall Update(const Gr32::TRect &Rect)/* overload */;
	Gr32::TPoint __fastcall ControlToBitmap(const Gr32::TPoint &APoint)/* overload */;
	Gr32::TFloatPoint __fastcall ControlToBitmap(const Gr32::TFloatPoint &APoint)/* overload */;
	virtual void __fastcall EndUpdate();
	virtual void __fastcall ExecBitmapFrame(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecClearBuffer(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecClearBackgnd(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecControlFrame(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecCustom(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecDrawBitmap(Gr32::TBitmap32* Dest, int StageNum);
	virtual void __fastcall ExecDrawLayers(Gr32::TBitmap32* Dest, int StageNum);
	virtual Gr32::TRect __fastcall GetBitmapRect();
	virtual System::Types::TSize __fastcall GetBitmapSize();
	virtual void __fastcall Invalidate();
	virtual void __fastcall Loaded();
	HIDESBASE virtual void __fastcall PaintTo(Gr32::TBitmap32* Dest, const Gr32::TRect &DestRect);
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall SetupBitmap(bool DoClear = false, Gr32::TColor32 ClearColor = (Gr32::TColor32)(0xff000000));
	__property Gr32::TBitmap32* Bitmap = {read=FBitmap, write=SetBitmap};
	__property TBitmapAlign BitmapAlign = {read=FBitmapAlign, write=SetBitmapAlign, nodefault};
	__property Canvas;
	__property Gr32_layers::TLayerCollection* Layers = {read=FLayers, write=SetLayers};
	__property Gr32::TFloat OffsetHorz = {read=FOffsetHorz, write=SetOffsetHorz};
	__property Gr32::TFloat OffsetVert = {read=FOffsetVert, write=SetOffsetVert};
	__property TPaintStages* PaintStages = {read=FPaintStages};
	__property Gr32::TFloat Scale = {read=FScaleX, write=SetScale};
	__property Gr32::TFloat ScaleX = {read=FScaleX, write=SetScaleX};
	__property Gr32::TFloat ScaleY = {read=FScaleY, write=SetScaleY};
	__property TScaleMode ScaleMode = {read=FScaleMode, write=SetScaleMode, nodefault};
	__property System::Classes::TNotifyEvent OnBitmapResize = {read=FOnBitmapResize, write=FOnBitmapResize};
	__property Gr32::TPixelCombineEvent OnBitmapPixelCombine = {read=GetOnPixelCombine, write=SetOnPixelCombine};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnInitStages = {read=FOnInitStages, write=FOnInitStages};
	__property TImgMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property TImgMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property TImgMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
	__property TPaintStageEvent OnPaintStage = {read=FOnPaintStage, write=FOnPaintStage};
	__property System::Classes::TNotifyEvent OnScaleChange = {read=FOnScaleChange, write=FOnScaleChange};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomImage32(HWND ParentWindow) : TCustomPaintBox32(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TImage32 : public TCustomImage32
{
	typedef TCustomImage32 inherited;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property AutoSize = {default=0};
	__property Bitmap;
	__property BitmapAlign;
	__property Color = {default=-16777211};
	__property Constraints;
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property ParentColor = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RepaintMode = {default=0};
	__property Scale = {default=0};
	__property ScaleMode;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property OnBitmapResize;
	__property OnCanResize;
	__property OnClick;
	__property OnChange;
	__property OnDblClick;
	__property OnGDIOverlay;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnInitStages;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnPaintStage;
	__property OnResize;
	__property OnStartDrag;
public:
	/* TCustomImage32.Create */ inline __fastcall virtual TImage32(System::Classes::TComponent* AOwner) : TCustomImage32(AOwner) { }
	/* TCustomImage32.Destroy */ inline __fastcall virtual ~TImage32() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TImage32(HWND ParentWindow) : TCustomImage32(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TScrollBarVisibility : unsigned char { svAlways, svHidden, svAuto };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TIVScrollProperties : public Gr32_rangebars::TArrowBarAccess
{
	typedef Gr32_rangebars::TArrowBarAccess inherited;
	
private:
	int __fastcall GetIncrement();
	int __fastcall GetSize();
	TScrollBarVisibility __fastcall GetVisibility();
	void __fastcall SetIncrement(int Value);
	void __fastcall SetSize(int Value);
	void __fastcall SetVisibility(const TScrollBarVisibility Value);
	
protected:
	TCustomImgView32* ImgView;
	
__published:
	__property int Increment = {read=GetIncrement, write=SetIncrement, default=8};
	__property int Size = {read=GetSize, write=SetSize, default=0};
	__property TScrollBarVisibility Visibility = {read=GetVisibility, write=SetVisibility, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TIVScrollProperties() { }
	
public:
	/* TObject.Create */ inline __fastcall TIVScrollProperties() : Gr32_rangebars::TArrowBarAccess() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TSizeGripStyle : unsigned char { sgAuto, sgNone, sgAlways };

class PASCALIMPLEMENTATION TCustomImgView32 : public TCustomImage32
{
	typedef TCustomImage32 inherited;
	
private:
	bool FCentered;
	int FScrollBarSize;
	TScrollBarVisibility FScrollBarVisibility;
	TIVScrollProperties* FScrollBars;
	TSizeGripStyle FSizeGrip;
	System::Classes::TNotifyEvent FOnScroll;
	int FOverSize;
	void __fastcall SetCentered(bool Value);
	void __fastcall SetScrollBars(TIVScrollProperties* Value);
	void __fastcall SetSizeGrip(TSizeGripStyle Value);
	void __fastcall SetOverSize(const int Value);
	
protected:
	bool DisableScrollUpdate;
	Gr32_rangebars::TCustomRangeBar* HScroll;
	Gr32_rangebars::TCustomRangeBar* VScroll;
	void __fastcall AlignAll();
	virtual void __fastcall BitmapResized();
	void __fastcall DoDrawSizeGrip(const Gr32::TRect &R);
	virtual void __fastcall DoScaleChange();
	virtual void __fastcall DoScroll();
	bool __fastcall GetScrollBarsVisible();
	int __fastcall GetScrollBarSize();
	Gr32::TRect __fastcall GetSizeGripRect();
	bool __fastcall IsSizeGripVisible();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y)/* overload */;
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y)/* overload */;
	virtual void __fastcall Paint();
	void __fastcall Recenter();
	virtual void __fastcall SetScaleMode(TScaleMode Value);
	virtual void __fastcall ScrollHandler(System::TObject* Sender);
	virtual void __fastcall UpdateImage();
	virtual void __fastcall UpdateScrollBars();
	
public:
	__fastcall virtual TCustomImgView32(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomImgView32();
	virtual Gr32::TRect __fastcall GetViewportRect();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Resize();
	void __fastcall ScrollToCenter(int X, int Y);
	void __fastcall Scroll(int Dx, int Dy);
	__property bool Centered = {read=FCentered, write=SetCentered, default=1};
	__property TIVScrollProperties* ScrollBars = {read=FScrollBars, write=SetScrollBars};
	__property TSizeGripStyle SizeGrip = {read=FSizeGrip, write=SetSizeGrip, default=0};
	__property int OverSize = {read=FOverSize, write=SetOverSize, nodefault};
	__property System::Classes::TNotifyEvent OnScroll = {read=FOnScroll, write=FOnScroll};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomImgView32(HWND ParentWindow) : TCustomImage32(ParentWindow) { }
	
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer){ TCustomImage32::MouseDown(Button, Shift, X, Y, Layer); }
	inline void __fastcall  MouseMove(System::Classes::TShiftState Shift, int X, int Y, Gr32_layers::TCustomLayer* Layer){ TCustomImage32::MouseMove(Shift, X, Y, Layer); }
	
};


class PASCALIMPLEMENTATION TImgView32 : public TCustomImgView32
{
	typedef TCustomImgView32 inherited;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property AutoSize = {default=0};
	__property Bitmap;
	__property BitmapAlign;
	__property Centered = {default=1};
	__property Color = {default=-16777211};
	__property Constraints;
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property ParentColor = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RepaintMode = {default=0};
	__property Scale = {default=0};
	__property ScaleMode;
	__property ScrollBars;
	__property ShowHint;
	__property SizeGrip = {default=0};
	__property OverSize;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property OnBitmapResize;
	__property OnCanResize;
	__property OnClick;
	__property OnChange;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnGDIOverlay;
	__property OnInitStages;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnPaintStage;
	__property OnResize;
	__property OnScroll;
	__property OnStartDrag;
public:
	/* TCustomImgView32.Create */ inline __fastcall virtual TImgView32(System::Classes::TComponent* AOwner) : TCustomImgView32(AOwner) { }
	/* TCustomImgView32.Destroy */ inline __fastcall virtual ~TImgView32() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TImgView32(HWND ParentWindow) : TCustomImgView32(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TBitmap32Item : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gr32::TBitmap32* FBitmap;
	void __fastcall SetBitmap(Gr32::TBitmap32* ABitmap);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TBitmap32Item(System::Classes::TCollection* Collection);
	__fastcall virtual ~TBitmap32Item();
	
__published:
	__property Gr32::TBitmap32* Bitmap = {read=FBitmap, write=SetBitmap};
};

#pragma pack(pop)

typedef System::TMetaClass* TBitmap32ItemClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBitmap32Collection : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TBitmap32Item* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	HIDESBASE TBitmap32Item* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TBitmap32Item* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TBitmap32Collection(System::Classes::TPersistent* AOwner, TBitmap32ItemClass ItemClass);
	HIDESBASE TBitmap32Item* __fastcall Add();
	__property TBitmap32Item* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TBitmap32Collection() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TBitmap32List : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	Gr32::TBitmap32* operator[](int Index) { return this->Bitmap[Index]; }
	
private:
	TBitmap32Collection* FBitmap32Collection;
	void __fastcall SetBitmap(int Index, Gr32::TBitmap32* Value);
	Gr32::TBitmap32* __fastcall GetBitmap(int Index);
	void __fastcall SetBitmap32Collection(TBitmap32Collection* Value);
	
public:
	__fastcall virtual TBitmap32List(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TBitmap32List();
	__property Gr32::TBitmap32* Bitmap[int Index] = {read=GetBitmap, write=SetBitmap/*, default*/};
	
__published:
	__property TBitmap32Collection* Bitmaps = {read=FBitmap32Collection, write=SetBitmap32Collection};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 PST_CUSTOM = System::Int8(0x1);
static const System::Int8 PST_CLEAR_BUFFER = System::Int8(0x2);
static const System::Int8 PST_CLEAR_BACKGND = System::Int8(0x3);
static const System::Int8 PST_DRAW_BITMAP = System::Int8(0x4);
static const System::Int8 PST_DRAW_LAYERS = System::Int8(0x5);
static const System::Int8 PST_CONTROL_FRAME = System::Int8(0x6);
static const System::Int8 PST_BITMAP_FRAME = System::Int8(0x7);
}	/* namespace Gr32_image */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_IMAGE)
using namespace Gr32_image;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_imageHPP
