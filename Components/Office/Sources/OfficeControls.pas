
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeControls;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CommCtrl, Menus, Buttons, CheckLst, OfficeTypes, OfficeUtils;

type

{ TksoOfficeLabel }

  TksoOfficeLabel = class(TLabel)
  private
    FMouseInControl: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;

{ TksoOfficeMemo }

  TksoOfficeMemo = class(TCustomMemo)
  private
    FMouseInControl: Boolean;
    FFlat: boolean;
    FBorderStyle: TksoBorderStyle;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure NewAdjustHeight;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);

    procedure DrawBorder;
    procedure InvalidateFrame;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Align;
    property Alignment;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TksoOfficeListBox }

  TksoOfficeListBox = class(TCustomListBox)
  private
    FMouseInControl: Boolean;
    FFlat: boolean;
    FBorderStyle: TksoBorderStyle;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure NewAdjustHeight;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);

    procedure DrawBorder;
    procedure InvalidateFrame;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Align;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TksoOfficeCheckListBox }

  TksoOfficeCheckListBox = class(TCheckListBox)
  private
    FMouseInControl: Boolean;
    FFlat: boolean;
    FBorderStyle: TksoBorderStyle;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure NewAdjustHeight;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);

    procedure DrawBorder;
    procedure InvalidateFrame;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Align;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TksoOfficeCheckBox }

  TksoOfficeCheckBox = class(TCustomControl)
  private
    FState: TCheckBoxState;
    FMouseInControl: Boolean;
    FFocused: Boolean;
    FFlat: boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

    procedure Toggle;
    procedure SetFlat(const Value: boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer);
      override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure SetState(Value:TCheckBoxState);
    procedure SetChecked(Value:Boolean);
    function  GetChecked:Boolean;
    function  GetCaption: TCaption;
    procedure SetCaption(const Value:TCaption);
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Checked: Boolean read GetChecked write SetChecked;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: boolean read FFlat write SetFlat;
    property Font;
    property Color;
    property ParentFont;
    property PopupMenu;
    Property Visible;
    property ShowHint;
    property State: TCheckBoxState read FState write SetState;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TksoOfficeRadioButton }

 TksoOfficeRadioButton = class(TCustomControl)
  private
    FChecked: Boolean;
    FMouseInControl: Boolean;
    FFocused: Boolean;
    FCheckColor: TColor;
    FGroupIndex: Byte;
    FFlat: boolean;
    procedure TurnSiblingsOff;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure SetFlat(const Value: boolean);
  protected
  public
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer);
      override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    function  GetCaption: TCaption;
    procedure SetCaption(const Value:TCaption);
    procedure SetChecked(Value:Boolean);
    procedure SetCheckColor(Value:TColor);
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;//:TCaption read GetCaption write SetCaption;
    property Checked:Boolean read FChecked write SetChecked;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: boolean read FFlat write SetFlat;
    property Font;
    property Visible;
    property GroupIndex: Byte read FGroupIndex write FGroupIndex;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TksoOfficeGroupBox }

  TksoOfficeGroupBox = class(TCustomControl)
  private
    FMouseInControl: Boolean;
    FFlat: boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure SetFlat(const Value: boolean);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: boolean read FFlat write SetFlat;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation {==============================================================}

{$R *.res }

const
  BtnWidth = 13;

var
  Btns: TBitmap;

procedure ConvertBtns;
var
  i, j: integer;
begin
  Btns.Handle := LoadBitmap(HInstance, 'KSO_BTNS');
  for i := 0 to Btns.Width-1 do
    for j := 0 to Btns.Height-1 do
    begin
      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clYellow) then
        Btns.Canvas.Pixels[i, j] := clWindow;
      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clLime) then
        Btns.Canvas.Pixels[i, j] := clBtnShadow;
      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clSilver) then
        Btns.Canvas.Pixels[i, j] := clBtnFace;
      if Btns.Canvas.Pixels[i, j] = TColor(RGB(198,195,198)) then // silver
        Btns.Canvas.Pixels[i, j] := clBtnFace;
      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clBtnFace) then // silver
        Btns.Canvas.Pixels[i, j] := clBtnFace;
{      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clWhite) then
        Btns.Canvas.Pixels[i, j] := clBtnHighlight;}
      if Btns.Canvas.Pixels[i, j] = ColorToRGB(clGray) then
        Btns.Canvas.Pixels[i, j] := clBtnShadow;
    end;
end;

{ TksoOfficeLabel }

Constructor TksoOfficeLabel.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  FMouseInControl := false;
end;

procedure TksoOfficeLabel.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  Repaint;
end;

procedure TksoOfficeLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  Repaint;
end;

procedure TksoOfficeLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: array[ 0..255 ] of Char;
begin
  GetTextBuf(Text, SizeOf(Text));
  if (Flags and DT_CALCRECT <> 0) and
     ((Text[0] = #0) or ShowAccelChar and (Text[0] = '&') and
      (Text[1] = #0)) then
      StrCopy(Text, ' ');
  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Canvas.Font.Color := Font.Color;
  if FMouseInControl then
    Canvas.Font.Color := clHighlight;
  if not Enabled then
    Canvas.Font.Color := clGrayText;
  DrawText(Canvas.Handle, Text, StrLen(Text), Rect, Flags);
end;

procedure TksoOfficeLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Dx: Integer;
  Rect: TRect;
begin
  If FMouseInControl and Enabled Then
  begin
    if not Transparent then
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(ClientRect);
    end;
    Canvas.Brush.Style := bsClear;
    Rect := ClientRect;
    DoDrawText( Rect, ( DT_EXPANDTABS or DT_WORDBREAK ) or
                Alignments[ Alignment ] );
  end
  else
    with Canvas do
    begin
      If not Transparent Then
      begin
        Brush.Color := Self.Color;
        Pen.Color := Self.Color;
        Rectangle(0,0,Width,Height);
      end;
      If Caption <> '' then
      begin
        Font := Self.Font;
        Dx := TextWidth(Caption);
        case Self.Alignment of
          taCenter: TextOut((Width-Dx) Div 2,0,Caption);
          taLeftJustify: TextOut(0,0,Caption);
          taRightJustify: TextOut(Width-Dx,0,Caption);
        end;
      end;
    end;
end;

{ TksoOfficeMemo }

constructor TksoOfficeMemo.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  AutoSize := false;
  Height := 40;
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
  FMouseInControl := false;
end;

procedure TksoOfficeMemo.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeMemo.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeMemo.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics (DC, Metrics);
  SelectObject (DC, SaveFont);
  ReleaseDC (0, DC);
end;

procedure TksoOfficeMemo.Loaded;
begin
  inherited Loaded;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TksoOfficeMemo.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TksoOfficeMemo.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TksoOfficeMemo.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.WMNCPaint (var Message: TMessage);
begin
  inherited;
  DrawBorder;
  Message.Result := 0;
end;

procedure TksoOfficeMemo.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.SetBorderStyleFlat(const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeMemo.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

procedure TksoOfficeMemo.DrawBorder;
var
  R : TRect;
  Canvas: TCanvas;
begin
  R := Rect(0, 0, Width, Height);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(Handle);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Color;
    Canvas.Brush.Style := bsClear;
    { Draw background }
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Rectangle(1, 1, Width-1, Height-1);
    { Draw border }
    if FFlat then
      if Focused or FMouseInControl then
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFocused)
      else
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFlat)
    else
      OfficeUtils.DrawBorder(Canvas, R, FBorderStyle);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

{ TksoOfficeCheckBox }

constructor TksoOfficeCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 98;
  Height := 20;
  ParentColor := False;
  FFlat := false;
  FMouseInControl := false;
  Color := clBtnFace;
end;

procedure TksoOfficeCheckBox.Toggle;
begin
  case State of
    cbUnchecked, cbGrayed: State := cbChecked;
    cbChecked: State := cbUnchecked;
  end;
end;

procedure TksoOfficeCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
    end else
      inherited;
end;

procedure TksoOfficeCheckBox.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  if FFlat then Paint;
end;

procedure TksoOfficeCheckBox.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  if FFlat then Paint;
end;

procedure TksoOfficeCheckBox.Paint;
var
  R, Dest, Sour: TRect;
  TextTop, TextLeft: integer;
begin
  Canvas.Font := Font;
  with Canvas do
  begin
    PaintBackground(Self, Canvas);
    Brush.Style := bsClear;
    TextTop := (Height + Font.Height) div 2 - 2;
    TextLeft := BtnWidth + 5;
    R := Rect(TextLeft, TextTop, 0, 0);
    DrawText(Handle, PChar(Caption), Length(Caption), R, DT_NOCLIP);
    // Normal draw
    case State of
      cbUnChecked:
      begin
        if Enabled then
          if not FFlat then
            Sour := Rect(0, 0, BtnWidth, BtnWidth)
          else
            if FMouseInControl or FFocused then
              Sour := Rect(0, 0, BtnWidth, BtnWidth)
            else
              Sour := Rect(5*BtnWidth, 0, 6*BtnWidth, BtnWidth)
        else
          Sour := Rect(BtnWidth*2, 0, BtnWidth*3, BtnWidth);
      end;
      cbChecked:
      begin
        if Enabled then
          if not FFlat then
            Sour := Rect(BtnWidth, 0, BtnWidth*2, BtnWidth)
          else
            if FMouseInControl or FFocused then
              Sour := Rect(BtnWidth, 0, BtnWidth*2, BtnWidth)
            else
              Sour := Rect(6*BtnWidth, 0, 7*BtnWidth, BtnWidth)
        else
          Sour := Rect(BtnWidth*4, 0, BtnWidth*5, BtnWidth);
      end;
      cbGrayed:
        Sour := Rect(BtnWidth*3, 0, BtnWidth*4, BtnWidth);
    end;
    Dest := Rect(1, (Height - BtnWidth) div 2, 1 + BtnWidth,
      (Height - BtnWidth) div 2 + BtnWidth);
    Canvas.CopyRect(Dest, Btns.Canvas, Sour);
    if FFocused then
    begin
      Brush.Color := clBtnFace;
      R := Bounds(TextLeft - 1, TextTop - 1, TextWidth(Caption) + 2,
        TextTop + TextHeight(Caption) - 1);
      FrameRect(R);
      DrawFocusRect(R);
    end;
  end;
end;

function TksoOfficeCheckBox.GetCaption:TCaption;
var
  Buf: array[0..255] of Char;
begin
  GetTextBuf(Buf, 256);
  Result := StrPas(Buf);
end;

procedure TksoOfficeCheckBox.SetCaption(const Value:TCaption);
var
  Buffer: array[0..255] of Char;
begin
  if GetCaption <> Value then
    SetTextBuf(StrPCopy(Buffer,Value));
  Invalidate;
end;

procedure TksoOfficeCheckBox.SetState(Value:TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Paint;
    Click;
  end;
end;

function TksoOfficeCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TksoOfficeCheckBox.SetChecked(Value:Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
end;

procedure TksoOfficeCheckBox.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Paint;
end;

procedure TksoOfficeCheckBox.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  Paint;
end;

procedure TksoOfficeCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  SetFocus;
  FFocused := True;
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
end;

procedure TksoOfficeCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  MouseCapture := False;
  if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then
    Checked := not Checked;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TksoOfficeCheckBox.MouseMove(Shift: TShiftState;X, Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

procedure TksoOfficeCheckBox.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  inherited KeyDown(Key,Shift);
end;

procedure TksoOfficeCheckBox.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
    Checked := not Checked;
end;

procedure TksoOfficeCheckBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TksoOfficeCheckBox.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  Invalidate;
end;

{ TksoOfficeRadioButton }

procedure TksoOfficeRadioButton.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  if FFlat then Paint;
end;

procedure TksoOfficeRadioButton.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  if FFlat then Paint;
end;

constructor TksoOfficeRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 98;
  Height := 20;
  ParentColor := False;
  Color := clBtnFace;
  FMouseInControl := false;
end;

procedure TksoOfficeRadioButton.Paint;
var
  R, Dest, Sour: TRect;
  TextTop, TextLeft: integer;
begin
  Canvas.Font := Font;
  with Canvas do
  begin
    PaintBackground(Self, Canvas);
    // out text
    Brush.Style := bsClear;
    TextTop := (Height + Font.Height) div 2 - 2;
    TextLeft := BtnWidth + 5;
    R := Rect(TextLeft, TextTop, 0, 0);
    DrawText(Handle, PChar(Caption), Length(Caption), R, DT_NOCLIP);
    // Normal draw
    if not Checked then
      if Enabled then
        if not FFlat then
          Sour := Rect(0, BtnWidth, BtnWidth, BtnWidth*2)
        else
          if FMouseInControl or FFocused then
            Sour := Rect(0, BtnWidth, BtnWidth, BtnWidth*2)
          else
            Sour := Rect(5*BtnWidth, BtnWidth, 6*BtnWidth, BtnWidth*2)
      else
        Sour := Rect(BtnWidth*2, BtnWidth, BtnWidth*3, BtnWidth*2);
    if Checked then
      if Enabled then
        if not FFlat then
          Sour := Rect(BtnWidth, BtnWidth, BtnWidth*2, BtnWidth*2)
        else
          if FMouseInControl or FFocused then
            Sour := Rect(BtnWidth, BtnWidth, BtnWidth*2, BtnWidth*2)
          else
            Sour := Rect(BtnWidth*6, BtnWidth, BtnWidth*7, BtnWidth*2)
      else
        Sour := Rect(BtnWidth*4, BtnWidth, BtnWidth*5, BtnWidth*2);
    Dest := Rect(1, (Height - BtnWidth) div 2, 1 + BtnWidth,
      (Height - BtnWidth) div 2 + BtnWidth);
    Canvas.CopyRect(Dest, Btns.Canvas, Sour);
    if FFocused then
    begin
      Brush.Color := clBtnFace;
      R := Bounds(TextLeft - 1, TextTop - 1, TextWidth(Caption) + 2,
        TextTop + TextHeight(Caption) + 2);
      FrameRect(R);
      DrawFocusRect(R);
    end;
  end;
end;

function TksoOfficeRadioButton.GetCaption: TCaption;
var
  Buf: array[0..256] of Char;
begin
  GetTextBuf(Buf, 256);
  Result := StrPas(Buf);
end;

procedure TksoOfficeRadioButton.SetCaption(const Value:TCaption);
var
  Buffer: array[0..255] of Char;
begin
  if GetCaption <> Value then
    SetTextBuf(StrPCopy(Buffer, Value));
  Invalidate;
end;

procedure TksoOfficeRadioButton.TurnSiblingsOff;
var
  i: integer;
  Sibling: TksoOfficeRadioButton;
begin
  if Parent <> nil then
  for i := 0 to Parent.ControlCount-1 do
    if Parent.Controls[i] is TksoOfficeRadioButton then
    begin
      Sibling := TksoOfficeRadioButton(Parent.Controls[i]);
      if (Sibling <> Self) and
         (Sibling.GroupIndex=GroupIndex) then
        Sibling.SetChecked(False);
    end;
end;

procedure TksoOfficeRadioButton.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    TabStop := Value;
    FChecked := Value;
    if Value then
    begin
      TurnSiblingsOff;
      Click;
    end;
    Paint;
  end;
end;

procedure TksoOfficeRadioButton.SetCheckColor(Value:TColor);
begin
  FCheckColor := Value;
  Paint;
end;

procedure TksoOfficeRadioButton.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Checked := True;
  Paint;
end;

procedure TksoOfficeRadioButton.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  Paint;
end;

procedure TksoOfficeRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  SetFocus;
  FFocused := True;
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
end;

procedure TksoOfficeRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  MouseCapture := False;
  if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height)
    and not Checked then Checked := True;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TksoOfficeRadioButton.MouseMove(Shift: TShiftState;X, Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

procedure TksoOfficeRadioButton.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  inherited KeyDown(Key,Shift);
end;

procedure TksoOfficeRadioButton.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
  begin
    if not Checked then Checked := True;
  end;
end;

procedure TksoOfficeRadioButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TksoOfficeRadioButton.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  Invalidate;
end;

procedure TksoOfficeRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Focused then
        Checked := true;
      Result := 1;
    end else
      inherited;
end;

{ TksoOfficeGroupBox }

constructor TksoOfficeGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csReplicatable];
  FFlat := false;
  Width := 185;
  Height := 105;
end;

procedure TksoOfficeGroupBox.AlignControls(AControl: TControl; var Rect: TRect);
begin
  Canvas.Font := Font;
  Inc(Rect.Top, Canvas.TextHeight('0'));
  InflateRect(Rect, -1, -1);
  if Ctl3d then InflateRect(Rect, -1, -1);
  inherited AlignControls(AControl, Rect);
end;

procedure TksoOfficeGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TksoOfficeGroupBox.CMMouseEnter(var Message: TMessage);
var
  P: TPoint;
begin
  if not FFlat then Exit;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X >= 0) and (P.Y >= 0) and (P.X <= Width) and (P.Y <= Height) then
    FMouseInControl := true;
  Paint;
end;

procedure TksoOfficeGroupBox.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
begin
  if not FFlat then Exit;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X <= 0) or (P.Y <= 0) or (P.X >= Width) or (P.Y >= Height) then
    FMouseInControl := false;
  Paint;
end;

procedure TksoOfficeGroupBox.Paint;
var
  H: Integer;
  R: TRect;
begin
  with Canvas do
  begin
    Font := Self.Font;
    { Draw background }
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Style := psClear;
    Rectangle(0, 0, Width, Height);
    { Draw frame }
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width-1, Height-1);
    if not FFlat then
    begin
      Brush.Style := bsClear;
      Pen.Style := psSolid;
      Pen.Color := clBtnShadow;
      Rectangle(R.left, R.top, R.right, R.bottom);
      OffsetRect(R, 1, 1);
      Pen.Color := clBtnHighlight;
      Rectangle(R.left, R.top, R.right, R.bottom);
    end
    else
      if FMouseInControl then
      begin
        Brush.Style := bsClear;
        Pen.Style := psSolid;
        Pen.Color := clBtnShadow;
        Rectangle(R.left, R.top, R.right, R.bottom);
        OffsetRect(R, 1, 1);
        Pen.Color := clBtnHighlight;
        Rectangle(R.left, R.top, R.right, R.bottom);
      end
      else
      begin
        Brush.Style := bsClear;
        Pen.Style := psSolid;
        Pen.Color := clBtnShadow;
        Brush.Color := Color;
        Rectangle(R.left, R.top, R.right+1, R.bottom+1);
      end;
    if Text <> '' then
    begin
      R := Rect(8, 0, 0, H);
      DrawText(Handle, PChar(Text), Length(Text), R, DT_LEFT or DT_SINGLELINE or
        DT_CALCRECT);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      DrawText(Handle, PChar(Text), Length(Text), R, DT_LEFT or DT_SINGLELINE);
    end;
  end;
end;

procedure TksoOfficeGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end else
      inherited;
end;

procedure TksoOfficeGroupBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  Realign;
end;

procedure TksoOfficeGroupBox.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Realign;
end;

procedure TksoOfficeGroupBox.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TksoOfficeGroupBox.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  Invalidate;
end;

{ TksoOfficeListBox }

constructor TksoOfficeListBox.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  Height := 40;
  FMouseInControl := false;
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
end;

procedure TksoOfficeListBox.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  FMouseInControl := true;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeListBox.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  FMouseInControl := false;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeListBox.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics (DC, Metrics);
  SelectObject (DC, SaveFont);
  ReleaseDC (0, DC);
end;

procedure TksoOfficeListBox.Loaded;
begin
  inherited Loaded;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TksoOfficeListBox.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TksoOfficeListBox.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TksoOfficeListBox.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.WMNCPaint (var Message: TMessage);
begin
  inherited;
  DrawBorder;
  Message.Result := 0;
end;

procedure TksoOfficeListBox.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.SetBorderStyleFlat(
  const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeListBox.DrawBorder;
var
  R : TRect;
  Canvas: TCanvas;
begin
  R := Rect(0, 0, Width, Height);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(Handle);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Color;
    Canvas.Brush.Style := bsClear;
    { Draw background }
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Rectangle(1, 1, Width-1, Height-1);
    { Draw border }
    if FFlat then
      if Focused or FMouseInControl then
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFocused)
      else
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFlat)
    else
      OfficeUtils.DrawBorder(Canvas, R, FBorderStyle);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TksoOfficeListBox.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

{ TksoOfficeCheckListBox }

constructor TksoOfficeCheckListBox.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  Height := 40;
  FMouseInControl := false;
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
end;

procedure TksoOfficeCheckListBox.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  FMouseInControl := true;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  FMouseInControl := false;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics (DC, Metrics);
  SelectObject (DC, SaveFont);
  ReleaseDC (0, DC);
end;

procedure TksoOfficeCheckListBox.Loaded;
begin
  inherited Loaded;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TksoOfficeCheckListBox.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TksoOfficeCheckListBox.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TksoOfficeCheckListBox.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.WMNCPaint (var Message: TMessage);
begin
  inherited;
  DrawBorder;
  Message.Result := 0;
end;

procedure TksoOfficeCheckListBox.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.SetBorderStyleFlat(
  const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficeCheckListBox.DrawBorder;
var
  R : TRect;
  Canvas: TCanvas;
begin
  R := Rect(0, 0, Width, Height);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(Handle);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Color;
    Canvas.Brush.Style := bsClear;
    { Draw background }
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Rectangle(1, 1, Width-1, Height-1);
    { Draw border }
    if FFlat then
      if Focused or FMouseInControl then
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFocused)
      else
        OfficeUtils.DrawBorder(Canvas, R, FBorderStyleFlat)
    else
      OfficeUtils.DrawBorder(Canvas, R, FBorderStyle);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TksoOfficeCheckListBox.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

initialization
  Btns := TBitmap.Create;
  ConvertBtns;

  randomize;
finalization
  Btns.Free;
end.
