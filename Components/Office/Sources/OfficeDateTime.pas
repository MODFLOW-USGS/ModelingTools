
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeDateTime;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, OfficeTypes, OfficeUtils;

type

  TksoDateTimePicker = class(TDateTimePicker)
  private
    FMouseInControl: boolean;
    FFlat: boolean;
    FBorderStyle: TksoBorderStyle;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMPaint(var Msg : TMessage); message WM_PAINT;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure DrawBorder;
    procedure InvalidateFrame;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);
  protected
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
  end;

implementation {===============================================================}

uses BaseCombo;

{ TksoDateTimePicker }

constructor TksoDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
end;

destructor TksoDateTimePicker.Destroy;
begin
  inherited Destroy;
end;

procedure TksoDateTimePicker.CMMouseEnter(var Msg : TMessage);
begin
  inherited;
  FMouseInControl := true;
  if FFlat and (not Focused) then
    InvalidateFrame;
end;

procedure TksoDateTimePicker.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  FMouseInControl := false;
  if FFlat and (not Focused) then
    InvalidateFrame;
end;

procedure TksoDateTimePicker.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

procedure TksoDateTimePicker.DrawBorder;
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
    { Draw button }
    InflateRect(R, -2, -2);
    R.left := R.right - ButtonWidth;
    DrawButton(Canvas, R, FFlat, Focused or FMouseInControl, DroppedDown);
    { Draw down image }
    if not DroppedDown then
      Canvas.Draw(R.left + (R.right-R.left - DownImage.Width) div 2,
        R.top + (R.bottom-R.top - DownImage.Height) div 2, DownImage)
    else
      Canvas.Draw(R.left + (R.right-R.left - DownImage.Width) div 2 + 1,
        R.top + (R.bottom-R.top - DownImage.Height) div 2 + 1, DownImage);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TksoDateTimePicker.WMNCPaint(var Msg : TMessage);
begin
  inherited;
  DrawBorder;
  Msg.Result := 0;
end;

procedure TksoDateTimePicker.WMPaint(var Msg: TMessage);
begin
  inherited ;
  DrawBorder;
end;

procedure TksoDateTimePicker.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoDateTimePicker.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoDateTimePicker.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoDateTimePicker.SetBorderStyleFlat(const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoDateTimePicker.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoDateTimePicker.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  InvalidateFrame;
end;

end.
