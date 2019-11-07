
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeEdit;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, OfficeTypes, OfficeUtils;

type

  TksoEdit = class(TEdit)
  private
    FMouseInControl: boolean;
    FBorderStyle: TksoBorderStyle;
    FFlat: boolean;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure DrawBorder;
    procedure InvalidateFrame;
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);
  protected
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
  end;

implementation {===============================================================}

{ TksoEdit }

constructor TksoEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
end;

destructor TksoEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TksoEdit.CMMouseEnter(var Msg : TMessage);
begin
  inherited;
  FMouseInControl := true;
  if FFlat then InvalidateFrame;
end;

procedure TksoEdit.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  FMouseInControl := false;
  if FFlat then InvalidateFrame;
end;

procedure TksoEdit.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

procedure TksoEdit.DrawBorder;
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

procedure TksoEdit.WMNCPaint(var Msg : TMessage);
begin
  inherited;
  DrawBorder;
  Msg.Result := 0;
end;

procedure TksoEdit.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoEdit.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;
  InvalidateFrame;
end;

procedure TksoEdit.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoEdit.SetBorderStyleFlat(const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoEdit.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoEdit.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  if csLoading in ComponentState then Exit;
  if not FFlat then
  begin
    FBorderStyle := kbsSunken;
    FBorderStyleFlat := kbsSolid;
    FBorderStyleFocused := kbsOuterSunken;
  end
  else
  begin
    FBorderStyle := kbsSunken;
    FBorderStyleFlat := kbsSolid;
    FBorderStyleFocused := kbsOuterSunken;
  end;
  InvalidateFrame;
end;

end.
