
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficePanel;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CommCtrl, Menus, Buttons, CheckLst, OfficeTypes, OfficeUtils;

type

{ TksoOfficePanel }

  TksoOfficePanel = class(TCustomPanel)
  private
    FMouseInControl: Boolean;
    FFlat: boolean;
    FBorderStyle: TksoBorderStyle;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    procedure DrawBorder;
    procedure InvalidateFrame;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    property Flat: boolean read FFlat write SetFlat;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation {==============================================================}

{ TksoOfficePanel }

constructor TksoOfficePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderWidth := 2;

  FMouseInControl := false;
  FBorderStyle := kbsSunken;
  FBorderStyleFlat := kbsSolid;
  FBorderStyleFocused := kbsOuterSunken;
  FFlat := false;
end;

procedure TksoOfficePanel.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := true;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficePanel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := false;
  if FFlat then InvalidateFrame;
end;

procedure TksoOfficePanel.Paint;
begin
  inherited Paint;
  DrawBorder;
end;

procedure TksoOfficePanel.SetBorderStyle(const Value: TksoBorderStyle);
begin
  FBorderStyle := Value;
  InvalidateFrame;
end;

procedure TksoOfficePanel.SetBorderStyleFlat(const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficePanel.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  InvalidateFrame;
end;

procedure TksoOfficePanel.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  InvalidateFrame;
end;

procedure TksoOfficePanel.DrawBorder;
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

procedure TksoOfficePanel.InvalidateFrame;
var
  R : TRect;
begin
  R := Rect(0, 0, Width, Height);
  RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame);
end;

end.
