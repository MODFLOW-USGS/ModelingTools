
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit SpectrumCombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo;

type

{ TksoSpectrumComboBox }

  TksoSpectrumComboBox = class(TksoAbstractComboBox)
  private
    FColorValue: TColor;
    procedure SetColorValue(const Value: TColor);
  protected
    function GetDropDownForm: TCustomForm; override;
    procedure DrawBox(Canvas: TCanvas; R: TRect; State: TComboState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Change; override;
    procedure Previous; override;
    procedure Next; override;
  published
    property Color;
    property ColorValue: TColor read FColorValue write SetColorValue;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property BorderStyle;
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
    property OnChange;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation {===============================================================}

uses SpectrumComboForm;

{ TksoSpectrumComboBox }

constructor TksoSpectrumComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 50;
  FColorValue := clBlack;
end;

destructor TksoSpectrumComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoSpectrumComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TksoSpectrumComboBox.Change;
begin
  inherited Change;
end;

procedure TksoSpectrumComboBox.SetColorValue(const Value: TColor);
begin
  FColorValue := Value;
  Invalidate;
end;

procedure TksoSpectrumComboBox.DrawBox(Canvas: TCanvas; R: TRect; State: TComboState);
var
  R1: TRect;
begin
  with Canvas do
  begin
    case State of
      csNormal, csDown:
      begin
        Pen.Color := clWindowText;
        Brush.Color := clWindow;
      end;
      csFocused:
      begin
        Pen.Color := clHighlightText;
        Brush.Color := clHighlight;
      end;
      csDisable:
      begin
        Pen.Color := clSilver;
        Brush.Color := clSilver;
      end;
    end;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    // Draw
    FillRect(R);
    R1 := R;
    InflateRect(R1, -2, -2);
    Brush.Color := FColorValue;
    Rectangle(R1.left, R1.top, R1.right, R1.bottom);
    // End Draw
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clHighlightText;
    if State = csFocused then
      Canvas.DrawFocusRect(R);
  end;
end;

function TksoSpectrumComboBox.GetDropDownForm: TCustomForm;
begin
  Result := TfrmSpectrumCombo.Create(Self);
end;

procedure TksoSpectrumComboBox.Next;
begin
end;

procedure TksoSpectrumComboBox.Previous;
begin
end;

end.
