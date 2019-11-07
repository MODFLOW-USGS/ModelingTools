
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit ColorCombo2000;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo;

const

  ColorCount = 16;

  ColorValues: array [0..ColorCount - 1] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

type

  TColorKind = (ckDefault, ckStandard, ckCustom);

{ TksoColorComboBox2000 }

  TksoColorComboBox2000 = class(TksoAbstractComboBox)
  private
    FColorValue: TColor;
    FColorIndex: Integer;
    FColorKind: TColorKind;
    FDefaultColor: TColor;
    FCustomColors: TStrings;
    procedure SetColorValue(const Value: TColor);
    procedure SetColorIndex(const Value: Integer);
    procedure SetColorKind(const Value: TColorKind);
    procedure SetDefaultColor(const Value: TColor);
    procedure SetCustomColors(const Value: TStrings);
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
    property ColorKind: TColorKind read FColorKind write SetColorKind;
    property ColorIndex: Integer read FColorIndex write SetColorIndex;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor;
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

uses ColorForm2000;

{ TksoColorComboBox2000 }

constructor TksoColorComboBox2000.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);
  Width := 50;
  FColorValue := clBlack;
  FColorKind := ckDefault;
  FCustomColors := TStringList.Create;
  for i := 0 to ColorCount-1 do
    FCustomColors.Add(ColorToString(clWhite));
end;

destructor TksoColorComboBox2000.Destroy;
begin
  FCustomColors.Free;
  inherited Destroy;
end;

procedure TksoColorComboBox2000.Loaded;
begin
  inherited Loaded;
end;

procedure TksoColorComboBox2000.Change;
begin
  inherited Change;
end;

procedure TksoColorComboBox2000.DrawBox(Canvas: TCanvas; R: TRect; State: TComboState);
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

function TksoColorComboBox2000.GetDropDownForm: TCustomForm;
begin
  Result := TfrmColorCombo2000.Create(Self);
end;

procedure TksoColorComboBox2000.Next;
begin
end;

procedure TksoColorComboBox2000.Previous;
begin
end;

procedure TksoColorComboBox2000.SetColorIndex(const Value: Integer);
begin
  FColorIndex := Value;
  if FColorIndex < 0 then
    FColorIndex := 0;
  if FColorIndex > ColorCount-1 then
    FColorIndex := ColorCount-1;
end;

procedure TksoColorComboBox2000.SetColorValue(const Value: TColor);
var
  i: integer;
begin
  FColorValue := Value;
  Invalidate;
  if FColorValue = FDefaultColor then
  begin
    FColorKind := ckDefault;
    Exit;
  end;
  for i := 0 to ColorCount-1 do
    if FColorValue = ColorValues[i] then
    begin
      FColorKind := ckStandard;
      FColorIndex := i;
      Exit;
    end;
  for i := 0 to ColorCount-1 do
    if FColorValue = StringToColor(FCustomColors[i]) then
    begin
      FColorKind := ckCustom;
      FColorIndex := i;
      Exit;
    end;
end;

procedure TksoColorComboBox2000.SetColorKind(const Value: TColorKind);
begin
  FColorKind := Value;
end;

procedure TksoColorComboBox2000.SetDefaultColor(const Value: TColor);
begin
  FDefaultColor := Value;
end;

procedure TksoColorComboBox2000.SetCustomColors(const Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;

end.
