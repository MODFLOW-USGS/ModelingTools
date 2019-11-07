
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit ColorCombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboBox;

type

{ TksoColorComboBox }

  TksoColorComboBox = class(TksoCustomComboBox)
  private
    FColorValue: TColor;
    FDisplayNames: Boolean;
    FColorNames: TStrings;
    FOnChange: TNotifyEvent;
    function GetColorValue: TColor;
    procedure SetColorValue(NewValue: TColor);
    procedure SetDisplayNames(Value: Boolean);
    procedure SetColorNames(Value: TStrings);
    procedure ColorNamksohanged(Sender: TObject);
  protected
    procedure Click; override;
    procedure PopulateList; virtual;
    procedure DoChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change; override;
    property Text;
    { Override methods}
    procedure DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  published
    property Color;
    property Ctl3D;
    property ColorValue: TColor read GetColorValue write SetColorValue
      default clBlack;
    property ColorNames: TStrings read FColorNames write SetColorNames;
    property DisplayNames: Boolean read FDisplayNames write SetDisplayNames
      default True;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawItem;
    property OnMeasureItem;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation {===============================================================}

{ TksoColorComboBox }

const
  ColorsInList = 16;
  ColorValues: array [0..ColorsInList - 1] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

constructor TksoColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;  { make default color selected }
  FColorNames := TStringList.Create;
  ItemIndex := 0;
  TStringList(FColorNames).OnChange := ColorNamksohanged;
  FDisplayNames := True;
  PopulateList;
end;

destructor TksoColorComboBox.Destroy;
begin
  TStringList(FColorNames).OnChange := nil;
  FColorNames.Free;
  FColorNames := nil;
  inherited Destroy;
end;

procedure TksoColorComboBox.PopulateList;
var
  I: Integer;
  ColorName: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for I := 0 to Pred(ColorsInList) do begin
      if (I <= Pred(FColorNames.Count)) and (FColorNames[I] <> '') then
        ColorName := FColorNames[I]
      else
        { delete two first characters which prefix "cl" educated }
        ColorName := Copy(ColorToString(ColorValues[I]), 3, MaxInt);
      Items.AddObject(ColorName, TObject(ColorValues[I]));
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TksoColorComboBox.ColorNamksohanged(Sender: TObject);
begin
  if HandleAllocated then
  begin
    PopulateList;
    FColorValue := ColorValue;
  end;
end;

procedure TksoColorComboBox.SetColorNames(Value: TStrings);
begin
  FColorNames.Assign(Value);
end;

procedure TksoColorComboBox.SetDisplayNames(Value: Boolean);
begin
  if DisplayNames <> Value then
  begin
    FDisplayNames := Value;
    Invalidate;
  end;
end;

function TksoColorComboBox.GetColorValue: TColor;
begin
  Result := FColorValue;
  if ItemIndex >= 0 then
    Result := TColor(Items.Objects[ItemIndex])
end;

procedure TksoColorComboBox.SetColorValue(NewValue: TColor);
var
  Item: Integer;
  CurrentColor: TColor;
begin
  if (ItemIndex < 0) or (NewValue <> FColorValue) then
  begin
    FColorValue := NewValue;
    { change selected item }
    for Item := 0 to Pred(Items.Count) do
    begin
      CurrentColor := TColor(Items.Objects[Item]);
      if CurrentColor = NewValue then
      begin
        if ItemIndex <> Item then
          ItemIndex := Item;
        Exit;
      end;
    end;
  end;
end;

procedure TksoColorComboBox.DrawItem(Canvas: TCanvas; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  ColorWidth = 22;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  if Index < 0 then Exit;
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  if FDisplayNames then ARect.Right := ARect.Left + ColorWidth
  else Dec(ARect.Right, 3);
  with Canvas do begin
    FillRect(Rect);
    Safer := Brush.Color;
    Pen.Color := clWindowText;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect);
    finally
      Brush.Color := Safer;
    end;
    if FDisplayNames then begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := Rect.Left + ColorWidth + 6;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect,
        DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    end;
  end;
end;

procedure TksoColorComboBox.Change;
var
  AColor: TColor;
begin
  inherited Change;
  AColor := GetColorValue;
  if FColorValue <> AColor then
  begin
    ColorValue := AColor;
    DoChange;
  end;
end;

procedure TksoColorComboBox.Click;
begin
  if ItemIndex >= 0 then ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
end;

procedure TksoColorComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if Assigned(FOnChange) then FOnChange(Self);
end;

end.
 