
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit TrackCombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo;

type

{ TksoTrackComboBox }

  TksoTrackComboBox = class(TksoAbstractComboBox)
  private
    FSliderVisible: boolean;
    FLineSize: Integer;
    FMin: Integer;
    FPageSize: Integer;
    FThumbLength: Integer;
    FBarWidth: integer;
    FFrequency: Integer;
    FBarHeight: integer;
    FMax: Integer;
    FPosition: Integer;
    FValue: integer;
    FTickMarks: TTickMark;
    FTickStyle: TTickStyle;
    FOrientation: TTrackBarOrientation;
    FAlignment: TAlignment;
    procedure SetBarHeight(const Value: integer);
    procedure SetBarWidth(const Value: integer);
    procedure SetFrequency(const Value: Integer);
    procedure SetLineSize(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetOrientation(const Value: TTrackBarOrientation);
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSliderVisible(const Value: boolean);
    procedure SetThumbLength(const Value: Integer);
    procedure SetTickMarks(const Value: TTickMark);
    procedure SetTickStyle(const Value: TTickStyle);
    procedure SetValue(const Value: integer);
    procedure SetAlignment(const Value: TAlignment);
  protected
    { Override declarations }
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
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BarWidth: integer read FBarWidth write SetBarWidth;
    property BarHeight: integer read FBarHeight write SetBarHeight;
    property Color;
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
    property Frequency: Integer read FFrequency write SetFrequency;
    property BorderStyle;
    property LineSize: Integer read FLineSize write SetLineSize;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;
    property PageSize: Integer read FPageSize write SetPageSize;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition;
    property ShowHint;
    property SliderVisible: boolean read FSliderVisible write SetSliderVisible;
    property TabOrder;
    property TabStop;
    property ThumbLength: Integer read FThumbLength write SetThumbLength;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle;
    property Value: integer read FValue write SetValue;
    property Visible;
    property OnClick;
    property OnChange;
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

uses TrackComboForm;

{ TksoTrackComboBox }

constructor TksoTrackComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 60;
  Height := 19;
  FBarWidth := 150;
  FBarHeight := 45;
  FFrequency := 1;
  FLineSize := 1;
  FMax := 10;
  FPageSize := 2;
  FSliderVisible := true;
  FThumbLength := 20;
  FTickMarks := tmBottomRight;
  TickStyle := tsAuto;
end;

destructor TksoTrackComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoTrackComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TksoTrackComboBox.Change;
begin
  inherited Change;
end;

procedure TksoTrackComboBox.DrawBox(Canvas: TCanvas; R: TRect; State: TComboState);
var
  Text: string;
begin
  with Canvas do
  begin
    case State of
      csNormal, csDown:
      begin
        Canvas.Pen.Color := clWindowText;
        Brush.Color := Color;
        Font.Color := clWindowText;
      end;
      csFocused:
      begin
        Canvas.Pen.Color := clHighlightText;
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end;
      csDisable:
      begin
        Pen.Color := clSilver;
        Brush.Color := clSilver;
        Font.Color := clGray;
      end;
    end;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    // Draw
    FillRect(R);
    Text := IntToStr(FPosition);
    case FAlignment of
      taLeftJustify: TextOut(R.left+1, R.top + (R.bottom-R.top-
        TextHeight(Text)) div 2, Text);
      taRightJustify: TextOut(R.right-TextWidth(Text)-2, R.top + (R.bottom-R.top-
        TextHeight(IntToStr(FPosition))) div 2, IntToStr(FPosition));
      taCenter: TextOut(R.left + (R.right-R.left-TextWidth(Text)) div 2,
        R.top + (R.bottom-R.top-TextHeight(IntToStr(FPosition))) div 2,
        IntToStr(FPosition));
    end;
    // End Draw
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clHighlightText;
    if State = csFocused then
      Canvas.DrawFocusRect(R);
  end;
end;

function TksoTrackComboBox.GetDropDownForm: TCustomForm;
begin
  Result := TfrmTrackCombo.Create(Self);
end;

procedure TksoTrackComboBox.SetBarHeight(const Value: integer);
begin
  FBarHeight := Value;
end;

procedure TksoTrackComboBox.SetBarWidth(const Value: integer);
begin
  FBarWidth := Value;
end;

procedure TksoTrackComboBox.SetFrequency(const Value: Integer);
begin
  FFrequency := Value;
end;

procedure TksoTrackComboBox.SetLineSize(const Value: Integer);
begin
  FLineSize := Value;
end;

procedure TksoTrackComboBox.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure TksoTrackComboBox.SetMin(const Value: Integer);
begin
  FMin := Value;
end;

procedure TksoTrackComboBox.SetOrientation(
  const Value: TTrackBarOrientation);
var
  Temp: integer;
begin
  if Value <> FOrientation then
  begin
    FOrientation := Value;
    if csDesigning in ComponentState then
    begin
      Temp := FBarWidth;
      FBarWidth := FBarHeight;
      FBarHeight := Temp;
    end;
  end;
end;

procedure TksoTrackComboBox.SetPageSize(const Value: Integer);
begin
  FPageSize := Value;
end;

procedure TksoTrackComboBox.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;
  Invalidate;
end;

procedure TksoTrackComboBox.SetSliderVisible(const Value: boolean);
begin
  FSliderVisible := Value;
end;

procedure TksoTrackComboBox.SetThumbLength(const Value: Integer);
begin
  FThumbLength := Value;
end;

procedure TksoTrackComboBox.SetTickMarks(const Value: TTickMark);
begin
  FTickMarks := Value;
end;

procedure TksoTrackComboBox.SetTickStyle(const Value: TTickStyle);
begin
  FTickStyle := Value;
end;

procedure TksoTrackComboBox.SetValue(const Value: integer);
begin
  FValue := Value;
end;

procedure TksoTrackComboBox.Next;
begin
  FPosition := FPosition + FLineSize;
  if FPosition > FMax then
    FPosition := FMax;
  Invalidate;
end;

procedure TksoTrackComboBox.Previous;
begin
  FPosition := FPosition - FLineSize;
  if FPosition < FMin then
    FPosition := FMin;
  Invalidate;
end;

procedure TksoTrackComboBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

end.
