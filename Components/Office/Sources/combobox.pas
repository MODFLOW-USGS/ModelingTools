
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit ComboBox;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboEdit;

type

  TComboStyle = (csDropDown, csDropDownList, csOwnerDrawFixed,
    csOwnerDrawVariable);

  TDrawItemEvent = procedure(Canvas: TCanvas; Index: Integer;
    Rect: TRect; State: TOwnerDrawState) of object;

  TMeasureItemEvent = procedure(Index: Integer; var Height: Integer) of object;

  TksoCustomComboBox = class(TksoAbstractComboBox)
  private
    FItems: TStrings;
    FDropDownCount: integer;
    FItemIndex: integer;
    FText: string;
    FEdit: TksoComboEdit;
    FOnChange: TNotifyEvent;
    FItemHeight: integer;
    FStyle: TComboStyle;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FSorted: boolean;
    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(const Value: integer);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure DoChange(Sender: TObject);
    procedure SetItemHeight(const Value: integer);
    function GetItemHeight: integer;
    procedure SetStyle(const Value: TComboStyle);
    procedure SetSorted(const Value: boolean);
    { Private declarations }
  protected
    { Protected declarations }
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetParent(AParent: TWinControl); override;
    { Abstract override }
    function GetDropDownForm: TCustomForm; override;
    procedure DrawBox(Canvas: TCanvas; R: TRect; State: TComboState); override;
    property Edit: TksoComboEdit read FEdit;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    { Abstract override }
    procedure DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); dynamic;
    procedure MeasureItem(Index: Integer; var Height: Integer); dynamic;
    procedure Change; override;
    procedure Previous; override;
    procedure Next; override;
    procedure EnterFocus; dynamic;
    procedure ExitFocus; dynamic; 
    { public property }
    property DropDownCount: integer read FDropDownCount write FDropDownCount;
    property Color;
    property Font;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
    property Items: TStrings read FItems write SetItems;
    property Sorted: boolean read FSorted write SetSorted;
    property Style: TComboStyle read FStyle write SetStyle;
    property Text: string read GetText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  published
    { Published declarations }
  end;

  TksoComboBox = class(TksoCustomComboBox)
  published
    { Published declarations }
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderStyleFlat;
    property BorderStyleFocused;
    property Constraints;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Flat;
    property Font;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
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
    property Text;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDrawItem;
    property OnMeasureItem;
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

uses ComboForm;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 1;
end;

{ TksoCustomComboBox }

constructor TksoCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFixedHeight];
  FSorted := false;
  FStyle := csDropDown;
  FItems := TStringList.Create;
  FDropDownCount := 8;
  FItemHeight := 15;
  FItemIndex := -1;
  Width := 150;
  Height := 21;
end;

procedure TksoCustomComboBox.Loaded;
begin
  inherited Loaded;
end;

destructor TksoCustomComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TksoCustomComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (FStyle = csDropDown) and (AParent <> nil) and (FEdit = nil) then
  begin
    FEdit := TksoComboEdit.Create(Self);
    FEdit.Parent := Self;
    Canvas.Font := Font;
    FEdit.Height := ComboBox.GetItemHeight(Font);
    FEdit.BoundsRect := Rect(3, 0, ClientWidth - ButtonWidth-4, FEdit.Height);
    FEdit.Top := (ClientHeight - FEdit.Height) div 2;
    if (Style = csDropDown) and not (csDesigning in ComponentState) then
      FEdit.Visible := true
    else
      FEdit.Visible := false;
    if not TabStop then
      FEdit.TabStop := false;
    TabStop := false;
    FEdit.Text := FText;
  end;
end;

procedure TksoCustomComboBox.DrawBox(Canvas: TCanvas; R: TRect; State: TComboState);
var
  ODState: TOwnerDrawState;
begin
  if Style = csDropDown then Exit;
  Canvas.Font.Assign(Font);
  if (Style = csOwnerDrawFixed) or (Style = csOwnerDrawVariable) then
  begin
    case State of
      csNormal, csDown: with Canvas do
      begin
        ODState := [];
        Canvas.Pen.Color := clWindowText;
        Brush.Color := Color;
      end;
      csFocused: with Canvas do
      begin
        ODState := [odFocused];
        Canvas.Pen.Color := clHighlightText;
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end;
      csDisable: with Canvas do
      begin
        ODState := [odDisabled];
        Pen.Color := clSilver;
        Brush.Color := clSilver;
        Font.Color := clGray;
      end;
    end;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    DrawItem(Canvas, FItemIndex, R, ODState);
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clHighlightText;
    if State = csFocused then
      Canvas.DrawFocusRect(R);
    Exit;
  end;
  with Canvas do
  begin
    Pen.Style := psClear;
    Brush.Color := Color;
    if State = csFocused then
      Brush.Color := clHighlight;
    Rectangle(R.left+1, R.top+1, R.right, R.bottom);
    if State = csFocused then
      Font.Color := clHighlightText;
    if not Enabled then
      Font.Color := clGray;

    Brush.Style := bsClear;
    TextOut(R.left+1, R.top + (R.bottom-R.top-TextHeight(Text)) div 2,
      Text);
    Pen.Style := psSolid;
    Pen.Color := clHighlightText;
    if State = csFocused then
      DrawFocusRect(R);
  end;
end;

function TksoCustomComboBox.GetDropDownForm: TCustomForm;
begin
  Result := TfrmCombo.Create(Self);
end;

function TksoCustomComboBox.GetText: string;
begin
  if (Style <> csDropDown) and (FItemIndex <> -1) then
    Result := FItems[FItemIndex];
  if Style = csDropDown then
    Result := FText;
end;

procedure TksoCustomComboBox.Next;
begin
  if ItemIndex < FItems.Count-1 then
    ItemIndex := ItemIndex + 1;
end;

procedure TksoCustomComboBox.Previous;
begin
  if ItemIndex > 0 then
    ItemIndex := ItemIndex - 1;
end;

procedure TksoCustomComboBox.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    if (FItemIndex > 0) and (Style = csDropDown) then
      Text := FItems[Value];
    Invalidate;
  end;
end;

procedure TksoCustomComboBox.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TksoCustomComboBox.SetText(const Value: string);
begin
  if (Style = csDropDown) and (FEdit <> nil) then
  begin
    FEdit.Text := Value;
    FEdit.SelectAll;
  end;
  FText := Value;
  Invalidate;
end;

procedure TksoCustomComboBox.WMSize(var Msg: TWMSize);
begin
  inherited ;
  FItemHeight := Height - 8;
  if FEdit = nil then exit;
  FEdit.Height := ComboBox.GetItemHeight(Font);
  FEdit.BoundsRect := Rect(3, 0, ClientWidth - ButtonWidth-4, FEdit.Height);
  FEdit.Top := (ClientHeight - FEdit.Height) div 2;
end;

procedure TksoCustomComboBox.DoChange(Sender: TObject);
begin
  if (csDesigning in ComponentState) then Exit;
  if Style = csDropDown then
    FText := FEdit.Text;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TksoCustomComboBox.WMKillFocus(var Message: TWMSetFocus);
begin
  if FStyle = csDropDown then Exit;
  inherited ;
end;

procedure TksoCustomComboBox.WMSetFocus(var Message: TWMSetFocus);
begin
  if FStyle = csDropDown then Exit;
  inherited ;
end;

procedure TksoCustomComboBox.Change;
begin
  DoChange(Self);
end;

procedure TksoCustomComboBox.SetStyle(const Value: TComboStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if FStyle = csDropDownList then FEdit.Visible := false;
    Invalidate;
  end;
end;

function TksoCustomComboBox.GetItemHeight: integer;
begin
  Result := FItemHeight;
end;

procedure TksoCustomComboBox.SetItemHeight(const Value: integer);
begin
  if Style in [csDropDown, csDropDownList] then
  begin
    FItemHeight := Height - 8;
    Exit;
  end;
  FItemHeight := Value;
  Height := Value + 8;
end;

procedure TksoCustomComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited ;
  Canvas.Font := Font;
  Height := ComboBox.GetItemHeight(Font) + 8;
end;

procedure TksoCustomComboBox.DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Canvas, Index, Rect, State)
  else
  begin
    Canvas.Font.Assign(Self.Font);
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TksoCustomComboBox.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Index, Height);
end;

procedure TksoCustomComboBox.SetSorted(const Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if (FItems is TStringList) then
      (FItems as TStringList).Sorted := Value;
  end;
end;

procedure TksoCustomComboBox.EnterFocus;
begin
end;

procedure TksoCustomComboBox.ExitFocus;
begin
end;

end.
