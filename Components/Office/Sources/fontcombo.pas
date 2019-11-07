
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit fontcombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboBox;

type

{ TksoFontComboBox }

  TFontDevice = (fdScreen, fdPrinter, fdBoth);
  TFontListOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly, foNoSymbolFonts);
  TFontListOptions = set of TFontListOption;

  TksoFontComboBox = class(TksoCustomComboBox)
  private
    FOnChange: TNotifyEvent;
    FDevice: TFontDevice;
    FUpdate: Boolean;
    FUseFonts: Boolean;
    FFontName: string;
    FOptions: TFontListOptions;
    procedure SetFontName(const NewFontName: TFontName);
    function GetFontName: TFontName;
    function GetTrueTypeOnly: Boolean;
    procedure SetDevice(Value: TFontDevice);
    procedure SetOptions(Value: TFontListOptions);
    procedure SetTrueTypeOnly(Value: Boolean);
    procedure SetUseFonts(Value: Boolean);
    procedure Reset;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
  protected
    procedure PopulateList; virtual;
    procedure Click; override;
    procedure DoChange; dynamic;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change; override;
    procedure DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    property Text;
  published
    property Color;
    property Ctl3D;
    property Device: TFontDevice read FDevice write SetDevice default fdScreen;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property FontName: TFontName read GetFontName write SetFontName;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property BorderStyle;
    property Options: TFontListOptions read FOptions write SetOptions default [];
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TrueTypeOnly: Boolean read GetTrueTypeOnly write SetTrueTypeOnly
      stored False;
    property Visible;
    property UseFonts: Boolean read FUseFonts write SetUseFonts default False;
    property OnClick;
    property OnDrawItem;
    property OnMeasureItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

uses Printers;

{$R *.res}

const
  WRITABLE_FONTTYPE = 256;

function IsValidFont(Box: TksoFontComboBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if (foAnsiOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if (foTrueTypeOnly in Box.Options) then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);
  if (foFixedPitchOnly in Box.Options) then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH);
  if (foOEMFontsOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if (foNoOEMFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if (foNoSymbolFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> SYMBOL_CHARSET);
  if (foScalableOnly in Box.Options) then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProc(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var
  FaceName: string;
begin
  FaceName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
  with TksoFontComboBox(Data) do
    if (Items.IndexOf(FaceName) < 0) and
      IsValidFont(TksoFontComboBox(Data), EnumLogFont.elfLogFont, FontType) then
    begin
      if EnumLogFont.elfLogFont.lfCharSet <> SYMBOL_CHARSET then
        FontType := FontType or WRITABLE_FONTTYPE;
      Items.AddObject(FaceName, TObject(FontType));
    end;
  Result := 1;
end;

{ TksoFontComboBox }

var
  TrueTypeBmp: TBitmap;
  DeviceBmp: TBitmap;

constructor TksoFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FDevice := fdScreen;
  Sorted := True;
end;

destructor TksoFontComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoFontComboBox.CreateWnd;
var
  OldFont: TFontName;
begin
  OldFont := FontName;
  inherited CreateWnd;
  FUpdate := True;
  try
    PopulateList;
    FFontName := '';
    SetFontName(OldFont);
  finally
    FUpdate := False;
  end;
  if AnsiCompareText(FontName, OldFont) <> 0 then DoChange;
end;

procedure TksoFontComboBox.PopulateList;
var
  DC: HDC;
begin
  if not HandleAllocated then Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    DC := GetDC(0);
    try
      if (FDevice = fdScreen) or (FDevice = fdBoth) then
        EnumFontFamilies(DC, nil, @EnumFontsProc, Longint(Self));
      if (FDevice = fdPrinter) or (FDevice = fdBoth) then
      try
        EnumFontFamilies(Printer.Handle, nil, @EnumFontsProc, Longint(Self));
      except
        { skip any errors }
      end;
    finally
      ReleaseDC(0, DC);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TksoFontComboBox.SetFontName(const NewFontName: TFontName);
var
  Item: Integer;
begin
  if FontName <> NewFontName then
{    if not (csLoading in ComponentState) then }begin
      HandleNeeded;
      { change selected item }
      for Item := 0 to Items.Count - 1 do
        if AnsiCompareText(Items[Item], NewFontName) = 0 then begin
          ItemIndex := Item;
          DoChange;
          Exit;
        end;
    end;
end;

function TksoFontComboBox.GetFontName: TFontName;
begin
  if ItemIndex < 0 then
    Result := FFontName
  else
    Result := Items[ItemIndex];
end;

function TksoFontComboBox.GetTrueTypeOnly: Boolean;
begin
  Result := foTrueTypeOnly in FOptions;
end;

procedure TksoFontComboBox.SetOptions(Value: TFontListOptions);
begin
  if Value <> Options then begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TksoFontComboBox.SetTrueTypeOnly(Value: Boolean);
begin
  if Value <> TrueTypeOnly then begin
    if Value then FOptions := FOptions + [foTrueTypeOnly]
    else FOptions := FOptions - [foTrueTypeOnly];
    Reset;
  end;
end;

procedure TksoFontComboBox.SetDevice(Value: TFontDevice);
begin
  if Value <> FDevice then begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TksoFontComboBox.SetUseFonts(Value: Boolean);
begin
  if Value <> FUseFonts then begin
    FUseFonts := Value;
    Invalidate;
  end;
end;

procedure TksoFontComboBox.DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Bitmap: TBitmap;
  BmpWidth: Integer;
  Text: array[0..255] of Char;
begin
  if Index < 0 then Exit;
  with Canvas do
  begin
    FillRect(Rect);
    BmpWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      Bitmap := TrueTypeBMP
    else if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      Bitmap := DeviceBMP
    else Bitmap := nil;
    if Bitmap <> nil then begin
      BmpWidth := Bitmap.Width;
      BrushCopy(Bounds(Rect.Left + 2, (Rect.Top + Rect.Bottom - Bitmap.Height)
        div 2, Bitmap.Width, Bitmap.Height), Bitmap, Bounds(0, 0, Bitmap.Width,
        Bitmap.Height), Bitmap.TransparentColor);
    end;
    { uses DrawText instead of TextOut in order to get clipping against
      the combo box button }
    {TextOut(Rect.Left + bmpWidth + 6, Rect.Top, Items[Index])}
    StrPCopy(Text, Items[Index]);
    Rect.Left := Rect.Left + BmpWidth + 6;
    if FUseFonts and (Integer(Items.Objects[Index]) and WRITABLE_FONTTYPE <> 0) then
      Font.Name := Items[Index];
    DrawText(Handle, Text, StrLen(Text), Rect, DT_SINGLELINE or DT_VCENTER or
      DT_NOPREFIX);
  end;
end;

procedure TksoFontComboBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;

procedure TksoFontComboBox.Change;
begin
  inherited Change;
  DoChange;
end;

procedure TksoFontComboBox.Click;
begin
  inherited Click;
  DoChange;
end;

procedure TksoFontComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if not FUpdate and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TksoFontComboBox.Reset;
var
  SaveName: TFontName;
begin
  if HandleAllocated then begin
    FUpdate := True;
    try
      SaveName := FontName;
      PopulateList;
      FontName := SaveName;
    finally
      FUpdate := False;
      if FontName <> SaveName then DoChange;
    end;
  end;
end;

initialization
  TrueTypeBmp := TBitmap.Create;
  TrueTypeBmp.Handle := LoadBitmap(HInstance, 'KSO_FONT_TRUETYPE');
  DeviceBmp := TBitmap.Create;
  DeviceBmp.Handle := LoadBitmap(HInstance, 'KSO_FONT_DEVICE');
finalization
  TrueTypeBmp.Free;
  DeviceBmp.Free;
end.
