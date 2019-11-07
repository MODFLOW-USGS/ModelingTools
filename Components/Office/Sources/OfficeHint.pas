
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeHint;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, OfficeTypes, OfficeUtils;

type

{ TksoHintShape }

  TksoShapePoint = record
    X, Y: SmallInt;
  end; 

  TksoHintShape = class(TPersistent)
  private
    FPoints: TList;
    FTopLeft: TPoint;
    FBottomRight: TPoint;
    function GetCount: integer;
    function GetPoint(Index: integer): TksoShapePoint; { list of point 1..100, 1..40}
  protected
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { region }
    function GetRegion(Width, Height: integer): HRgn;
    { drawing }
    procedure Draw(Canvas: TCanvas; X, Y, Width, Height: integer);
    { margins }
    property TopLeft: TPoint read FTopLeft write FTopLeft; 
    property BottomRight: TPoint read FBottomRight write FBottomRight; 
    { points }
    procedure Add(P: TksoShapePoint);
    procedure Clear;
    property Count: integer read GetCount;
    property Points[Index: integer]: TksoShapePoint read GetPoint;
  end;

{ TksoOfficeHint }

  TksoOfficeHint = class(TComponent)
  private
    FOnShowHint: TShowHintEvent;
    FActive: Boolean;
    FShape: TksoHintShape;
    FColor: TColor;
    FBorderColor: TColor;
    FFont: TFont;
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure SetActive(const Value: Boolean);
    procedure SetShape(const Value: TksoHintShape);
    procedure SetFont(const Value: TFont);
  protected
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property Color: TColor read FColor write FColor;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Font: TFont read FFont write SetFont;
    property Shape: TksoHintShape read FShape write SetShape;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
  end;

{  TksoHintWindow }

  TksoHintWindow = class(THintWindow)
  private
    FActivating: Boolean;
    FHint: TksoOfficeHint;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    function IsHintMsg(var Msg: TMsg): Boolean; override;
    procedure ReleaseHandle;
    property BiDiMode;
    property Caption;
    property Color;
    property Canvas;
    property Font;
  end;

  TksoHintWindowClass = class of TksoHintWindow;

implementation {===============================================================}

var
  Ps: array [0..1000] of TPoint;
  VarHint: TksoOfficeHint;

{ TksoHintShape }

constructor TksoHintShape.Create;
begin
  inherited Create;
  FPoints := TList.Create;
end;

destructor TksoHintShape.Destroy;
begin
  FPoints.Free;
  inherited Destroy;
end;

procedure TksoHintShape.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TksoHintShape then
  begin
    FTopLeft := (Source as TksoHintShape).FTopLeft;
    FBottomRight := (Source as TksoHintShape).FBottomRight;
    FPoints.Clear;
    for i := 0 to (Source as TksoHintShape).Count-1 do
      FPoints.Add((Source as TksoHintShape).FPoints[i]);
  end
  else
    inherited Assign(Source);
end;

function TksoHintShape.GetCount: integer;
begin
  Result := FPoints.Count;
end;

function TksoHintShape.GetPoint(Index: integer): TksoShapePoint;
begin
  Result := TksoShapePoint(FPoints[index]);
end;

procedure TksoHintShape.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Points', ReadData, WriteData, Count > 0);
end;

procedure TksoHintShape.ReadData(Stream: TStream);
var
  i, C: Integer;
  P: TksoShapePoint;
begin
  Stream.Read(C, SizeOf(C));
  if C = 0 then Exit;
  FPoints.Clear;
  for i := 0 to C-1 do
  begin
    Stream.Read(P, SizeOf(P));
    FPoints.Add(Pointer(P));
  end;
  Stream.Read(FTopLeft, SizeOf(FTopLeft));
  Stream.Read(FBottomRight, SizeOf(FBottomRight));
end;

procedure TksoHintShape.WriteData(Stream: TStream);
var
  i, C: Integer;
  P: TksoShapePoint;
begin
  C := Count;
  Stream.Write(C, SizeOf(C));
  if C = 0 then Exit;
  for i := 0 to C-1 do
  begin
    P := TksoShapePoint(FPoints[i]);
    Stream.Write(P, SizeOf(P));
  end;
  Stream.Write(FTopLeft, SizeOf(FTopLeft));
  Stream.Write(FBottomRight, SizeOf(FBottomRight));
end;

procedure TksoHintShape.Clear;
begin
  FPoints.Clear;
end;

procedure TksoHintShape.Add(P: TksoShapePoint);
begin
  FPoints.Add(Pointer(P));
end;

procedure TksoHintShape.Draw(Canvas: TCanvas; X, Y, Width, Height: integer);
var
  i: integer;
  Cash: TBitmap;
begin
  if Count = 0 then Exit;
  Cash := TBitmap.Create;
  try
    Cash.Width := Width;
    Cash.Height := Height;
    for i := 0 to Count-1 do
    begin
      Ps[i].X := Round(Points[i].X * (Width-1) / 100);
      Ps[i].Y := Round(Points[i].Y * (Height-1) / 40);
    end;
    Cash.Canvas.Brush.Color := RGB(255,0,255);
    Cash.Canvas.Rectangle(-1, -1, Width+1, Height+1);
    Cash.Canvas.Pen := Canvas.Pen;
    Cash.Canvas.Brush := Canvas.Brush;
    Cash.Canvas.Polygon(Slice(Ps, Count));
    Cash.Transparent := true;
    Cash.TransparentColor := RGB(255,0,255);
    Canvas.Draw(X, Y, Cash);
  finally
    Cash.Free;
  end;
end;

function TksoHintShape.GetRegion(Width, Height: integer): HRgn;
var
  i: integer;
  Cash: TBitmap;
begin
  Result := 0;
  if Count = 0 then Exit;
  Cash := TBitmap.Create;
  try
    Cash.Width := Width;
    Cash.Height := Height;
    Cash.Canvas.Pen.Style := psSolid;
    Cash.Canvas.Pen.Color := clBlack;
    Cash.Canvas.Brush.Style := bsSolid;
    Cash.Canvas.Brush.Color := clBlack;
    for i := 0 to Count-1 do
    begin
      Ps[i].X := Round(Points[i].X * (Width-1) / 100);
      Ps[i].Y := Round(Points[i].Y * (Height-1) / 40);
    end;
    Cash.Canvas.Polygon(Slice(Ps, Count));
    Result := CreateRegionFromBitmap(Cash, clWhite);
  finally
    Cash.Free;
  end;
end;

{ TksoHintWindow }

constructor TksoHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := $80FFFF;
end;

procedure TksoHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
  end;
end;

procedure TksoHintWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TksoHintWindow.WMNCPaint(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TksoHintWindow.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TksoHintWindow.Paint;
var
  R: TRect;
begin
  if (FHint <> nil) and (FHint.Shape.Count > 0) then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FHint.Color;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := FHint.BorderColor;
    FHint.Shape.Draw(Canvas, 0, 0, Width, Height)
  end
  else
  begin
    DrawBorder(Canvas, Rect(0, 0, Width, Height), kbsOuterRaised);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clInfoBk;
    Canvas.Pen.Style := psClear;
    Canvas.Rectangle(1, 1, Width, Height);
  end;
  R := ClientRect;
  Canvas.Font.Color := clInfoText;
  Canvas.Brush.Style := bsClear;
  if (FHint <> nil) and (FHint.Shape.Count > 0) then
  begin
    Canvas.Font.Assign(FHint.Font);
    Inc(R.left, FHint.Shape.TopLeft.X);
    Inc(R.top, FHint.Shape.TopLeft.Y);
    Dec(R.right, FHint.Shape.BottomRight.X);
    Dec(R.bottom, FHint.Shape.BottomRight.Y);
  end;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;

function TksoHintWindow.IsHintMsg(var Msg: TMsg): Boolean;
begin
  with Msg do
    Result := ((Message >= WM_KEYFIRST) and (Message <= WM_KEYLAST)) or
      ((Message = CM_ACTIVATE) or (Message = CM_DEACTIVATE)) or
      (Message = CM_APPKEYDOWN) or (Message = CM_APPSYSCOMMAND) or
      (Message = WM_COMMAND) or ((Message > WM_MOUSEMOVE) and
      (Message <= WM_MOUSELAST)) or (Message = WM_NCMOUSEMOVE);
end;

procedure TksoHintWindow.ReleaseHandle;
begin
  DestroyHandle;
end;

procedure TksoHintWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
  { Avoid flicker when calling ActivateHint }
  if FActivating then Exit;
  Width := Canvas.TextWidth(Caption) + 6;
  Height := Canvas.TextHeight(Caption) + 4;
end;

procedure TksoHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  Rgn: HRgn;
begin
  FActivating := True;
  try
    Caption := AHint;
    UpdateBoundsRect(Rect);
    if Rect.Top + Height > Screen.DesktopHeight then
      Rect.Top := Screen.DesktopHeight - Height;
    if Rect.Left + Width > Screen.DesktopWidth then
      Rect.Left := Screen.DesktopWidth - Width;
    if Rect.Left < Screen.DesktopLeft then Rect.Left := Screen.DesktopLeft;
    if Rect.Bottom < Screen.DesktopTop then Rect.Bottom := Screen.DesktopTop;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height, SWP_NOACTIVATE);
    if (FHint <> nil) and (FHint.FShape.Count > 0) then
    begin
      Rgn := FHint.FShape.GetRegion(Width, Height);
      SetWindowRgn(Handle, Rgn, true);
    end;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
      SWP_SHOWWINDOW or SWP_NOACTIVATE);
  finally
    FActivating := False;
  end;
end;

procedure TksoHintWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  ActivateHint(Rect, AHint);
  if (AData <> nil) then
    FHint := TksoOfficeHint(AData);
end;

function TksoHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  if (FHint = nil) and (VarHint <> nil) then
    FHint := VarHint;

  if FHint <> nil then
    Canvas.Font.Assign(FHint.Font);
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, PChar(AHint), -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  if (FHint = nil) or (FHint.Shape.Count = 0) then
  begin
    Inc(Result.Right, 6);
    Inc(Result.Bottom, 2);
  end
  else
  begin
    Inc(Result.Right, FHint.Shape.TopLeft.X + FHint.Shape.BottomRight.X);
    Inc(Result.Bottom, FHint.Shape.TopLeft.Y + FHint.Shape.BottomRight.Y);
  end;
end;

procedure TksoHintWindow.WMSize(var Msg: TWMSize);
begin
  inherited ;
end;

{ TksoOfficeHint }

constructor TksoOfficeHint.Create(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);

  FShape := TksoHintShape.Create;
  FFont := TFont.Create;
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    FFont.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont);

  FActive := true;
  FColor := clInfoBk;
  FBorderColor := clBtnShadow;

  VarHint := Self;
end;

destructor TksoOfficeHint.Destroy;
begin
  FFont.Free;
  FShape.Free;
  inherited Destroy;
end;

procedure TksoOfficeHint.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  HintInfo.HintData := Self;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
end;

procedure TksoOfficeHint.SetActive(const Value: Boolean);
var
  i: Integer;
begin
  FActive := Value;
  if FActive and not (csDesigning in ComponentState) then
  begin
    HintWindowClass := TksoHintWindow;
    with Application do
    begin
      ShowHint := false;
      OnShowHint := DoShowHint;
      ShowHint := true;
    end;
  end;

  if FActive and (Application.MainForm <> nil) then
    with Application.MainForm do
      for i := 0 to ComponentCount-1 do
        if (Components[i] is TksoOfficeHint) and (Components[i] <> Self) then
          if TksoOfficeHint(Components[i]).Active then
            TksoOfficeHint(Components[i]).Active := false;

  if not (csDesigning in ComponentState) and FActive then
    Application.OnShowHint := DoShowHint;
end;

procedure TksoOfficeHint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksoOfficeHint.SetShape(const Value: TksoHintShape);
begin
  FShape.Assign(Value);
end;

end.
