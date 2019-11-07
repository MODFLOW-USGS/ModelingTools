
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeBalloon;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, OfficeTypes, OfficeUtils;

type

{ TksoBalloonForm }

  TksoBalloonShape = (bsRect, bsRoundRect, bsBreakRect, bsEllipse, bsOval);

  TksoBalloonForm = class(TGraphicControl)
  private
    FBorderColor: TColor;
    FShape: TksoBalloonShape;
    procedure SetBorderColor(const Value: TColor);
    procedure SetShape(const Value: TksoBalloonShape);

    function GetRegion: HRgn;
    procedure DrawBalloon(Canvas: TCanvas);
  protected
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParent(AParent: TWinControl); override;
    { Paint }
    procedure Paint; override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Color;
    property Shape: TksoBalloonShape read FShape write SetShape;
  end;

{ TksoBalloonButton }

  TksoBalloonButton = class(TButton)
  private
    FCanvas: TCanvas;
    FGlyph: Pointer;
    FStyle: TButtonStyle;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FModifiedGlyph: Boolean;
    FBorderColor: TColor;
    FMouseInControl: boolean;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure GlyphChanged(Sender: TObject);
    function IsCustom: Boolean;
    function IsCustomCaption: Boolean;
    procedure SetStyle(Value: TButtonStyle);
    procedure SetKind(Value: TBitBtnKind);
    function GetKind: TBitBtnKind;
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure SetBorderColor(const Value: TColor);
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Cancel stored IsCustom;
    property Caption stored IsCustomCaption;
    property Color;
    property Constraints;
    property Default stored IsCustom;
    property Enabled;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustom;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult stored IsCustom;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs stored IsCustom default 1;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
  end;

{ TksoOfficeBalloon }

  TksoOfficeBalloon = class(TComponent)
  private
    FColor: TColor;
    FBorderColor: TColor;
    FShape: TksoBalloonShape;
    function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons): TForm;
  protected
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Messages }
    function InputBox(const ACaption, APrompt, ADefault: string): string;
    function InputQuery(const ACaption, APrompt: string;
      var Value: string): Boolean;

    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: Integer): Integer;
    function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx, X, Y: Integer): Integer;
    function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx, X, Y: Integer;
      const HelpFileName: string): Integer;
      
    procedure ShowMessage(const Msg: string);
    procedure ShowMessageFmt(const Msg: string; Params: array of const);
    procedure ShowMessagePos(const Msg: string; X, Y: Integer);
  published
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Color: TColor read FColor write FColor;
    property Shape: TksoBalloonShape read FShape write FShape;
  end;

implementation {===============================================================}

uses OfficeConsts, ImgList, ActnList, CommCtrl;

{$R OfficeBalloon.RES}

{ TksoBalloonButton data }
var
  BalloonButtonResNames: array[TBitBtnKind] of PChar = (
    nil, 'KSO_BBOK', 'KSO_BBCANCEL', 'KSO_BBHELP', 'KSO_BBYES', 'KSO_BBNO',
    'KSO_BBCLOSE', 'KSO_BBABORT', 'KSO_BBRETRY', 'KSO_BBIGNORE', 'KSO_BBALL');
  BalloonButtonCaptions: array[TBitBtnKind] of Pointer = (
    nil, @SOKButton, @SCancelButton, @SHelpButton, @SYesButton, @SNoButton,
    @SCloseButton, @SAbortButton, @SRetryButton, @SIgnoreButton,
    @SAllButton);
  BalloonButtonModalResults: array[TBitBtnKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll);

var
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');
  ButtonCaptions: array[TMsgDlgBtn] of Pointer = (
    @SMsgDlgYes, @SMsgDlgNo, @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort,
    @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAll, @SMsgDlgNoToAll, @SMsgDlgYesToAll,
    @SMsgDlgHelp);
  ButtonKinds: array[TMsgDlgBtn] of TBitBtnKind = (
    bkYes, bkNo, bkOK, bkCancel, bkAbort, bkRetry, bkIgnore, bkAll, bkNo,
    bkYes, bkHelp);

var
  ButtonWidths : array[TMsgDlgBtn] of integer;  // initialized to zero
  BalloonButtonGlyphs: array[TBitBtnKind] of TBitmap;

type

  TBForm = class(TForm)
  private
    procedure DoHelpButton(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
  published
    property Position;
  end;

{ TBForm }

procedure TBForm.DoHelpButton(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TBForm.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #27 then Close;
end;

procedure DrawShape(Canvas: TCanvas; X, Y, Width, Height: integer; Shape: TksoBalloonShape);
var
  R: integer;
begin
  with Canvas do
    case Shape of
      bsRect: Rectangle(X, Y, X+Width, Y+Height);
      bsRoundRect: RoundRect(X, Y, X+Width, Y+Height, 20, 20);
      bsBreakRect:
      begin
        Polygon([Point(X+10,Y), Point(Width-11,Y), Point(Width-1,Y+10), Point(Width-1,Height-11),
                 Point(Width-11,Height-1),Point(X+10,Height-1),Point(X,Height-11), Point(X,Y+10)]);
      end;
      bsEllipse: Ellipse(X, Y, X+Width, Y+Height);
      bsOval:
      begin
        if Width > Height then
          R := Height
        else
          R := Width;
        RoundRect(X, Y, X+Width, Y+Height, R, R);
      end;
    end;
end;

{ TksoBalloonForm }

constructor TksoBalloonForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  Color := clInfoBk;
  FBorderColor := clWindowFrame;
  FShape := bsRoundRect;
end;

destructor TksoBalloonForm.Destroy;
begin
  inherited Destroy;
end;

procedure TksoBalloonForm.DrawBalloon(Canvas: TCanvas);
begin
  DrawShape(Canvas, 0, 0, Width, Height, FShape);
end;

function TksoBalloonForm.GetRegion: HRgn;
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.Width := Width;
    B.Height := Height;
    B.Canvas.Brush.Style := bsSolid;
    B.Canvas.Brush.Color := clBlack;
    B.Canvas.Pen.Color := clBlack;
    DrawBalloon(B.Canvas);
    Result := CreateRegionFromBitmap(B, clWhite);
  finally
    B.Free;
  end;
end;

procedure TksoBalloonForm.Paint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    DrawBalloon(Canvas);
  end;
end;

procedure TksoBalloonForm.Resize;
var
  Rgn: HRgn;
begin
  inherited ;
  if csDesigning in ComponentState then Exit;
  if (Owner is TWinControl) then
    with (Owner as TWinControl) do
    begin
      Rgn := GetRegion;
      if Rgn <> 0 then
        SetWindowRgn(Handle, Rgn, true);
    end;
end;

procedure TksoBalloonForm.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TksoBalloonForm.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (AParent is TForm) then
  begin
    (AParent as TForm).BorderStyle := bsNone;
  end;
end;

procedure TksoBalloonForm.SetShape(const Value: TksoBalloonShape);
begin
  FShape := Value;
  if not (csDesigning in ComponentState) then
    Resize;
  Invalidate;
end;

function GetBalloonButtonGlyph(Kind: TBitBtnKind): TBitmap;
begin
  if BalloonButtonGlyphs[Kind] = nil then
  begin
    BalloonButtonGlyphs[Kind] := TBitmap.Create;
    BalloonButtonGlyphs[Kind].LoadFromResourceName(HInstance, BalloonButtonResNames[Kind]);
  end;
  Result := BalloonButtonGlyphs[Kind];
end;
    
type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;
    
  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;
    
  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean; TransparentColor: TColor);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; TransparentColor: TColor;
      BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
    
{ TGlyphList }
    
constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;
    
destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;
    
function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;
    
function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;
    
procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;
    
{ TGlyphCache }
    
constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;
    
destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;
    
function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;
    
procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;
    
function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;
    
var
  GlyphCache: TGlyphCache = nil;

{ TButtonGlyph }
    
constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;
    
destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;
    
procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;
    
procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
    
procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;
    
procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;
    
function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;
    
              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    
              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    
              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;
    
procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean; TransparentColor: TColor);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        TransparentColor, clNone, ILD_Normal);
end;
    
procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;
    
procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else 
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);
    
  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);
    
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;
    
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;
    
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
    
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
    
  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;
    
function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  TransparentColor: TColor; BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent, TransparentColor);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;
    
{ TksoBalloonButton }
    
constructor TksoBalloonButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FStyle := bsAutoDetect;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  ControlStyle := ControlStyle + [csReflector];

  Color := clInfoBk;
  FBorderColor := clBtnFace;
end;
    
destructor TksoBalloonButton.Destroy;
begin
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;
end;
    
procedure TksoBalloonButton.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;
    
procedure TksoBalloonButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;
    
procedure TksoBalloonButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;
    
procedure TksoBalloonButton.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then Form.Close
        else inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then Application.HelpContext(Control.HelpContext)
        else inherited Click;
      end;
    else
      inherited Click;
  end;
end;
    
procedure TksoBalloonButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;
    
procedure TksoBalloonButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TksoBalloonButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown: Boolean;
  State: TButtonState;
  R: TRect;
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.Width := Width;
    B.Height := Height;
    with DrawItemStruct do
    begin
      R := ClientRect;
      PaintBackground(Self, B.Canvas);
      CopyParentImage(Self, B.Canvas);
      { Draw face }
      IsDown := itemState and ODS_SELECTED <> 0;
      IsFocused := itemState and ODS_FOCUS <> 0;

      if not Enabled then State := bsDisabled
      else if IsDown then State := bsDown
      else State := bsUp;

      { DrawFrameControl does not draw a pressed button correctly }
      if IsDown then
      begin
        { Inner }
        B.Canvas.Pen.Style := psClear;
        B.Canvas.Brush.Style := bsSolid;
        B.Canvas.Brush.Color := clBtnFace;
        B.Canvas.RoundRect(R.left, R.top, R.right-R.left+1, R.bottom-R.top+1, 4, 4);
        { Outer }
        B.Canvas.Brush.Color := clBtnShadow;
        B.Canvas.RoundRect(R.left+1, R.top+1, R.right-R.left-1, R.bottom-R.top-1, 2, 2);
        B.Canvas.Brush.Color := clBtnHighlight;
        B.Canvas.RoundRect(R.left+2, R.top+2, R.right-R.left, R.bottom-R.top, 2, 2);
        { Face }
        B.Canvas.Pen.Style := psClear;
        B.Canvas.Brush.Style := bsSolid;
        B.Canvas.Brush.Color := Color;
        B.Canvas.RoundRect(R.left+2, R.top+2, R.right-R.left-1, R.bottom-R.top-1, 2, 2);
      end
      else
        if not FMouseInControl then
        begin
          B.Canvas.Pen.Color := FBorderColor;
          B.Canvas.Brush.Color := Color;
          B.Canvas.RoundRect(R.left, R.top, R.right-R.left, R.bottom-R.top, 6, 6);
          InflateRect(R, -1, -1);
        end
        else
        begin
          { Inner }
          B.Canvas.Pen.Style := psClear;
          B.Canvas.Brush.Style := bsSolid;
          B.Canvas.Brush.Color := clBtnFace;
          B.Canvas.RoundRect(R.left, R.top, R.right-R.left+1, R.bottom-R.top+1, 4, 4);
          { Outer }
          B.Canvas.Brush.Color := clBtnHighlight;
          B.Canvas.RoundRect(R.left+1, R.top+1, R.right-R.left-1, R.bottom-R.top-1, 2, 2);
          B.Canvas.Brush.Color := clBtnShadow;
          B.Canvas.RoundRect(R.left+2, R.top+2, R.right-R.left, R.bottom-R.top, 2, 2);
          { Face }
          B.Canvas.Pen.Style := psClear;
          B.Canvas.Brush.Style := bsSolid;
          B.Canvas.Brush.Color := Color;
          B.Canvas.RoundRect(R.left+2, R.top+2, R.right-R.left-1, R.bottom-R.top-1, 2, 2);
        end;

      if IsFocused then
      begin
        R := ClientRect;
        InflateRect(R, -1, -1);
      end;

      if IsDown then
        OffsetRect(R, 1, 1);

      B.Canvas.Font := Self.Font;

      R := TButtonGlyph(FGlyph).Draw(B.Canvas, R, Point(0,0), Caption, FLayout, FMargin,
        FSpacing, State, true, Color, DrawTextBiDiModeFlags(0));

      if IsFocused then
      begin
        InflateRect(R, 2, 2);
        B.Canvas.Pen.Color := clWindowFrame;
        B.Canvas.Brush.Color := clBtnFace;
        DrawFocusRect(B.Canvas.Handle, R);
      end;

      FCanvas.Handle := hDC;
      FCanvas.Draw(0, 0, B);
      FCanvas.Handle := 0;
    end;
  finally
    B.Free;
  end;
end;

procedure TksoBalloonButton.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TksoBalloonButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TksoBalloonButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
    
procedure TksoBalloonButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;
    
function TksoBalloonButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;
    
procedure TksoBalloonButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value as TBitmap;
  FModifiedGlyph := True;
  Invalidate;
end;
    
function TksoBalloonButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;
    
procedure TksoBalloonButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;
    
function TksoBalloonButton.IsCustom: Boolean;
begin
  Result := Kind = bkCustom;
end;
    
procedure TksoBalloonButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;
    
procedure TksoBalloonButton.SetKind(Value: TBitBtnKind);
begin
  if Value <> FKind then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];
    
      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if BalloonButtonCaptions[Value] <> nil then
          Caption := LoadResString(BalloonButtonCaptions[Value]);
      end;
    
      ModalResult := BalloonButtonModalResults[Value];
      TButtonGlyph(FGlyph).Glyph := GetBalloonButtonGlyph(Value);
      NumGlyphs := 2;
      FModifiedGlyph := False;
    end;
    FKind := Value;
    Invalidate;
  end;
end;
    
function TksoBalloonButton.IsCustomCaption: Boolean;
begin
  Result := AnsiCompareStr(Caption, LoadResString(BalloonButtonCaptions[FKind])) <> 0;
end;
    
function TksoBalloonButton.GetKind: TBitBtnKind;
begin
  if FKind <> bkCustom then
    if ((FKind in [bkOK, bkYes]) xor Default) or
      ((FKind in [bkCancel, bkNo]) xor Cancel) or
      (ModalResult <> BalloonButtonModalResults[FKind]) or
      FModifiedGlyph then
      FKind := bkCustom;
  Result := FKind;
end;
    
procedure TksoBalloonButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;
    
function TksoBalloonButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;
    
procedure TksoBalloonButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;
    
procedure TksoBalloonButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;
    
procedure TksoBalloonButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TksoBalloonButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure DestroyLocals; far;
var
  I: TBitBtnKind;
begin
  for I := Low(TBitBtnKind) to High(TBitBtnKind) do
    BalloonButtonGlyphs[I].Free;
end;
    
procedure TksoBalloonButton.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TksoBalloonButton.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := true;
  Invalidate;
end;

procedure TksoBalloonButton.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := false;
  Invalidate;
end;

{ TksoOfficeBalloon }

constructor TksoOfficeBalloon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FBorderColor := clWindowFrame;
  FShape := bsRoundRect;
end;

destructor TksoOfficeBalloon.Destroy;
begin
  inherited Destroy;
end;

function Max(I, J: Integer): Integer;
begin
  if I > J then Result := I else Result := J;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function TksoOfficeBalloon.CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm;
const
  mcHorzMargin = 30;
  mcVertMargin = 15;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B, DefaultButton, CancelButton: TMsgDlgBtn;
  IconID: PChar;
  TextRect: TRect;
begin
  Result := TBForm.CreateNew(Application);
  with Result do
  begin
    BorderStyle := bsNone;
    Position := poScreenCenter;
    KeyPreview := true;
    { Add balloon }
    with TksoBalloonForm.Create(Result) do
    begin
      Parent := Result;
      Shape := Self.FShape;
      BorderColor := Self.FBorderColor;
      Color := Self.Color;
    end;
    { Standard }
    BiDiMode := Application.BiDiMode;
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        if ButtonWidths[B] = 0 then
        begin
          TextRect := Rect(0,0,0,0);
          Windows.DrawText( canvas.handle,
            PChar(LoadResString(ButtonCaptions[B])), -1,
            TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
            DrawTextBiDiModeFlagsReadingOnly);
          with TextRect do ButtonWidths[B] := Right - Left + 8;
        end;
        if ButtonWidths[B] > ButtonWidth then
          ButtonWidth := ButtonWidths[B];
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg)+1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
    IconID := IconIDs[DlgType];
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := LoadResString(Captions[DlgType]) else
      Caption := Application.Title;
    if IconID <> nil then
      with TImage.Create(Result) do
      begin
        Name := 'Image';
        Parent := Result;
        Picture.Icon.Handle := LoadIcon(0, IconID);
        SetBounds(HorzMargin, VertMargin, 32, 32);
      end;
    with TLabel.Create(Result) do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Transparent := true;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Right, TextRect.Bottom);
    end;
    if mbOk in Buttons then DefaultButton := mbOk else
      if mbYes in Buttons then DefaultButton := mbYes else
        DefaultButton := mbRetry;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
        with TksoBalloonButton.Create(Result) do
        begin
          BorderColor := Self.FBorderColor;
          Color := Self.Color;

          Name := ButtonNames[B];
          Parent := Result;
          Caption := LoadResString(ButtonCaptions[B]);
          Kind := ButtonKinds[B];
          if B = DefaultButton then Default := True;
          if B = CancelButton then Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TBForm(Result).DoHelpButton;
        end;
  end;
end;

function TksoOfficeBalloon.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := MessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, -1, -1, '');
end;

function TksoOfficeBalloon.MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
  Result := MessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, X, Y, '');
end;

function TksoOfficeBalloon.MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  begin
    try
      HelpContext := HelpCtx;
      HelpFile := HelpFileName;
      if (Y < 0) and (X < 0) then
        Position := poScreenCenter
      else
        Position := poDefault;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TksoOfficeBalloon.ShowMessage(const Msg: string);
begin
  ShowMessagePos(Msg, -1, -1);
end;

procedure TksoOfficeBalloon.ShowMessageFmt(const Msg: string; Params: array of const);
begin
  ShowMessage(Format(Msg, Params));
end;

procedure TksoOfficeBalloon.ShowMessagePos(const Msg: string; X, Y: Integer);
begin
  MessageDlgPos(Msg, mtCustom, [mbOK], 0, X, Y);
end;

{ Input dialog }

function TksoOfficeBalloon.InputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TBForm.CreateNew(Application);
  with Form do
    try
      BorderStyle := bsNone;
      Position := poScreenCenter;
      KeyPreview := true;
      { Add balloon }
      with TksoBalloonForm.Create(Form) do
      begin
        Parent := Form;
        Shape := Self.FShape;
        BorderColor := Self.FBorderColor;
        Color := Self.Color;
      end;
      { Standard }
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4)+30;
      ClientHeight := MulDiv(63, DialogUnits.Y, 8)+20;
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Transparent := true;
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4) + 15;
        Top := MulDiv(8, DialogUnits.Y, 8) + 10;
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8) + 10;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TksoBalloonButton.Create(Form) do
      begin
        BorderColor := Self.FBorderColor;
        Color := Self.Color;

        Parent := Form;
        Caption := SMsgDlgOK;
        Kind := bkOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4)+15, ButtonTop+10, ButtonWidth,
          ButtonHeight);
      end;
      with TksoBalloonButton.Create(Form) do
      begin
        BorderColor := Self.FBorderColor;
        Color := Self.Color;

        Parent := Form;
        Caption := SMsgDlgCancel;
        Kind := bkCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4)+15, ButtonTop+10, ButtonWidth,
          ButtonHeight);
        Left := Left + 15;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function TksoOfficeBalloon.InputBox(const ACaption, APrompt, ADefault: string): string;
begin
  Result := ADefault;
  InputQuery(ACaption, APrompt, Result);
end;

initialization
  FillChar(BalloonButtonGlyphs, SizeOf(BalloonButtonGlyphs), 0);
finalization
  DestroyLocals;
end.


