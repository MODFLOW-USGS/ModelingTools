{
 ASLink Component 1.0 Copyright © 1997
        Andrey Abakumov (aga@oficina.rnd.su)

 URL Component version 1.0 Copyright © 1997
        Alexander Meeder (ameeder@dds.nl)

 RXLabel Component Copyright © 1997
        Fedor Koshevnikov  (kosh@masterbank.msk.ru)
        Igor Pavluk        (pavluk@masterbank.msk.ru)
        Serge Korolev      (korolev@masterbank.msk.ru)

ASLink is a component was assembled from RXLabel and URL
for more convenient work with Internet Link's

With this control it's easy to add www-addresses, mail-addresses etc.
behind menu-options, buttons etc.

How to use TASLink:
1. First put this control on your form
2. Set the URLType property to one of the following nine types:
   File, Ftp, Gopher, Http, Https, Mailto, News, Telnet, Wais
3. Set the property Caption to the desired address
4. Set the property URLEnabled if to use a label as Link
5. Set the property URLTypeAdd if wish to add to the
   address string of a type http: // and etc.
6. Will choose colour active Link
7. Call the TASLink.Execute method or click a label
}
unit ASLink;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI;

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TURLType = (utFile,utFtp,utGopher,utHttp,utHttps,
              utMailto,utNews,utTelnet,utWais);

  TASLink = class(TGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FShadowColor: TColor;
    FShadowSize: Byte;
    FShadowPos: TShadowPosition;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FShowFocus: Boolean;
    FFocused: Boolean;

    FURLEnabled: Boolean;
    FURLType: TURLType;
    FURLTypeAdd: Boolean;

    NormalColor: TColor;
    FLinkColor: TColor;

    Procedure SetLinkColor(Value: TColor);
    Procedure SetURLEnabled(Value: Boolean);
    Procedure SetURLType(Value: TURLType);
    Procedure SetURLTypeAdd(Value: Boolean);

    Procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    Procedure AdjustBounds;
    Procedure DoDrawText(var Rect: TRect; Flags: Word);
    Function GetTransparent: Boolean;
    Procedure SetAlignment(Value: TAlignment);
    Procedure SetAutoSize(Value: Boolean);
    Procedure SetFocusControl(Value: TWinControl);
    Procedure SetLayout(Value: TTextLayout);
    Procedure SetShadowColor(Value: TColor);
    Procedure SetShadowSize(Value: Byte);
    Procedure SetShadowPos(Value: TShadowPosition);
    Procedure SetShowAccelChar(Value: Boolean);
    Procedure SetTransparent(Value: Boolean);
    Procedure SetWordWrap(Value: Boolean);
    Procedure SetShowFocus(Value: Boolean);
    Procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    Procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    Procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    Procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;


  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure Paint; override;
  Public
    constructor Create(AOwner: TComponent); override;
    Property Canvas;
    Procedure Loaded; override;
    Procedure Click; override;
    Function Execute:Boolean;

  Published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnHighlight;
    property ShadowSize: Byte read FShadowSize write SetShadowSize default 1;
    property ShadowPos: TShadowPosition read FShadowPos write SetShadowPos default spLeftTop;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property LinkColor: TColor
             read FLinkColor
             write SetLinkColor
             default clBlue;

    property URLEnabled: Boolean
             read FURLEnabled
             write SetURLEnabled
             default True;

    property URLTypeAdd: Boolean
             read FURLTypeAdd
             write SetURLTypeAdd
             default True;

    property URLType: TURLType
             read FURLType
             write SetURLType
             default utMailTo;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

Procedure Register;

implementation

Function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var
  chFileName,
  chParams,
  chDir: array[0..79] of Char;
begin
Result:=ShellExecute(Application.Handle, nil, StrPCopy(chFileName, FileName),
            StrPCopy(chParams,Params), StrPCopy(chDir, DefaultDir), ShowCmd);
end;

Function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;
var
  RText, RShadow: TRect;
  Color: TColorRef;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(DC, ShadowColor);
  case ShadowPos of
    spLeftTop: OffsetRect(RText, ShadowSize, ShadowSize);
    spRightBottom: OffsetRect(RShadow, ShadowSize, ShadowSize);
    spLeftBottom:
      begin
        OffsetRect(RText, ShadowSize, 0);
        OffsetRect(RShadow, 0, ShadowSize);
      end;
    spRightTop:
      begin
        OffsetRect(RText, 0, ShadowSize);
        OffsetRect(RShadow, ShadowSize, 0);
      end;
  end; { case }
  Result := DrawText(DC, Str, Count, RShadow, Format);
  if Result > 0 then Inc(Result, ShadowSize);
  SetTextColor(DC, Color);
  DrawText(DC, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

constructor TASLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 1;
  FShadowPos := spLeftTop;

  FLinkColor := clBlue;
  FURLEnabled:=True;
  FURLTypeAdd:=True;
  FURLType := utMailTo;

  NormalColor := Font.Color;
  Transparent := True;
  Cursor := crHandPoint;
end;

Procedure TASLink.DoDrawText(var Rect: TRect; Flags: Word);
var
{$IFDEF WIN32}
  Text: string;
{$ELSE}
  Text: array[0..255] of Char;
{$ENDIF}
  PosShadow: TShadowPosition;
  SizeShadow: Byte;
  ColorShadow: TColor;
begin
{$IFDEF WIN32}
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
{$ELSE}
  StrPLCopy(Text, Caption, 255);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or FShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then StrCopy(Text, ' ');
{$ENDIF}
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Canvas.Font := Font;
  PosShadow := FShadowPos;
  SizeShadow := FShadowSize;
  ColorShadow := FShadowColor;
  if not Enabled then begin
    if (FShadowSize = 0) and NewStyleControls then begin
      PosShadow := spRightBottom;
      SizeShadow := 1;
    end;
    Canvas.Font.Color := clGrayText;
    ColorShadow := clBtnHighlight;
  end;
{$IFDEF WIN32}
  DrawShadowText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
{$ELSE}
  DrawShadowText(Canvas.Handle, Text, StrLen(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
{$ENDIF}
end;

Procedure TASLink.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect: TRect;
  DrawStyle: Integer;
{$IFDEF WIN32}
  P: TPoint;
{$ENDIF}
begin
  with Canvas do begin
    if not Transparent then begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    InflateRect(Rect, -1, 0);
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> tlTop then begin
      DoDrawText(Rect, DrawStyle or DT_CALCRECT);
      Rect.Left := ClientRect.Left;
      Rect.Right := ClientRect.Right;
      if FLayout = tlBottom then OffsetRect(Rect, 0, Height - Rect.Bottom)
      else OffsetRect(Rect, 0, (Height - Rect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
    if FShowFocus and Assigned(FFocusControl) and FFocusControl.Focused and
      not (csDesigning in ComponentState) then
    begin
      InflateRect(Rect, 1, 0);
{$IFDEF WIN32}
      P := ClientToScreen(Rect.TopLeft);
      Rect := Bounds(P.X, P.Y, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
//      DrawInvertFrame(Rect, 1);
{$ELSE}
      DrawFocusRect(Rect);
{$ENDIF}
    end;
  end;
end;

Procedure TASLink.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if FAutoSize then begin
    Rect := ClientRect;
    InflateRect(Rect, -1, 0);
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if FAlignment = taRightJustify then Inc(X, Width - Rect.Right);
    InflateRect(Rect, 1, 0);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

Procedure TASLink.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

Procedure TASLink.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

Procedure TASLink.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Invalidate;
  end;
end;

Procedure TASLink.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

Procedure TASLink.SetShadowSize(Value: Byte);
begin
  if Value <> FShadowSize then begin
    FShadowSize := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

Procedure TASLink.SetShadowPos(Value: TShadowPosition);
begin
  if Value <> FShadowPos then begin
    FShadowPos := Value;
    Invalidate;
  end;
end;

function TASLink.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

Procedure TASLink.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  if FShowFocus then Invalidate;
end;

Procedure TASLink.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

Procedure TASLink.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then begin
    if Value then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

Procedure TASLink.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

Procedure TASLink.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    AdjustBounds;
  end;
end;

Procedure TASLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FocusControl := nil;
end;

Procedure TASLink.CMFocusChanged(var Message: TCMFocusChanged);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (Message.Sender = FFocusControl);
  if FFocused <> Active then begin
    FFocused := Active;
    if FShowFocus then Invalidate;
  end;
  inherited;
end;

Procedure TASLink.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

Procedure TASLink.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

Procedure TASLink.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then begin
        SetFocus;
        Message.Result := 1;
      end;
end;

Procedure TASLink.SetLinkColor(Value: TColor);
begin
  If Value <> FLinkColor Then
  begin
    FLinkColor := Value;
  end;
end;

Procedure TASLink.SetURLEnabled(Value:Boolean);
begin
 If Value <> FURLEnabled Then
  begin
    FURLEnabled:= Value;
  end;
end;

Procedure TASLink.SetURLTypeAdd(Value:Boolean);
begin
 If Value <> FURLTypeAdd Then
  begin
    FURLTypeAdd:= Value;
  end;
end;

Procedure TASLink.CMMouseEnter(var Message: TMessage);
begin
 NormalColor := Font.Color;
 Font.Color := FLinkColor;
 Invalidate;
end;

Procedure TASLink.CMMouseLeave(var Message: TMessage);
begin
 Font.Color := NormalColor;
 Invalidate;
end;

Procedure TASLink.Loaded;
begin
  inherited Loaded;
end;

Function TASLink.Execute: Boolean;
var
  HyperlinkType,
  URLString: string;
begin
  result := false;
  If (Caption <> '')And(FURLEnabled)
   Then
   begin
   If FURLTypeAdd
    Then
    begin
     case FURLType of
       utFile   : HyperlinkType := 'file://';
       utFtp    : HyperlinkType := 'ftp://';
       utGopher : HyperlinkType := 'gopher://';
       utHttp   : HyperlinkType := 'http://';
       utHttps  : HyperlinkType := 'https://';
       utMailto : HyperlinkType := 'mailto:';
       utNews   : HyperlinkType := 'news:';
       utTelnet : HyperlinkType := 'telnet:';
       utWais   : HyperlinkType := 'wais:';
     end;
     URLString := HyperlinkType + Caption;
     result := (ExecuteFile(URLString, '', '', SW_SHOWNOACTIVATE) > 32);
    end
   Else
   begin
    URLString :=Caption;
    result := (ExecuteFile(URLString, '', '', SW_SHOWNOACTIVATE) > 32);
   end;
end;
end;

Procedure TASLink.Click;
begin
inherited Click;
Execute;
Invalidate;
end;

Procedure TASLink.SetURLType(Value: TURLType);
begin
 If Value <> FURLType Then
     FURLType := Value;
end;

Procedure Register;
begin     
  RegisterComponents('AbakSoft', [TASLink]);
end;

end.
