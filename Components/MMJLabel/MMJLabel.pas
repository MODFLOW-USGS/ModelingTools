unit MMJLabel;

{TMMJLable is 3D label with Angle poperty.

 Happy programming
Happy Programming

Mihaela Mihaljevic Jakic
mickj@hi.hinet.hr}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCaptionStyle=(csNormal, csRaised, csRecessed, csShadow);
  TBorderStyle=(bsBump, bsEtched);
  TButtonState=(btRaised,btPressed,btDisabled,btMouseOver);
  TMouseType =(mtNone, mtUp, mtDown, mtMoving);
  TAngle = 0..360;
  TMMJLabel = class(TLabel)
  private
    { Private declarations }
    FStyle : TCaptionStyle;
    FBorderStyle:TBorderStyle;
    FBorder :Boolean;
    FBorderLeft:Boolean;
    FBorderTop:Boolean;
    FBorderRight:Boolean;
    FBorderBottom:Boolean;
    FShadowColor : TColor;
    FDown:Boolean;
    FTrapMouse:Boolean;
    FShadowDepth : Integer;
    FOnClick :TNotifyEvent;
    FCount:cardinal;
    FEnabled : Boolean;
    FButtonUpStyle:TCaptionStyle;
    FRect :TRect;
    FAngle : TAngle;
    FButton:Boolean;
    FButtonState:TButtonState;
    FMouse:TMouseType;
    FCursorDown:TCursor;
    FCursorMoving:TCursor;
    FMoveCaptionColor:TColor;
    FCaptionColor:TColor;
    FButtonWidth : integer;
    FButtonHeight : integer;
    procedure SetStyle(Value: TCaptionStyle);
    procedure SetButtonUpStyle(Value: TCaptionStyle);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowDepth(Value : Integer);
    procedure SetButtonWidth(Value : Integer);
    procedure SetButtonHeight(Value : Integer);
    procedure SetEnabled(Value : Boolean);
    procedure SetBorder (Value:Boolean);
    procedure SetBorderTop (Value:Boolean);
    procedure SetBorderLeft (Value:Boolean);
    procedure SetBorderRight (Value:Boolean);
    procedure SetBorderBottom (Value:Boolean);
    procedure SetButton (Value:Boolean);
    procedure SetTrapMouse (Value:Boolean);
    procedure SetMoveCaptionColor(Value: TColor);
    procedure SetCaptionColor(Value: TColor);
    procedure DrawBorder(R:TRect;SColor,HColor:TColor);
    procedure DrawButton(R:TRect;SColor,HColor:TColor;btnState:TButtonState);
    procedure SetAngle (Value : TAngle);
    function  IsTrueTypeFont : Boolean;
    procedure DrawRotated(Rect:tRect;Text:String);
    function  GetRotatedRect(OldRect:TRect;Angle:Integer;Text:String):TRect;
    function GetRect(OldRect:TRect; Angle:Integer; Text:string):TRect;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseExit(var Message: TMessage); message CM_MOUSELEAVE;
    function CursorPosition : TPoint;
    property ButtonupStyle : TCaptionStyle
       read FButtonUpStyle
       write SetButtonUpStyle;
    property CaptionColor :TColor
       read FCaptionColor
       write SetCaptionColor
       default clBlack;
  protected
    { Protected declarations }
    procedure Paint;override;
    procedure Paint3D(clRect:TRect; Flags:Word; Stil:TCaptionStyle);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    { Public declarations }
        Constructor Create(AOwner:TComponent);override;
  published
    { Published declarations }
    property Style :TCaptionStyle
       read FStyle  write SetStyle
       Stored true default csNormal;
    property BorderStyle :TBorderStyle
       read FBorderStyle
       write SetBorderStyle
       Stored true
       default bsEtched;
    property ShadowColor :TColor
       read FShadowColor
       write SetShadowColor
       Stored true default clGray;
    property ShadowDepth :Integer
       read FShadowDepth
       write SetShadowDepth
       Stored true default 2;
    property ButtonWidth :Integer
       read FButtonWidth
       write SetButtonWidth;
    property ButtonHeight :Integer
       read FButtonHeight
       write SetButtonHeight;
    property Border :Boolean
       read FBorder
       write SetBorder;
    property Button :Boolean
       read FButton
       write SetButton;
    property Angle : TAngle
       read FAngle
       write SetAngle;
    property Enabled : Boolean
       read FEnabled
       write SetEnabled ;
    property TrapMouse : Boolean
       read FTrapMouse
       write SetTrapMouse ;
    property Click : TNotifyEvent
       read FOnClick  write FOnClick;
    property MoveCaptionColor :TColor
       read FMoveCaptionColor  write SetMoveCaptionColor
       Stored true default clBlue;
    property BorderLeft : Boolean
       read FBorderLeft
       write SetBorderLeft;
    property BorderTop : Boolean
       read FBorderTop
       write SetBorderTop;
    property BorderBottom : Boolean
       read FBorderBottom
       write SetBorderBottom;
    property BorderRight : Boolean
       read FBorderRight
       write SetBorderRight;
  end;
const
TextAlignments : array[ TAlignment ] of Word = ( dt_Left,
                                                   dt_Right,
                                                   dt_Center );

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mihaela''s', [TMMJLabel]);
end;

procedure TMMJLabel.SetMoveCaptionColor(Value: TColor);
begin
   if FMoveCaptionColor <> Value then begin
      FMoveCaptionColor := Value;
  end;
end;

procedure TMMJLabel.SetCaptionColor(Value: TColor);
begin
   if FCaptionColor <> Value then begin
      FCaptionColor := Value;
  end;
end;

procedure TMMJLabel.SetEnabled(Value : Boolean);
begin
   if FEnabled <> Value then
      FEnabled:=Value;
   if FButton then
      FButtonState:=btDisabled;
   Invalidate;
end;

procedure TMMJLabel.SetTrapMouse(Value : Boolean);
begin
   if FTrapMouse <> Value then
      FTrapMouse:=Value;
   if Not TrapMouse then
      CaptionColor:= Canvas.Font.Color;
   Invalidate;
end;

procedure TMMJLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Not((FButton and Enabled) or TrapMouse) then Exit;
  if (Button = mbLeft) and Enabled then begin
     Fmouse:=mtDown;
     FDown:=True;
     FButtonState:=btPressed;
     Invalidate;
  end;
end;

procedure TMMJLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Not((FButton and Enabled) or TrapMouse) then Exit;
  if FDown then begin
     FMouse:=mtUP;
     FButtonState:=btRaised;
  end;
  FDown:=False;
  Fmouse:=mtUp;
  Invalidate;
end;

procedure TMMJLabel.CMMouseEnter(var Message: TMessage);
begin
   if (FButton and Enabled) or TrapMouse then begin
       if FDown then begin
          FMouse:=mtDown;
          FButtonState:=btPressed;
          //SetCursor(FCursorDown);
       end;
       if not FDown then begin
          FMouse:=mtMoving;
          FButtonState:=btMouseOver;
         // SetCursor(FCursorMoving);
       end;
       Invalidate;
   end;
end;

function TMMJLabel.CursorPosition : TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;

procedure TMMJLabel.CMMouseExit(var Message: TMessage);
begin
   inherited;
   if (FButton and Enabled) or TrapMouse then begin
      FMouse:=mtNone;
      FButtonState:=btMouseOver;
      Invalidate;
   end;
end;

procedure TMMJLabel.Paint;
begin
   if not FButton then begin
      Paint3d(ClientRect,dt_ExpandTabs or TextAlignments[ Alignment ],FStyle);
   end;
   if FButton and FEnabled then begin
      if FButtonState = btRaised then begin
         Paint3d(ClientRect,dt_ExpandTabs or TextAlignments[ Alignment ],
                 ButtonUpStyle);
      end;

      if FButtonState = btMouseOver then begin
         Paint3d(ClientRect,dt_ExpandTabs or TextAlignments[ Alignment ],
                 ButtonUpStyle);
      end;

      if FButtonState = btPressed then begin
         Paint3d(ClientRect,dt_ExpandTabs or TextAlignments[ Alignment ],
                 csRecessed);
      end;
   end;
   if FButton and not FEnabled then begin
      Paint3d(ClientRect,dt_ExpandTabs or TextAlignments[ Alignment ],
                 FStyle);
   end;
   
   if FBorder then
      DrawBorder(ClientRect,clBtnShadow,clBtnHighlight);
   if FButton then begin
      DrawButton(ClientRect,clBtnShadow,clBtnHighlight,FButtonState);
   end;
end;

procedure TMMJLabel.SetStyle(Value: TCaptionStyle);
begin
   if FStyle <> Value then begin
      FStyle := Value;
      if FButton then
         ButtonUpStyle:=FStyle;
      Invalidate;
  end;
end;

procedure TMMJLabel.SetBorderStyle(Value: TBorderStyle);
begin
   if FBorderStyle <> Value then begin
      FBorderStyle := Value;
      if FBorder then
         Invalidate;
  end;
end;

procedure TMMJLabel.SetButtonUpStyle(Value: TCaptionStyle);
begin
   if FButtonUpStyle <> Value then begin
       FButtonUpStyle := Value;
       if FButtonUpStyle= csRecessed then
          FButtonUpStyle:=csRaised;
       if FButton then
          Invalidate;
  end;
end;

procedure TMMJLabel.SetShadowColor(Value: TColor);
 begin
   if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
 end;

 procedure TMMJLabel.SetBorder(Value: Boolean);
 begin
    if FBorder <> Value then begin
       FBorder := Value;
       if FBorder then
          FButton:=False;
     end;
     Invalidate;
 end;

procedure TMMJLabel.SetBorderTop(Value: Boolean);
begin
   if FBorderTop <> Value then begin
      FBorderTop := Value;
      if FBorder then
         Invalidate;
   end;
end;

procedure TMMJLabel.SetBorderLeft(Value: Boolean);
begin
   if FBorderLeft <> Value then begin
      FBorderLeft := Value;
      if FBorder then
         Invalidate;
   end;
end;

procedure TMMJLabel.SetBorderRight(Value: Boolean);
begin
   if FBorderRight <> Value then begin
      FBorderRight := Value;
      if FBorder then
         Invalidate;
   end;
end;

procedure TMMJLabel.SetBorderBottom(Value: Boolean);
begin
   if FBorderBottom <> Value then begin
      FBorderBottom := Value;
      if FBorder then
         Invalidate;
   end;
end;

procedure TMMJLabel.SetButton (Value:Boolean);
begin
   FButtonWidth:=Width;
   FButtonHeight:=Height;
   if FButton <> Value then begin
        FButton := Value;
        if FButton then begin
           FBorder:=False;
        end;
   end;
   if not FButton then begin
      AutoSize:=True;
      AdjustBounds;
   end;
   ButtonUpStyle:=FStyle;
   Invalidate;
end;

procedure TMMJLabel.SetShadowDepth(Value : Integer);
begin
   if FShadowDepth <> Value then begin
      FShadowDepth := Value;
      Invalidate;
   end;
end;

procedure TMMJLabel.SetButtonWidth(Value : Integer);
begin
   if FButtonWidth <> Value then begin
      FButtonWidth := Value;
      if FButtonWidth<> Width then
         AutoSize:=False;
      Width:=FButtonWidth;
      Invalidate;
   end;
end;

procedure TMMJLabel.SetButtonHeight(Value : Integer);
begin
   if FButtonHeight <> Value then begin
      FButtonHeight := Value;
      if FButtonHeight<> Height then
         AutoSize:=False;
      Height:=FButtonHeight;
      Invalidate;
   end;
end;
constructor TMMJLabel.Create;
begin
   FButton:=False;
   FDown:=False;
   FMouse:=mtNone;
   FButtonState:=btRaised;
   Fcount:=0;
   FButtonState:=btRaised;
   FBorder :=False;
   FShadowColor :=clGray;
   FShadowDepth :=2;
   FAngle :=0;
   FStyle:=csNormal;
   FCursorDown:=crArrow;
   FCursorMoving:=crHandPoint;
   FEnabled:=True;
   FMoveCaptionColor:=clGreen;
   FCaptionColor:=clBlack;
   FBorderLeft:=True;
   FBorderTop:=True;
   FBorderRight:=True;
   FBorderBottom:=True;
   FTrapMouse:=False;
   inherited Create(AOwner);
end;

procedure TMMJLabel.DrawButton(R:TRect;SColor,HColor:TColor;btnState:TButtonState);
begin
   if FButtonState=btRaised then begin
      DrawEdge(Canvas.Handle,R,EDGE_RAISED,BF_RECT);
      Exit;
   end;
   if FButtonState=btPressed then begin
      DrawEdge(Canvas.Handle,R,EDGE_SUNKEN,BF_RECT);
      //DrawFrameControl(Canvas.Handle,R,DFC_BUTTON,DFCS_PUSHED+DFCS_TRANSPARENT);
      Exit;
   end;
   if FButtonState=btMouseOver then begin
      DrawEdge(Canvas.Handle,R,EDGE_Raised,BF_RECT);
      Exit;
   end;
   if FButtonState=btDisabled then begin
      DrawEdge(Canvas.Handle,R,EDGE_BUMP,BF_RECT);
      Exit;
   end;
end;

procedure TMMJLabel.DrawBorder(R:TRect;SColor,HColor:TColor);
var
   Edge:uint;
   Flags:uint;
begin
   Flags:=0;
   if BorderStyle= bsBump then
      Edge:=EDGE_BUMP;
   if BorderStyle= bsEtched then
      Edge:=EDGE_ETCHED;
   if BorderTop then
      Flags:= Flags + BF_TOP;
   if BorderLeft then
      Flags:= Flags + BF_LEFT;
   if BorderRight then
      Flags:= Flags + BF_RIGHT;
   if BorderBottom then
      Flags:= Flags + BF_BOTTOM;

   DrawEdge(Canvas.Handle,R,Edge,Flags);
  // DrawEdge(Canvas.Handle,R,EDGE_ETCHED,BF_RECT);
end;

function TMMJLabel.IsTrueTypeFont : Boolean;
var
  FontInfo: TTextMetric;            // holds the font metric information
begin
   result:=False;
   GetTextMetrics(Canvas.Handle, FontInfo);
   with FontInfo do
      if ((tmPitchAndFamily and $0F) and TMPF_TRUETYPE) = TMPF_TRUETYPE then
         result:=True;

end;

function TMMJLabel.GetRotatedRect(OldRect:TRect;Angle:Integer;Text:String):TRect;
var
   DC      : HDC;
   hSavFont: HFont;
   Size    : TSize;
   x,y     : Integer;
   cStr    : array[0..255] of Char;
begin
  StrPCopy(cStr,Text);
  DC := GetDC(0);
  hSavFont := SelectObject(DC,Font.Handle);
  GetTextExtentPoint32(DC,cStr,Length(Text),Size);
  SelectObject  (DC,hSavFont);
  ReleaseDC(0,DC);
  if          Angle<=90  then begin             { 1.Quadrant }
     x := 0;
     y := Trunc(Size.cx * sin(Angle*Pi/180));
  end else if Angle<=180 then begin             { 2.Quadrant }
     x := Trunc(Size.cx * -cos(Angle*Pi/180));
     y := Trunc(Size.cx *  sin(Angle*Pi/180) + Size.cy * cos((180-Angle)*Pi/180));
  end else if Angle<=270 then begin             { 3.Quadrant }
     x := Trunc(Size.cx * -cos(Angle*Pi/180) + Size.cy * sin((Angle-180)*Pi/180));
     y := Trunc(Size.cy * sin((270-Angle)*Pi/180));
  end else if Angle<=360 then begin             { 4.Quadrant }
     x := Trunc(Size.cy * sin((360-Angle)*Pi/180));
     y := 0;
  end;
  oldRect.Top := oldRect.Top +y;
  oldRect.Left:= oldRect.Left+x;

  x := Abs(Trunc(Size.cx * cos(Angle*Pi/180))) + Abs(Trunc(Size.cy * sin(Angle*Pi/180)));
  y := Abs(Trunc(Size.cx * sin(Angle*Pi/180))) + Abs(Trunc(Size.cy * cos(Angle*Pi/180)));

     Width  := x;
     Height := y;
  result:=OldRect;
end;

function TMMJLabel.GetRect(OldRect:TRect; Angle:Integer; Text:string):TRect;
var
   CurrentStringSize: TSize;
   TheString :PChar;
   FontRect,WRect :TRect;
   x,y : integer;
begin
   x:=Width;
   y:=Height;
   OldRect.Top:=Trunc(OldRect.Top -(x*sin(FAngle*Pi/180)));
   OldRect.Left:=Trunc(OldRect.Left -(y*sin(FAngle*Pi/180)));
   Result:=OldRect;
end;

procedure TMMJLabel.DrawRotated(Rect:tRect;Text:String);
var
   LogFont: TLogFont;
   OldFont, NewFont: HFont;
begin
  Rect:=GetRotatedRect(Rect,FAngle,Text);
  GetObject(Canvas.Font.Handle,SizeOf(LogFont),Addr(LogFont));
  LogFont.lfEscapement := Fangle*10;
  NewFont := CreateFontIndirect(LogFont);
  OldFont := SelectObject(Canvas.Handle,NewFont);

  Canvas.TextOut(Rect.Left,Rect.Top,Text);

  NewFont := SelectObject(Canvas.Handle,OldFont);
  DeleteObject(NewFont);
end;

procedure TMMJLabel.SetAngle(Value : TAngle);
var
   OldWidth,OldHeight : integer;
begin
   if FAngle <> Value then begin
      if FAngle <> 0 then begin
         if not IsTrueTypeFont then begin
            FAngle:=0;
            raise Exception.Create('Only True Type fonts may be rotated.'+
                                   'Please select one.');
            Exit;
         end;
      end;
         FAngle:=Value;
         AutoSize:=True;
         AdjustBounds;
         Invalidate;
   end;
end;

procedure TMMJLabel.Paint3D(clRect:TRect; Flags:Word; Stil:TCaptionStyle);
var
   Text: array [0..255] of char;
   Rtmp: TRect;
   bojaL, bojaR,clCaption : TColor;
   xStart,yStart: integer;
begin
  if FCount=0 then
     CaptionColor:=Canvas.Font.Color;
  FCount:=3;
  if (FMouse= mtUp) or (FMouse=mtDown) or (FMouse=mtNone) then
     clCaption:=FCaptionColor;
  if FMouse=mtMoving then
     clCaption:=FMoveCaptionColor;

  with Canvas do begin
      if not Transparent then begin
         Brush.Color := Self.Color;
         Brush.Style := bsSolid;
         FillRect( ClientRect );
      end;
    Brush.Style := bsClear;
    StrPCopy(Text, Caption );
    if WordWrap then
      Flags := Flags or dt_WordBreak;
    if not ShowAccelChar then
      Flags := Flags or dt_NoPrefix;
      Font := Self.Font;

    if (Stil = csRecessed)then begin
        BojaL := clBtnShadow;
        BojaR := clBtnHighlight;
       //Drawin Text
       
        RTmp := clRect;
        OffsetRect( Rtmp, 1, 1 );
        Font.Color := BojaR;
        Font.Size:=Self.Font.Size;
        Font.Style:=Self.Font.Style;
        //
        if FAngle = 0 then
           DrawText( Handle, Text, -1, Rtmp, Flags )
        else
           DrawRotated(Rtmp,Text);

        RTmp := clRect;
        OffsetRect( Rtmp, -1, -1 );
        Canvas.Font.Color := BojaL;
        Font.Size:=Self.Font.Size;
        Font.Style:=Self.Font.Style;
        //;
        if FAngle = 0 then
           DrawText( Handle, Text, -1, Rtmp, Flags )
        else
           DrawRotated(Rtmp,Text);
    end;

    if (Stil =csRaised) then begin
        bojaL := clBtnHighlight;
        bojaR := clBtnShadow;

        RTmp := clRect;
        OffsetRect( Rtmp, 1, 1 );
        Font.Color := BojaR;
                Font.Size:=Self.Font.Size;
        Font.Style:=Self.Font.Style;
        //;
        if FAngle = 0 then
           DrawText( Handle, Text, -1, Rtmp, Flags )
        else
           DrawRotated(Rtmp,Text);

        RTmp := clRect;
        OffsetRect( Rtmp, -1, -1 );
        Canvas.Font.Color := BojaL;
        Font.Size:=Self.Font.Size;
        Font.Style:=Self.Font.Style;
        //;
        if FAngle = 0 then
           DrawText( Handle, text, -1, Rtmp, Flags )
        else
           DrawRotated(Rtmp,Text);
    end;

   if Stil = csShadow then begin
      Rtmp := clRect;
      OffsetRect( RTmp, FShadowDepth, FShadowDepth );
      Font.Color := FShadowColor;
      Font.Size:=Self.Font.Size;
      Font.Style:=Self.Font.Style;
      //;
        if FAngle = 0 then
           DrawText( Handle, Text, -1, RTmp, Flags )
        else
           DrawRotated(Rtmp,Text);
    end;
   if (FButton and FEnabled) or FTrapMouse then
      Font.Color:=clCaption
   else
      Font.Color := Self.Font.Color;

   Font.Size:=Self.Font.Size;
   Font.Style:=Self.Font.Style;
   if not Enabled then
      Font.Color := clGrayText;
    Font.Size:=Self.Font.Size;
    Font.Style:=Self.Font.Style;
    //;
        if FAngle = 0 then
           DrawText( Handle, Text, -1, clRect, Flags )
        else
           DrawRotated(clRect,Text);
  end;
end;

end.
