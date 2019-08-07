unit FusedLabel;
{
 This component is a part of the freeware Fusion Package, containing
 a set of visual components. If redistributed, this unit should not be
 separated from the rest of the files in the package. If modified,
 all author credits must remain. Latest version of the package, bug
 reports, info on authors etc. can be found at
 http://www.geocities.com/SiliconValley/Drive/6381/
 Terms of use and other stuff explained in Fusion.hlp and FusionPack.txt.

 Component info:

 TFusedLabel introduces the following

  - 3D capabilities created by unknown author
  - AutoFitting by Louis Louw louw@gcs.co.za, www.gcs.co.za/mbs/mbs.htm
  - OnMouseOut event added by Durrin Hynes-Christensen: dxh@gv.dk
  - Starting browser or mailer for specified url
           by Martin Djernæs, djernaes@metronet.de, http://einstein.ot.dk/~djernaes
  - Button behaviour, color changing on mouse enter and formatting
           by Milos Dragovic,
  - Idea for button behaviour from AISSSOFT@AOL.Com http://members.xoom.com/WAISS

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
And now, some info from the authors: (first, TEZLabel)
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

Ever got tired of just how difficult it is to make those labels look
IMPRESIVE: First you have to select a cool font, which by the way must
be a default font so that you can be sure it is on your user's computer.
Then you must select a color and then you must try to guess the right
size of the font to best fit into the frame! And then still you have a
dull, 2D looking label!

All this is a thing of the past with new EZLabel. Just drop it on and
Double-click on it to bring up the easy to use component editor. Choose
the 3d-style,  color, font, caption, how it must fit into the frame, etc.
in one step! And your done! As easy as that - and you have your IMPRESIVE
looking labels.

Some tips:
· Try to use "True Type fonts" as when their size changes they don'r
  become all pixelated.
· After the label has resized to fit the frame, set the AFitStyle to
  "NormalFit" so that is does not calculate the size every time the form
  is created/shown.

It is absolutely free so we expect nothing from you, so don't you expect
something from us.

MindBlast Software
louw@gcs.co.za
www.gcs.co.za/mbs/mbs.htm

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
The info from the author of the TmdURLLabel:
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

-----------------------------------------------------------------
This package is in version 1.2 and given free the 5th november 1997.

Latest version will always be avalible at :
http://einstein.ot.dk/~djernaes

The Package gives one class (one components) to use within
the Delphi 2.0 enviroment (eventually also 3.0 and C++ Builder,
but that is not testet).

-----------------------------------------------------------------
Components :
  TmdURLLabel :

  A web link component. When LabelType is set to AUTO, it makes
  it self look like a normal label, when no internet transport
  is avalible. When a internet transport (web browser for URL and
  MAPI for e-mail's) the label looks like a URL in your web browser.

  The check for a web browser is done by testing if any program
  is connected to both the .htm and .html extensions.
  The check for a e-mail program, is done by checking if the MAPI
  dll is reachable.

-----------------------------------------------------------------
Note :
  I have only testet with Netscape as web browser, but it should
  work with any Windows 95 / NT (TM) web browser.
  (I'm told it works with IE4.0)

  The MAPI is check is not really testet, because I only have
  computers with MAPI installed.

  If a MAPI program (like Microsoft Messaging / Exchange) is
  installed without support for e-mail, the label will still
  expect that it can send a e-mail (via MAPI). It does only know
  if MAPI is there, not if it can send e-mail via the internet.

-----------------------------------------------------------------
Legal issues :
Copyright © 1997 by Martin Djernæs <djernaes@metronet.de>

This software is provided as it is, without any kind of warranty
given. The author can not be held responsible for any kind of
damage, problems etc. from using this product.

You may use this software in any kind of development, including
comercial, and redistribute it freely, under the following
restrictions:

1. The origin of this software may not be mispresented, you must
   not claim that you wrote the original software. If you use
   this software in any kind of product, it would be appreciated
   that in a information box, or in the documentation would be
   an acknowledgmnent like this
          Parts copyright © 1997 by Martin Djernæs

2. You may not have any income from distributing this source
   to other developers. When you use this product in a comercial
   package, the source may not be charged seperatly.

3. This notice may not be removed from the source, when distributing
   such. When distributing a comercial package, where the source
   also is avalible, this notice should also follow the package, even
   you choose not to make my source avalible.

                                                               - MD97
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, extctrls, Buttons, MAPI, Registry, ShellApi;

type
  T3DEffect = (Normal3d, Resit3d, Raised3d, Shadowed3d);
  TFitType = (NormalFit, BestFitVert, BestFithorz, BestFitBoth);

  TmdLabelType = (Auto, Passive, Link);
  TmdLinkType = (http, mailto);

  TFusedLabel = class(TLabel)
  private
    procedure setStyleEffect(Value : T3DEffect);
    procedure SetShadowColor(Value:TColor);
    procedure SetWhiteColor(Value:TColor);
    procedure DoDrawText(var Rect: TRect; Flags: Word);
    procedure SetFhOffSet(value: integer);
    procedure SetFvOffSet(value: integer);
    procedure SetShadeLT(value: boolean);
    procedure SetFitType(Value: TFitType);
    procedure WMMouseMove(var msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure DoMouseOut(msg:TWMMouseMove);
    procedure DoMouseMove(msg:TWMMouseMove);
    procedure UpdateDesigner;
  protected
    FOldWidth  : Integer;
    FOldHeight : Integer;
    FOldSize   : Integer;
    MouseOut:TNotifyEvent;
    MouseMove:TNotifyEvent;
    FFitType : TFitType;
    F3DEffect : T3DEffect;
    FShadowColor : TColor;
    FWhiteColor : TColor;
    FLast : TColor;
    FhOffSet,FvOffSet : integer;
    FShadeLTSet : boolean;

    FURLCursor : TCursor;
    FURL : String;
    FLabelType : TmdLabelType;
    FLinkType : TmdLinkType;
    FLinkAble : Boolean;
    FURLAsHint : Boolean;

    Pushed, FButtonLike : boolean;
    OldColor, FEnterColor : TColor;
    Old3D : T3DEffect;
    FFormatString : string;
    procedure Paint; override;

    Procedure SetURL(Value : String);
    Procedure SetLinkType(Value : TmdLinkType);
    Procedure SetURLAsHint(Value : Boolean);
    Procedure SetHint(Value : TCaption);
    Function GetHint : TCaption;
    Procedure CheckLinkAble;
    Procedure UpdateHint;
    Procedure Click; Override;
    Property LinkAble : Boolean Read FLinkAble;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetFormatString (f : string);
  public
    procedure DoEdit;
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Caption;
    Property AFitType : TFitType read FFitType write SetFitType;

    property AStyle3D: T3DEffect read F3DEffect write setStyleEffect default Normal3d;
    property AShadeRightBottom: TColor read FShadowColor write SetShadowColor default clGray;
    property AShadeLeftTop: TColor read FWhiteColor write SetWhiteColor default clWhite;
    property AHShadeOffSet: integer read FhOffSet write SetFhOffSet default 5;
    property AVShadeOffSet: integer read FvOffSet write SetFvOffSet default -5;
    property AShadeLTSet: boolean read FShadeLTSet write setShadeLT default true;

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
    property Transparent;
    property Width;
    property Top;
    property Left;
    property Height;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove :TNotifyEvent read MouseMove write MouseMove;
    property OnMouseUp;
    property OnMouseOut:TNotifyEvent read MouseOut write MouseOut;

    Property URL : String Read FURL Write SetURL;
    Property URLAsHint : Boolean Read FURLAsHint Write SetURLAsHint;
    Property LinkType : TmdLinkType Read FLinkType Write SetLinkType;

    property ButtonLike : boolean read FButtonLike write FButtonLike;
    property EnterColor : TColor read FEnterColor write FEnterColor;
    property FormatString : string read FFormatString write SetFormatString;
  end;

  TLabelEditorDlg = class(TForm)
    E3dStyle: TRadioGroup;
    EFitType: TRadioGroup;
    FontD: TFontDialog;
    btFont: TButton;
    Panel: TPanel;
    ECaption: TEdit;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ColorD: TColorDialog;
    btColor: TButton;
    Button1: TButton;
    cTrans: TCheckBox;

    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EFitTypeClick(Sender: TObject);
    procedure E3dStyleClick(Sender: TObject);
    procedure btFontClick(Sender: TObject);
    procedure ECaptionChange(Sender: TObject);
    procedure btColorClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cTransClick(Sender: TObject);

  private
    EZLabel : TFusedLabel;
  public
  end;


//  { Component editor - brings up Label editor when double clicking on
//    Labels property }
//  TLabelEditor = class(TDefaultEditor)
//  protected
//  public
//    procedure Edit; override;
//    procedure ExecuteVerb(Index: Integer); override;
//    function GetVerb(Index: Integer): string; override;
//    function GetVerbCount: Integer; override;
//  end;

Function GetProgramPathFromExt(Const Ext : String) : String;
procedure Register;

implementation
{$R EZLabel.DFM}
{$R *.res}

// Pick a program from the registry associated with a extension
Function GetProgramPathFromExt(Const Ext : String) : String;
Var
  S : String;
Begin
  Result := '';
  With TRegistry.Create do
  Try
    RootKey := HKEY_CLASSES_ROOT;

    If OpenKey('\'+Ext,False) Then
    Begin
      S := ReadString('');
      If S <> '' Then
      Begin
        If OpenKey('\'+S+'\shell\open\command',False) Then
        Begin
          S := ReadString('');
          If S <> '' Then
            Result := S;
        end;
      end
      else
      Begin
        If OpenKey('\'+Ext+'\shell\open\command',False) Then
        Begin
          S := ReadString('');
          If S <> '' Then
            Result := S;
        end;
      end;
    end;
  Finally
    Free;
  end;
end;


{ TLabelEditorDlg }

procedure TFusedLabel.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

{ TLabelsProperty }

procedure TFusedLabel.DoEdit;
var
  LabelEditor: TLabelEditorDlg;
begin
  LabelEditor := TLabelEditorDlg.Create(Application);
  try
    LabelEditor.EZLabel.Font.Assign(Font);
    LabelEditor.EZLabel.Caption:=Caption;
    LabelEditor.EZLabel.Color:=Color;
    LabelEditor.EZLabel.Transparent:=Transparent;
    LabelEditor.EZLabel.AlignMent:=AlignMent;
    LabelEditor.EZLabel.AStyle3D:=AStyle3D;
    LabelEditor.EZLabel.AFitType:=AFitType;
    LabelEditor.ECaption.Text:=Caption;
    LabelEditor.cTrans.Checked:=Transparent;

    With LabelEditor do
    Case LabelEditor.EZLabel.AFitType of
     BestFitBoth : EFitType.ItemIndex:=0;
     BestFitHorz : EFitType.ItemIndex:=1;
     BestFitVert : EFitType.ItemIndex:=2;
     NormalFit   : EFitType.ItemIndex:=3;
    end;

    With LabelEditor do
    Case LabelEditor.EZLabel.AStyle3D of
     Normal3d   : E3dStyle.ItemIndex:=0;
     Resit3d    : E3dStyle.ItemIndex:=1;
     Raised3d   : E3dStyle.ItemIndex:=2;
     Shadowed3d : E3dStyle.ItemIndex:=3;
    End;

    LabelEditor.ShowModal;
    IF LabelEditor.modalResult = mrOK then
     Begin
      Caption:=LabelEditor.EZLabel.Caption;
      Color:=LabelEditor.EZLabel.Color;
      Transparent:=LabelEditor.EZLabel.Transparent;
      Font.Assign(LabelEditor.EZLabel.Font);
      Alignment:=LabelEditor.EZLabel.AlignMent;
      AStyle3d:=LabelEditor.EZLabel.AStyle3D;
      AFitType:=LabelEditor.EZLabel.AFitType;
      UpdateDesigner;
     End;
     Invalidate;

  finally
    LabelEditor.Free;
  end;
end;



////Component
constructor TFusedLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transparent := True;
  ParentColor := False;
  FShadowColor := clGray;
  FWhiteColor := clWhite;
  FhOffSet := 5;
  FvOffSet := -5;
  FLast := clWhite;
  Autosize:=False;
  FFitType:=BestFitBoth;
  FOldSize:=Canvas.Font.Size;
  FOldWidth:=Width;
  FOldHeight:=Height;

  CheckLinkAble;
  // don't show accelerator char (underline a char)
  ShowAccelChar := False;
  // Use the Hint property as full URL notify
  FURLAsHint := True;

  Pushed := false;
end;

Procedure TFusedLabel.SetURLAsHint(Value : Boolean);
Begin
  If FURLAsHint = Value Then
    Exit;
  FURLAsHint := Value;
  UpdateHint;                    // Update the hint values
end;

Procedure TFusedLabel.UpdateHint;
Begin
  If URLAsHint Then              // If we use URL as hint
    Inherited Hint := FURL // copy URL caption
  else
    Inherited Hint := '';        // delete it...
end;

Procedure TFusedLabel.SetHint(Value : TCaption);
Begin
  Inherited Hint := Value;
end;

Function TFusedLabel.GetHint : TCaption;
Begin
  FURLAsHint := False;        // remove the property which "allows" the
                              // the hint to be used as URL notify event!
  Result := Inherited Hint;
end;
Procedure TFusedLabel.SetURL(Value : String);
Var
  S : String;
Begin
  If FURL = Value Then Exit;
  If Pos('@',Value) <> 0 Then  // can only be a e-mail
    FLinkType := mailto;
  If Pos('/',Value) <> 0 Then  // can only be a URL
    FLinkType := http;
  S := LowerCase(Copy(Value,1,7));
  If (S = 'mailto:') OR (S = 'http://') Then
      FURL := Copy(Value,8,Length(Value))
  else
      FURL := Value;
  if FURLAsHint then
     Hint := FURL;
  if FURL<>'' then Cursor := crHandPoint
  else Cursor := crDefault;
end;

Procedure TFusedLabel.SetLinkType(Value : TmdLinkType);
Begin
  If FLinkType = Value Then
    Exit;
  FLinkType := Value;
  CheckLinkAble;
end;

Procedure TFusedLabel.CheckLinkAble;
Var
  AModule : HModule;
Begin
  Case FLinkType of
    // If the .html and the .htm extension is assigned to
    // a program
    http : FLinkAble := (GetProgramPathFromExt('.html') <> '') AND
                        (GetProgramPathFromExt('.htm') <> '');
    // Check it the MAPI dll is there
    mailto :
    Begin
      AModule := LoadLibrary(PChar(MAPIDLL));
      FLinkAble := AModule > 32;
      IF FLinkAble Then
        FreeLibrary(AModule);
    end;
  end;
end;

Procedure TFusedLabel.Click;
Var
  Param : AnsiString;
Begin
  Inherited Click;
  if FURL='' then exit;
  If (FLabelType = Link) OR
     (FLinkAble AND (FLabelType = Auto)) Then
  Begin
    Case FLinkType of
      http   : Param := 'http://'+URL;
      mailto : Param := 'mailto:'+URL;
    end;
    // Execute the default web browser on the
    // web page or the mailto window
    ShellExecute(0,
                 'open',
                 PAnsiChar(Param),
                 NIL,
                 NIL,
                 SW_SHOWNORMAL);
  end;
end;

procedure TFusedLabel.DoDrawText( var Rect : TRect; Flags : Word );
  var Text, Text2 : PChar;  Size: Byte;  TmpRect    : TRect;  UpperColor : TColor;
    LowerColor : TColor; pom : string;i : integer;
//    AddMe      : Integer;
  begin
  if FFormatString<>'' then
      begin
        GetMem(Text2, 255);
        StrLFmt (Text2, 255, PChar(FormatString), [Caption]);
        Size := StrLen(Text2);
        GetMem(Text, Size+1);
        Strcopy (Text, Text2);
        FreeMem(Text2, 255);
      end
    else
      begin
        Size := GetTextLen+1;  {Get length of caption,add room for null character}
        GetMem(Text, Size);
        GetTextBuf(Text, Size);           {Creates Buffer dynamic variable}
      end;
//  Size := GetTextLen+1;  {Get length of string in Edit1}{Add room for null character}
//  GetMem(Text, Size);
//  GetTextBuf(Text, Size);           {Creates Buffer dynamic variable}
    if ( Flags and DT_CALCRECT <> 0) and
       ( ( Text[0] = #0 ) or ShowAccelChar and
         ( Text[0] = '&' ) and
         ( Text[1] = #0 ) ) then
      StrCopy(Text, ' ');

    if not ShowAccelChar then
      Flags := Flags or DT_NOPREFIX;
    Canvas.Font := Font;

    if F3DEffect = Resit3d then
    begin
      UpperColor := FShadowColor;
      LowerColor := FWhiteColor;
    end
    else
    begin
      UpperColor := FWhiteColor;
      LowerColor := FShadowColor;
    end;

    if F3DEffect in [ Resit3d, Raised3d ] then
    begin
      TmpRect := Rect;
      OffsetRect( TmpRect, 1, 1 );
      Canvas.Font.Color := LowerColor;
      DrawText(Canvas.Handle, Text, StrLen(Text), TmpRect, Flags);

      TmpRect := Rect;
      OffsetRect( TmpRect, -1, -1 );
      Canvas.Font.Color := UpperColor;
      DrawText(Canvas.Handle, Text, StrLen(Text), TmpRect, Flags);
    end
    else if F3DEffect = Shadowed3d then
    begin
      TmpRect := Rect;
      OffsetRect( TmpRect, FhOffSet, FvOffSet );
      Canvas.Font.Color := LowerColor;
      DrawText(Canvas.Handle, Text, StrLen(Text), TmpRect, Flags);
    end;

    Canvas.Font.Color := Font.Color;

   If Not AutoSize then
   IF (FOldSize<>Canvas.Font.Size) or (FOldWidth<>Width) or (FOldHeight<>Height) then
    Begin
     Case FFitType of
       BestFitBoth : Begin
                       Canvas.Font.Size:=0;
                     While (abs(Canvas.font.height) < Rect.Bottom) and (Canvas.textwidth(String(text)) < Rect.Right) do
                      canvas.font.size:=canvas.font.size+1;
                      canvas.font.size:=canvas.font.size-2;
                      Font.Size:=Canvas.Font.Size;
                 End;
       BestFitVert : Begin
                       Font.Height:=Rect.Bottom;
                     End;
       BestFitHorz : Begin
                     Canvas.Font.Size:=0;
                     While (Canvas.textwidth(String(text)) < Rect.Right) do
                      canvas.font.size:=canvas.font.size+1;
                      canvas.font.size:=canvas.font.size-2;
                      Font.Size:=Canvas.Font.Size;
                 End;

     End;
     FOldSize:=Canvas.Font.Size;
     FOldWidth:=Width;
     FOldHeight:=Height;
    end;

    if not Enabled then
      Canvas.Font.Color := clGrayText;
    DrawText(Canvas.Handle, Text, StrLen(Text), Rect, Flags);
    //canvas.rectangle(rect.left,rect.top,rect.right,rect.bottom);
    FreeMem(Text, Size);
  end;


  procedure TFusedLabel.Paint;
  const
    Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  var
    Rect: TRect;
  begin
    with Canvas do
    begin
      if not Transparent then
      begin
        Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(ClientRect);
      end;
      Brush.Style := bsClear;
      Rect := ClientRect;
      DoDrawText( Rect, ( DT_EXPANDTABS or DT_WORDBREAK ) or
                  Alignments[ Alignment ] );
    end;
  end;

procedure TFusedLabel.SetShadowColor(value: TColor);
begin
  if not (FShadowColor = value) then
  begin
     FShadowColor := value;
     invalidate;
   end;
 end;

Procedure TFusedLabel.SetFitType(Value : TFitType);
Begin
 If Not (FFitType=Value) then
  Begin
    FFitType:=Value;
    FOldSize:=FOldSize+1;
    invalidate;
  end;
End;

procedure TFusedLabel.SetFhOffSet(value: integer);
begin
  if value<>FhOffSet then
  begin
    FhOffSet := value;
    invalidate;
  end;
end;

procedure TFusedLabel.SetFvOffSet(value: integer);
begin
  if value<>FvOffSet then
  begin
    FvOffSet := value;
    invalidate;
  end;
end;

procedure TFusedLabel.SetWhiteColor(value: TColor);
begin
  if not (FWhiteColor = value) and (FShadeLTSet=false) then
  begin
     FWhiteColor := value;
     invalidate;
   end;
 end;

 procedure TFusedLabel.setStyleEffect(value: T3DEffect);
 begin
   if F3DEffect <> value then
   begin
     F3DEffect := value;
     invalidate;
   end;
 end;

procedure TFusedLabel.SetShadeLT(value: boolean);
begin
  if  FShadeLTSet <> value then
  begin
    FShadeLTSet := value;
    if FShadeLTSet = true then
    begin
      FLast := FWhiteColor;
      FWhiteColor := clWhite;
    end
    else
      FWhiteColor := Flast;
   end;
  invalidate;
end;

procedure TFusedLabel.WMMouseMove(var msg: TWMMouseMove);
begin
  if Font.Color <> EnterColor then
     begin
       OldColor := Font.Color;
       Font.Color := EnterColor;
     end;
  if not MouseCapture then
    SetCaptureControl(Self);
  if MouseCapture and ((msg.XPos < 0) or (msg.YPos < 0) or (msg.XPos > Width) or
     (msg.YPos > Height)) then {MouseOut}
  begin
    SetCaptureControl(nil);
    DoMouseOut(msg);
  end else DoMouseMove(msg);
end;
procedure TFusedLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
   if FButtonLike and not Pushed
     {and if the pixel at x,y is really text, not background}
      then
     begin
       Pushed := true;
       Old3D := AStyle3D;
       AStyle3D := Resit3D;
     end;
   inherited;
end;
procedure TFusedLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
   if FButtonLike and Pushed then
      begin
        AStyle3D := Old3D;
        Pushed := false;
      end;
   inherited;
end;

procedure TFusedLabel.DoMouseOut(msg:TWMMouseMove);
begin
  if assigned(MouseOut) then MouseOut(self);
  if Pushed then
     begin
       AStyle3D := Old3D;
       Pushed := false;
     end;
  Font.COlor := OldColor;
end;
procedure TFusedLabel.DoMouseMove(msg:TWMMouseMove);
begin
  if assigned(MouseMove) then MouseMove(self);
end;

procedure TFusedLabel.SetFormatString (f : string);
begin
     if FFormatString = f then exit;
     FFormatString := f;
     Invalidate;
end;

// TLabelEditorDlg
procedure TLabelEditorDlg.FormCreate(Sender: TObject);
begin
  EZLabel:=TFusedLabel.Create(Self);
  With EZLabel Do
   Begin
     Parent:=Panel;
     Align:=AlClient;
     {Top:=Dummy.Top;
     Left:=Dummy.Left;
     Width:=Dummy.Width;
     Height:=Dummy.Height;}
     Caption:='Example';
   End;
end;

procedure TLabelEditorDlg.EFitTypeClick(Sender: TObject);
begin
 Case EFitType.ItemIndex of
   0 : EZLabel.AFitType:=BestFitBoth;
   1 : EZLabel.AFitType:=BestFitHorz;
   2 : EZLabel.AFitType:=BestFitVert;
   3 : EZLabel.AFitType:=NormalFit;
 end;
end;

procedure TLabelEditorDlg.E3dStyleClick(Sender: TObject);
begin
 Case E3dStyle.ItemIndex of
   0 : EZLabel.AStyle3D:=Normal3d;
   1 : EZLabel.AStyle3D:=Resit3d;
   2 : EZLabel.AStyle3D:=Raised3d;
   3 : EZLabel.AStyle3D:=Shadowed3d;
 End;
end;

procedure TLabelEditorDlg.btFontClick(Sender: TObject);
begin
  FontD.Font.Assign(EZLabel.Font);
   IF FontD.Execute then
     EZLabel.Font.Assign(FontD.Font);
end;

procedure TLabelEditorDlg.ECaptionChange(Sender: TObject);
begin
  EZLabel.Caption:=ECaption.Text;
end;

procedure Register;
begin
  RegisterComponents('Fused',[TFusedLabel]);
  {RegisterPropertyEditor(TypeInfo(TLabels), nil, '', TLabelsProperty);}
end;

procedure TLabelEditorDlg.btColorClick(Sender: TObject);
begin
 ColorD.Color:=EZLabel.Font.Color;
 IF ColorD.Execute then
   EZLabel.Font.Color:=ColorD.Color;
end;

procedure TLabelEditorDlg.Button1Click(Sender: TObject);
begin
 ColorD.Color:=EZLabel.Color;
 IF ColorD.Execute then
  Begin
   EZLabel.Color:=ColorD.Color;
   EZLabel.Transparent:=False;
  end;
 cTrans.Checked:=EZLabel.Transparent;
end;

procedure TLabelEditorDlg.cTransClick(Sender: TObject);
begin
 EZLabel.Transparent:=cTrans.Checked;
end;

procedure TLabelEditorDlg.OKButtonClick(Sender: TObject);
begin

end;

end.
