{
 This component is a part of the freeware Fusion Package, containing
 a set of visual components. If redistributed, this unit should not be
 separated from the rest of the files in the package. If modified,
 all author credits must remain. Latest version of the package, bug
 reports, info on authors etc. can be found at
 http://www.geocities.com/SiliconValley/Drive/6381/
 Terms of use and other stuff explained in Fusion.hlp and FusionPack.txt.

 Component info:

{          Transparent Controls
           Copyright (c) 1997 Dream Company
           contact@dreamcompany.com

 With addition of
      - multi-line characteristics by CRESTO Sylvain
  cresto@mygale.org  http://www.mygale.org/~cresto/

      - color changing as a response to mouseover (for CheckBox and
  RadioButton), as well as checkboxes for the GroupBox by Milos Dragovic

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
From the author of multi-lined CheckBox and RadioButton:
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

 Class: TMultiCheckBox and TMultiRadioButton for Delphi 1, 2, 3 and C++ Builder 1
  Author: CRESTO Sylvain                                   -> version 1.3
  Written: 08/02/97  Modified: 12/15/97
  cresto@mygale.org
  http://www.mygale.org/~cresto/

  Information:
  TMultiCheckBox is a descendent of TCheckBox that allows multi-line captions.
  TMultiRadioButton is a descendent of TRadioButton that allows multi-line captions.
  I'm placing it in the public domain.  Do with it as you like!
}

Unit FusedTrctrls;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

Const
  WM_UPDTRANS = WM_USER + 2; {this message is called by hook procedure when
                                 transparent control should be updated  }
var                                 
  FDrawing: Integer = 0;

Type

  TTrGroupButton = class;
  TTransObject = Class(TObject)
  Private
    FControl: TWinControl;
    FTransparent, FDontEraseBack: Boolean;
    ftempdc: THandle;
    ftempbitmap: THandle;
    foldbitmap: THandle;
    Procedure KillTempDC;
    Procedure SetTransparent(V: Boolean);
    Procedure InternalPaint;
    Procedure SaveBackGround;
    Procedure WMUPDATETRANS;
    Procedure WMMove;
  Protected
  Public
    FDontDraw, FBackChanged: Boolean;
    Property Transparent: Boolean Read FTransparent Write SetTransparent Default True;
    Constructor Create(AControl: TWinControl);
    Destructor Destroy; override;
  End;

  TFusedRadioButton = class(TRadioButton)
  private
  protected
    fMultiLine: Boolean;
    FObject: TTransObject;
    FEnterColor, OldColor : TColor;
    Function GetTransparent: Boolean;
    Procedure SetTransparent(V: Boolean);
    procedure SetMultiLine(Value: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;

    Procedure WMUPDATETRANS(Var Msg: TMessage); message WM_UPDTRANS;
    Procedure BMSETCHECK(Var Msg: TMessage); message BM_SETCHECK;
    Procedure WMLBUTTONUP(Var Msg: TMessage); message WM_LBUTTONUP;
    Procedure WMMove(Var Msg: TMessage); message WM_MOVE;
    Procedure WMSize(Var Msg: TMessage); message WM_SIZE;
    Procedure WMPAINT(Var Msg: TWMPaint); message WM_PAINT;
    Procedure WMEraseBkgnd(Var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure CMTEXTCHANGED(Var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMMouseMove(var msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure DoMouseOut(msg:TWMMouseMove);
    procedure CMENABLEDCHANGED (var Message: TMessage); message CM_ENABLEDCHANGED;
    Procedure WMENABLE(Var Msg: TWMPaint); message WM_ENABLE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MultiLine: Boolean read fMultiLine write SetMultiLine default True;
    Property Transparent: Boolean Read GetTransparent Write SetTransparent;
    property EnterColor : TColor read FEnterColor write FEnterColor;
  end;

{ TTrGroupButton }
  TCustomTrRadioGroup = class;
  TTrGroupButton = Class(TFusedRadioButton)
  Private
    FInClick: Boolean;
    Procedure CNCommand(Var Message: TWMCommand); message CN_COMMAND;
  Protected
    Procedure ChangeScale(M, D: Integer); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyPress(Var Key: Char); override;
  Public
    Constructor InternalCreate(RadioGroup: TCustomTrRadioGroup);
    Destructor Destroy; override;
  End;

  TFusedCheckBox = class(TCheckBox)
  private
    FObject: TTransObject;
    Function GetTransparent: Boolean;
    Procedure SetTransparent(V: Boolean);
  protected
    FMultiLine: Boolean;
    FEnterColor, OldColor : TColor;
    procedure SetMultiLine(Value: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    Procedure WMUPDATETRANS(Var Msg: TMessage); message WM_UPDTRANS;
    Procedure WMMove(Var Msg: TMessage); message WM_MOVE;
    Procedure WMSize(Var Msg: TMessage); message WM_SIZE;
    Procedure WMPAINT(Var Msg: TWMPaint); message WM_PAINT;
    Procedure WMEraseBkgnd(Var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure CMTEXTCHANGED(Var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMMouseMove(var msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure DoMouseOut(msg:TWMMouseMove);
    procedure CMENABLEDCHANGED (var Message: TMessage); message CM_ENABLEDCHANGED;
    Procedure WMENABLE(Var Msg: TWMPaint); message WM_ENABLE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MultiLine: Boolean read FMultiLine write SetMultiLine;
    Property Transparent: Boolean Read GetTransparent Write SetTransparent;
    property EnterColor : TColor read FEnterColor write FEnterColor;
  end;

  TCustomTrGroupBox = Class(TCustomGroupBox)
  Private
    FObject: TTransObject;
    FCheckBox : TFusedCheckBox;
    FChecked, FHasCheckBox : boolean;
    Function GetTransparent: Boolean;
  Protected
    Procedure WMUPDATETRANS(Var Msg: TMessage); message WM_UPDTRANS;
    Procedure WMMove(Var Msg: TMessage); message WM_MOVE;
    Procedure WMSize(Var Msg: TMessage); message WM_SIZE;
    Procedure WMPAINT(Var Msg: TWMPaint); message WM_PAINT;
    Procedure WMEraseBkgnd(Var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    Procedure CMTEXTCHANGED(Var Msg: TMessage); message CM_TEXTCHANGED;
    procedure SetChecked (c : boolean);
    procedure SetHasCheckBox (c : boolean);
    procedure MakeCheckBox;
    procedure PositionCheckBox;
    procedure Loaded; override;
    procedure CheckClick (Sender : TObject);
    Procedure SetTransparent(V: Boolean); virtual;
    procedure SetCaption (c : TCaption);
    function GetCaption: TCaption;
    procedure CMENABLEDCHANGED (var Message: TMessage); message CM_ENABLEDCHANGED;
  Public
    Memo : TMemo;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    property CheckBox : boolean read FHasCheckBox write SetHasCheckBox;
    property Checked : boolean read FChecked write SetChecked;
    property Transparent : boolean read GetTransparent write SetTransparent;
    property Caption : TCaption read GetCaption write SetCaption;
  Published
  End;

  TFusedGroupBox = Class(TCustomTrGroupBox)
  Published
    Property Align;
    Property Caption;
    property Checked;
    property CheckBox;
    Property Color;
    Property Ctl3D;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentColor;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    property Transparent;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
  End;

{---------------------------------------------------------}
  TCustomTrRadioGroup = Class(TCustomTrGroupBox)
  Private
//    FSpec : boolean;
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    Procedure ArrangeButtons;
    Procedure ButtonClick(Sender: TObject);
    Procedure ItemsChange(Sender: TObject);
    Procedure SetButtonCount(Value: Integer);
    Procedure SetColumns(Value: Integer);
    Procedure SetItemIndex(Value: Integer);
    Procedure SetItems(Value: TStrings);
    Procedure UpdateButtons;
    Procedure CMEnabledChanged(Var Message: TMessage); message CM_ENABLEDCHANGED;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;
    Procedure WMSize(Var Message: TWMSize); message WM_SIZE;
    Procedure WMPaint(Var Message: TWMSize); message WM_paint;
    Procedure WMUPDATETRANS(Var Msg: TMessage); message WM_UPDTRANS;
    procedure Loaded; override;
  Protected
    FEnterColor : TColor;
    Procedure ReadState(Reader: TReader); override;
    Function CanModify: Boolean; virtual;
//{$IFDEF VER100}
    Procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
//{$ELSE}
//    Procedure GetChildren(Proc: TGetChildProc); override;
//{$ENDIF}
    Property Columns: Integer read FColumns write SetColumns default 1;
    Property ItemIndex: Integer read FItemIndex write SetItemIndex default - 1;
    Property Items: TStrings read FItems write SetItems;

    procedure SetEnterColor (c : TColor);

    Procedure SetTransparent(V: Boolean); override;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    property EnterColor : TColor read FEnterColor write SetEnterColor;
  End;

  TFusedRadioGroup = Class(TCustomTrRadioGroup)
  Published
    Property Align;
    Property Caption;
    property CheckBox;
    property Checked;
    Property Color;
    Property Columns;
    Property Ctl3D;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    property EnterColor;
    Property Font;
    Property ItemIndex;
    Property Items;
    Property ParentColor;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    property Transparent;    
    Property Visible;
    Property OnClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnStartDrag;
  End;

Function GetParentDC(P: TWInControl): THandle;
{---------------------------------------------------------}
Procedure Register;
{---------------------------------------------------------}
{$IFNDEF VER100}
type TWinControlClass = class of TWinControl;
{$ENDIF}

Const
  DDF_HALFTONE = $1000;

Procedure ControlTransPaintEX(W: TWinControl; BackDC: THandle; Var FTransparent: Boolean; X, Y: Integer);
Procedure ControlTransPaint(W: TWinControl; BackDC: THandle; Var FTransparent: Boolean);
Procedure AddHook(o: TWinControl);
Procedure RemoveHook(o: TWinControl);
Function GetTransparentColor(dc: THandle; arect: TRect): longint;
Procedure TransparentBitBlt(sourcedc, destdc: THandle; arect: TRect;
  aorigin: TPoint; atranscolor: longint);
Procedure SaveBackground(A: TWinControl; Var FTempDC, FTempBitmap, FOldBitmap: THandle);
Function Max(A, B: integer): integer;
Function Min(A, B: integer): integer;
Procedure RegisterTransControl(W: TWinControlClass);
Function IsTransControl(W: TWinControl): Boolean;

{---------------------------------------------------------}
Implementation
{$R *.res}
{----------------------------------------------------------}
{  TFusedRadioButton component }
{----------------------------------------------------------}
constructor TFusedRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMultiLine:=true;
  FObject := TTransObject.Create(Self);
  ControlStyle := ControlStyle - [csopaque];
end;

Procedure TFusedRadioButton.WMENABLE(Var Msg: TWMPaint);
begin

end;

procedure TFusedRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TFusedRadioButton.DoMouseOut(msg:TWMMouseMove);
begin
  Font.Color := OldColor;
end;

procedure TFusedRadioButton.WMMouseMove(var msg: TWMMouseMove);
begin
  inherited;
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
  end
end;

procedure TFusedRadioButton.CMTEXTCHANGED(Var Msg: TMessage);
begin
//     Height :=
     inherited;
     if FObject.Transparent then Invalidate;
end;

procedure TFusedRadioButton.SetMultiLine(Value: Boolean);
begin
  if fMultiLine<>Value then
  begin
    fMultiLine:=Value;
    RecreateWnd;
  end;
end;

procedure TFusedRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if MultiLine then Params.Style:=Params.Style or BS_MULTILINE or BS_TOP
               else Params.Style:=Params.Style and not BS_MULTILINE and not BS_TOP;
end;

Procedure TFusedRadioButton.WMPAINT(Var Msg: TWMPaint);
Var
  ps: TPaintStruct;
  R: TRect;
  DC: Thandle;
Begin
  if FObject.FDontDraw then
     begin
       Msg.result := 0;
       exit;
     end;
  With FObject do
  Begin
    If FDrawing > 0 then exit;
    If not FTransparent then
      Inherited
    Else
    Begin
      GetUpdateRect(FControl.Handle, R, False);
      If IsRectEmpty(R) then
        exit;
      BeginPaint(FControl.handle, ps);
      Msg.result := 0;
      InternalPaint;
      EndPaint(FControl.handle, ps);
    End;
  End;
End;

Procedure TFusedRadioButton.WMEraseBkgnd(Var Msg: TWMEraseBkgnd);
Begin
  If FObject.FTransparent or FObject.FDontEraseBack then
    Msg.Result := 1
  Else
    Inherited;
End;

Function TFusedRadioButton.GetTransparent: Boolean;
Begin
  Result := FObject.Transparent;
End;

{-------------------------------------------------------------}

Procedure TFusedRadioButton.SetTransparent(V: Boolean);
Begin
  FObject.Transparent := V;
End;

Destructor TFusedRadioButton.Destroy;
Begin
  FObject.Free;
  Inherited;
End;

Procedure TFusedRadioButton.WMUPDATETRANS(Var Msg: TMessage);
Begin
  FObject.WMUPDATETRANS;
End;

Procedure TFusedRadioButton.WMMove(Var Msg: TMessage);
Begin
  Inherited;
  FObject.WMMOVE;
End;

Procedure TFusedRadioButton.WMSize(Var Msg: TMessage);
Begin
  With FObject do
    If FTransparent then
    Begin
      KillTempDC;
      Inherited;
      WMMOVE;
    End else
      Inherited;
End;

Procedure TFusedRadioButton.BMSETCHECK(Var Msg: TMessage);
Begin
  Inherited;
  Invalidate;
End;

Procedure TFusedRadioButton.WMLBUTTONUP(Var Msg: TMessage);
Begin
  Inherited;
  Invalidate;
End;

{----------------------------------------------------------}
{  TCustomTrRadioGroup component }
{----------------------------------------------------------}

Constructor TCustomTrRadioGroup.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
  ControlStyle := ControlStyle - [csopaque];
End;

Destructor TCustomTrRadioGroup.Destroy;
Begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := Nil;
  FItems.Free;
  FButtons.Free;
  Inherited Destroy;
End;
procedure TCustomTrRadioGroup.Loaded;
begin
     inherited;
     ArrangeButtons;
end;

Procedure TCustomTrRadioGroup.ButtonClick(Sender: TObject);
Begin
  If not FUpdating then
  Begin
    FItemIndex := FButtons.IndexOf(Sender);
{$IFDEF VER100}
    Changed;
{$ENDIF}
    Click;
  End;
End;

Procedure TCustomTrRadioGroup.ItemsChange(Sender: TObject);
Begin
  If not FReading then
  Begin
    If FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  End;
End;

Procedure TCustomTrRadioGroup.ReadState(Reader: TReader);
Begin
  FReading := True;
  Inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
End;

Procedure TCustomTrRadioGroup.SetButtonCount(Value: Integer);
Begin
  While FButtons.Count < Value do TTrGroupButton.InternalCreate(Self);
  While FButtons.Count > Value do TTrGroupButton(FButtons.Last).Free;
End;

Procedure TCustomTrRadioGroup.SetColumns(Value: Integer);
Begin
  if csLoading in ComponentState then
    begin
      FColumns := Value;
      exit;
    end;
  If FColumns <> Value then
  Begin
    If Value < 1 then Value := 1;
    If Value > 16 then Value := 16;
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  End;
End;

Procedure TCustomTrRadioGroup.SetItemIndex(Value: Integer);
Begin
  If FReading then FItemIndex := Value else
  Begin
    If Value < -1 then Value := -1;
    If Value >= FButtons.Count then Value := FButtons.Count - 1;
    If FItemIndex <> Value then
    Begin
      If FItemIndex >= 0 then
        TTrGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      If FItemIndex >= 0 then
        TTrGroupButton(FButtons[FItemIndex]).Checked := True;
    End;
  End;
End;

Procedure TCustomTrRadioGroup.SetItems(Value: TStrings);
Begin
  FItems.Assign(Value);
End;

Procedure TCustomTrRadioGroup.UpdateButtons;
Var
  I: Integer;
Begin
  SetButtonCount(FItems.Count);
  For I := 0 to FButtons.Count - 1 do
    TTrGroupButton(FButtons[I]).Caption := FItems[I];
  If FItemIndex >= 0 then
  Begin
    FUpdating := True;
    TTrGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  End;
  ArrangeButtons;
  Invalidate;
End;

Procedure TCustomTrRadioGroup.ArrangeButtons;
Var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
Begin
  if csloading in componentstate then exit;
  If (FButtons.Count <> 0) and not FReading then
  Begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    For I := 0 to FButtons.Count - 1 do
      With TTrGroupButton(FButtons[I]) do
      Begin
        DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
          (I div ButtonsPerCol) * ButtonWidth + 8,
          (I mod ButtonsPerCol) * (ButtonHeight)+ TopMargin,
          ButtonWidth, ButtonHeight,
          SWP_NOZORDER or SWP_NOACTIVATE);
        Visible := True;
      End;
    EndDeferWindowPos(DeferHandle);
  End;
End;

Procedure TCustomTrRadioGroup.CMEnabledChanged(Var Message: TMessage);
Var
  I: Integer;
Begin
  Inherited;
  For I := 0 to FButtons.Count - 1 do
    TTrGroupButton(FButtons[I]).Enabled := Enabled;
End;

Procedure TCustomTrRadioGroup.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  ArrangeButtons;
End;

Procedure TCustomTrRadioGroup.WMSize(Var Message: TWMSize);
Begin
  Inherited;
  ArrangeButtons;
End;

Function TCustomTrRadioGroup.CanModify: Boolean;
Begin
  Result := True;
End;

//{$IFDEF VER100}
Procedure TCustomTrRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
//{$ELSE}
//Procedure TCustomTrRadioGroup.GetChildren(Proc: TGetChildProc);
//{$ENDIF}
Begin
End;


procedure TCustomTrRadioGroup.SetTransparent (v : boolean);
var i : integer;
begin
//   inherited;
   if FObject.Transparent = v then exit;
   FObject.FDontDraw := true;
   if FButtons.Count>0 then
      for i := 0 to FButtons.Count-1 do
          TFusedRadioButton(FButtons[i]).Visible := false;
   if FButtons.Count>0 then
      for i := 0 to FButtons.Count-1 do
          TFusedRadioButton(FButtons[i]).Transparent := v;
   FObject.FDontDraw := false;
   FObject.Transparent := v;
   if FButtons.Count>0 then
      for i := 0 to FButtons.Count-1 do
          TFusedRadioButton(FButtons[i]).Visible := true;
//   FObject.WMUPDATETRANS;
end;

procedure TCustomTrRadioGroup.SetEnterColor (c : TColor);
var i : integer;
begin
     if FEnterColor = c then exit;
     FEnterColor := c;
     For i := 0 to FButtons.Count - 1 do
         TFusedRadioButton(FButtons[i]).EnterColor := c;
end;

Procedure TCustomTrRadioGroup.WMUPDATETRANS(Var Msg: TMessage);
Var
  i: integer;
Begin
  Inherited;
  For i := 0 to FButtons.Count - 1 do
    TWInControl(FButtons[i]).Invalidate;
End;

Procedure TCustomTrRadioGroup.WMPaint(Var Message: TWMSize);
Var
  i: integer;
Begin
//  if FObject.FDontDraw then exit;
//  FObject.FDontDraw := true;
{  For i := 0 to ControlCount - 1 do
  Begin
    Controls[i].Visible := false;
  End;
  For i := 0 to ControlCount - 1 do
  Begin
    Controls[i].Visible := true;
  End;}
  Inherited;
  For i := 0 to ControlCount - 1 do
  Begin
    Controls[i].Invalidate;
  End;
{  FObject.FDontDraw := true;
  if FHasCheckBox then FCheckBox.Invalidate;
  For i := 0 to FButtons.Count - 1 do
  Begin
    TWInControl(FButtons[i]).Invalidate;
  End;
  FObject.FDontDraw := false;}
End;



{-------------------------------------------------------------}
{TTransObject}
{-------------------------------------------------------------}

Constructor TTransObject.Create(AControl: TWinControl);
Begin
  Inherited Create;
  FControl := AControl;
  FTransparent := True;
  fBackChanged := true;
  AddHook(AControl);
End;

Procedure TTransObject.WMUPDATETRANS;
Begin
  If FTransparent then
  Begin
    fbackchanged := true;
    InternalPaint;
  End;
End;


Procedure TTransObject.WMMove;
Begin
  If FTransparent then
  Begin
    FBackChanged := true;
    InternalPaint;
  End;
End;

{------------------------------------------------------------------}

Procedure TTransObject.KillTempDC;
Begin
  If FTempdc <> 0 then
  Begin
    SelectObject(ftempdc, foldbitmap);
    DeleteObject(ftempbitmap);
    DeleteDC(ftempdc);
    ftempdc := 0;
  End;
End;

{--------------------------------------------}

Procedure TTransObject.SaveBackGround;
Begin
  FBackChanged := false;
  Inc(FDrawing);
  FusedTrCtrls.SaveBackground(FControl, FTempDC, FTempBitmap, FOldBitmap);
  Dec(FDrawing);
End;

Procedure TTransObject.InternalPaint;
Var
  mParent: TWinControl;
  p: TPoint;
Begin
  If (Not FTransparent) or (FDrawing > 0) then exit;
  mParent := FControl.Parent;
  While (MParent <> Nil) and (IsTransControl(mParent.Parent))
    Do
    MParent := MParent.Parent;

  If (MParent <> Nil) and (isTransControl(mParent)) then
  Begin
    P.X := 0;
    P.Y := 0;
    P := FControl.ClientToScreen(P);
    P := mparent.ScreenToClient(P);
    ControlTransPaintEX(FControl, GetParentDC(mParent), FTransparent, P.X, P.Y);
  End else
  Begin
    If fBackChanged then
      SaveBackGround;
    ControlTransPaint(FCOntrol, FTempDC, FTransparent);
  End;
End;

Destructor TTransObject.Destroy;
Begin
  RemoveHook(FControl);
  KillTempDC;
  Inherited;
End;

Procedure TTransObject.SetTransparent(V: Boolean);
Begin
  If V <> FTransparent then
  Begin
    If FTransparent then RemoveHook(FControl);
    FTransparent := V;
    FBackChanged := True;
    FControl.Invalidate;
    InternalPaint;
    If FTransparent then AddHook(FControl);
  End;
End;

{-------------------------------------------------------}
{TFusedCheckBox}
{-------------------------------------------------------}

Constructor TFusedCheckBox.Create(AOwner: TComponent);
Begin
  Inherited;
  FObject := TTransObject.Create(Self);
  ControlStyle := ControlStyle - [csopaque];
End;

Destructor TFusedCheckBox.Destroy;
Begin
  FObject.Free;
  Inherited;
End;

Procedure TFusedCheckBox.WMENABLE(Var Msg: TWMPaint);
begin

end;

procedure TFusedCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TFusedCheckBox.DoMouseOut(msg:TWMMouseMove);
begin
  Font.Color := OldColor;
end;

procedure TFusedCheckBox.WMMouseMove(var msg: TWMMouseMove);
begin
  inherited;
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
  end
end;

Function TFusedCheckBox.GetTransparent: Boolean;
Begin
  Result := FObject.Transparent;
End;

{-------------------------------------------------------------}

Procedure TFusedCheckBox.SetTransparent(V: Boolean);
Begin
  FObject.Transparent := V;
End;

{-------------------------------------------------------------}
procedure TFusedCheckBox.CMTEXTCHANGED(Var Msg: TMessage);
begin
     inherited;
     if FObject.Transparent then Invalidate;
end;
procedure TFusedCheckBox.SetMultiLine(Value: Boolean);
begin
  if fMultiLine<>Value then
  begin
    fMultiLine:=Value;
    RecreateWnd;
  end;
end;

procedure TFusedCheckBox.CreateParams(var Params: TCreateParams);
begin
     inherited CreateParams(Params);
     if MultiLine then Params.Style:=Params.Style or BS_MULTILINE or BS_TOP
                  else Params.Style:=Params.Style and not BS_MULTILINE and not BS_TOP;
end;

Procedure TFusedCheckBox.WMUPDATETRANS(Var Msg: TMessage);
Begin
  FObject.WMUPDATETRANS;
End;

{--------------------------------------------}

Procedure TFusedCheckBox.WMPAINT(Var Msg: TWMPaint);
Var
  ps: TPaintStruct;
  R: TRect;
Begin
  With FObject do
  Begin
    If FDrawing > 0 then exit;
    If not FTransparent then
      Inherited
    Else
    Begin
      GetUpdateRect(FControl.Handle, R, False);
      If IsRectEmpty(R) then
        exit;
      BeginPaint(FControl.handle, ps);
      Msg.result := 0;
      InternalPaint;
      EndPaint(FControl.handle, ps);
    End;
  End;
End;

{--------------------------------------------}

Procedure TFusedCheckBox.WMEraseBkgnd(Var Msg: TWMEraseBkgnd);
Begin
  If FObject.FTransparent or FObject.FDontEraseBack then
    Msg.Result := 1
  Else
    Inherited;
End;

{------------------------------------------------------------------}

Procedure TFusedCheckBox.WMMove(Var Msg: TMessage);
Begin
  FObject.WMMOVE;
  Inherited;
End;
{-----------------------------------------------------------------}

Procedure TFusedCheckBox.WMSize(Var Msg: TMessage);
Begin
  With FObject do
    If FTransparent then
    Begin
      KillTempDC;
      Inherited;
      WMMOVE;
    End else
      Inherited;
End;

{-------------------------------------------------------------}
{TCustomTrGroupBox component}
{-------------------------------------------------------------}

Constructor TCustomTrGroupBox.Create(AOwner: TComponent);
Begin
  Inherited;
  FObject := TTransObject.Create(Self);
  ControlStyle := ControlStyle - [csopaque];
End;

Destructor TCustomTrGroupBox.Destroy;
Begin
  FObject.Free;
  if FHasCheckBox then FCheckBox.Free;
  Inherited;
End;

procedure TCustomTrGroupBox.Loaded;
begin
   inherited;
   PositionCheckBox;
     if not FChecked and CheckBox then
       begin
            FChecked := true;
            Checked := false;
       end;
end;
Procedure TCustomTrGroupBox.SetTransparent(V: Boolean);
begin
   if FObject.Transparent = v then exit;
   FObject.Transparent := v;
end;
Procedure TCustomTrGroupBox.CMTEXTCHANGED(Var Msg: TMessage);
begin
   inherited;
   PositionCheckBox;
end;
procedure TCustomTrGroupBox.SetCaption (c : TCaption);
begin
     if FHasCheckBox then
       begin
         FCheckBox.Caption := c;
         PositionCheckBox;
       end
     else inherited Caption := c;
end;

function TCustomTrGroupBox.GetCaption: TCaption;
begin
     if FHasCheckBox then result := FCheckBox.Caption
     else result := inherited Caption;
end;

procedure TCustomTrGroupBox.MakeCheckBox;
var cap : string;
begin
  cap := Caption;
  Caption := '';
  FCheckBox := TFusedCheckBox.Create(self);
  FHasCheckBox := true;
  FCheckBox.Top := 1;
  FCheckBox.OnClick := CheckClick;
  FCheckBox.Caption := Cap;
  FCheckBox.Transparent := Transparent;
//  FCheckBox.EnterCOlor := EnterColor;
  PositionCheckBox;
  FCheckBox.Checked := FChecked;
  FCheckBox.Parent := self;
//  FCheckBox.Invalidate;
end;

procedure TCustomTrGroupBox.CheckClick (Sender : TObject);
begin
  Checked := FCheckBox.Checked;
end;

procedure TCustomTrGroupBox.PositionCheckBox;
begin
  if csLoading in ComponentState then exit;
  if {(parent = nil) or }not FHasCheckBox then exit;
  FCheckBox.Left := 5;//+Canvas.TextWidth(Caption);
  FCheckBox.Width := 22+Canvas.TextWidth(FCheckBox.Caption);
end;

procedure TCustomTrGroupBox.CMENABLEDCHANGED (var Message: TMessage);
var i : integer;
begin
     inherited;
     for i := 0 to ControlCount-1 do
         Controls[i].Invalidate;
end;

procedure TCustomTrGroupBox.SetChecked (c : boolean);
var i : integer;
begin
   if FChecked = c then exit;
   FChecked := c;
   if FHasCheckBox then
     begin
       FCheckBox.Checked := c;
       for i := 0 to ControlCount-1 do
           if Controls[i]<>FCheckBox
              then Controls[i].Enabled := c and Enabled;
     end;
end;

procedure TCustomTrGroupBox.SetHasCheckBox (c : boolean);
var cap : string;
begin
   if FHasCheckBox=c then exit;
   if c then MakeCheckBox
   else
     begin
       Cap := FCheckBox.Caption;
       FHasCheckBox := false;
       FCheckBox.Free;
       Caption := cap;
     end;
end;

Function TCustomTrGroupBox.GetTransparent: Boolean;
Begin
  Result := FObject.Transparent;
End;

{-------------------------------------------------------------}

Procedure TCustomTrGroupBox.WMUPDATETRANS(Var Msg: TMessage);
Begin
  FObject.WMUPDATETRANS;
End;

{--------------------------------------------}

Procedure TCustomTrGroupBox.WMPAINT(Var Msg: TWMPaint);
Var
  ps: TPaintStruct;
  R: TRect;
Begin
  With FObject do
  Begin
    If FDrawing > 0 then exit;
    If not FTransparent then
      Inherited
    Else
    Begin
      GetUpdateRect(FControl.Handle, R, False);
      If IsRectEmpty(R) then
        exit;
      if Memo<>nil then Memo.Lines.Add ('wmpaint');
      if memo <>nil then
         memo.lines.add (inttostr(r.left)+', '+inttostr(r.top)+', '+inttostr(r.right)+', '+inttostr(r.bottom));
      BeginPaint(FControl.handle, ps);
      Msg.result := 0;
      InternalPaint;
      EndPaint(FControl.handle, ps);
    End;
  End;
End;

{--------------------------------------------}

Procedure TCustomTrGroupBox.WMEraseBkgnd(Var Msg: TWMEraseBkgnd);
Begin
  If FObject.FTransparent or FObject.FDontEraseBack then
    Msg.Result := 1
  Else
    Inherited;
End;

{------------------------------------------------------------------}

Procedure TCustomTrGroupBox.WMMove(Var Msg: TMessage);
Var
  i: integer;
Begin
  FObject.WMMOVE;
  Inherited;
  For i := 0 to ComponentCount - 1 do
    If (Components[i] is TWinControl) and
      (IsTransControl(TWinControl(Components[i]))) then
      PostMessage(TWinControl(Components[i]).Handle, WM_UPDTRANS, 0, 0);
End;

{-----------------------------------------------------------------}

Procedure TCustomTrGroupBox.WMSize(Var Msg: TMessage);
Begin
  With FObject do
    If FTransparent then
    Begin
      KillTempDC;
      Inherited;
      WMMOVE;
    End else
      Inherited;
End;

{-----------------------------------------------------------------------}

Var
  TransClasses: TList;

{-----------------------------------------------------------------------}

Function GetParentDC(P: TWInControl): THandle;
Begin
  Result := 0;
  If P is TFusedGroupBox then
    Result := TFusedGroupBox(P).FObject.FTempDC else
    If P is TFusedRadioGroup then
      Result := TFusedRadioGroup(P).FObject.FTempDC;
End;

Function IsTransControl(W: TWinControl): Boolean;
Var
  i: Integer;
Begin
  Result := True;
  For i := 0 to TransClasses.Count - 1 do
    If W is TWinControlClass(TransClasses.Items[i]) then
    Begin
      if W is TFusedRadioGroup then result := TCustomTrGroupBox(W).Transparent;
      exit;
    End;
  Result := False;
End;

{-----------------------------------------------------------------------}

Procedure RegisterTransControl(W: TWinControlClass);
Begin
  TransClasses.Add(W);
End;

{-----------------------------------------------------------------------}

Var
  WHook: HHook;
  hooks: TList;

Type TCWPStruct = Packed record
    lParam: LPARAM;
    wParam: WPARAM;
    message: integer;
    wnd: HWND;
  End;

Function CallWndProcHook(nCode: integer; wParam: Longint; Var Msg: TCWPStruct): longint; stdcall;
Var
  i: integer;
  r: TRect;
  r2: TRect;
  c: TWinControl;


  Function IsPaintMsg: boolean;
  Begin
    With TWinControl(hooks[i]) do
    Begin
      result := false;
      If not HandleAllocated then exit;
      If C = Owner then
      Begin
        If (msg.message = WM_MOVE) then exit;
        Result := True;
        exit;
      End;
      If C.Owner = Owner then
      Begin
        GetWindowRect(msg.wnd, r);
        GetWindowRect(handle, r2);
        result := IntersectRect(r, r, r2);
      End;
    End;
  End;

Begin
  Result := CallNextHookEx(WHook, nCode, wParam, Longint(@Msg));
  If ((msg.message > CN_BASE) and (msg.message < CN_BASE + 500)) or
    (msg.message = WM_PAINT) or (msg.message = WM_SIZE)
    Or (msg.message = WM_MOVE)
    Then
  Begin
    c := FindControl(msg.wnd);
    If (c = Nil) or (IsTransControl(c)) then exit;
    For i := 0 to hooks.Count - 1 do
    Begin
      If (IsPaintMsg) then
        SendMessage(TWinControl(hooks[i]).Handle, WM_UPDTRANS, 0, 0);
    End;
  End;
End;

{------------------------------------------------------------------}

Procedure AddHook(o: TWinControl);
Var
  i: integer;
Begin
  If hooks.Count = 0 then
    WHook := SetWindowsHookEx(WH_CALLWNDPROC, @CallWndProcHook, 0, GetCurrentThreadId);
  For i := 0 to Hooks.Count - 1 do
    If Hooks.Items[i] = o then exit;
  hooks.Add(o);
End;

{------------------------------------------------------------------}

Procedure RemoveHook(o: TWinControl);
Begin
  hooks.Remove(o);
  If hooks.Count = 0 then
    UnHookWindowsHookEx(WHook);
End;

{------------------------------------------------------}

Function Min(A, B: integer): integer;
Begin
  If A < B then
    Result := A
  Else
    Result := B;
End;

{------------------------------------------------------}

Function Max(A, B: integer): integer;
Begin
  If A > B then
    Result := A
  Else
    Result := B;
End;

{--------------------------------------------}

Procedure ControlTransPaint(W: TWinControl; BackDC: THandle; Var FTransparent: Boolean);
Begin
  ControlTransPaintEX(W, BackDC, FTransparent, 0, 0);
End;

Procedure ControlTransPaintEX(W: TWinControl; BackDC: THandle; Var FTransparent: Boolean; X, Y: Integer);
Var
  DC: THandle;
  memdc: THandle;
  formdc: THandle;
  fbitmap: THandle;
  oldfobject: THandle;
  bitmap: THandle;
  oldmemobject: THandle;
Begin
  With W do
  Begin
    If ([csReading, csLoading] * ComponentState <> []) or (Parent = Nil)
      Or ([csReading, csLoading] * Parent.ComponentState <> [])
      Or (Not HandleAllocated) or (Not (visible)) then
      exit;

    dc := GetDC(handle);
    memdc := CreateCompatibleDC(dc);
    formdc := CreateCompatibleDC(dc);

    fbitmap := CreateCompatibleBitmap(dc, width, height);
    oldfobject := SelectObject(formdc, fbitmap);
    bitmap := CreateCompatibleBitmap(dc, width, height);
    oldmemobject := SelectObject(memdc, bitmap);

    BitBlt(formdc, 0, 0, width, height, BackDC, x, y, SRCCOPY); {1}

    FTransparent := False;
    PaintTo(MemDC, 0, 0); {2}
    FTransparent := True;

    TransparentBitBlt(MemDC, FormDC, Rect(0, 0, width, height), Point(0, 0),
      GetTransparentColor(MemDC, Rect(0, 0, width - 1, height - 1))); {3}

    BitBlt(dc, 0, 0, width, height, formDC, 0, 0, SRCCOPY); {4}

    SelectObject(formdc, oldfobject);
    DeleteObject(fbitmap);
    SelectObject(memdc, oldmemobject);
    DeleteObject(bitmap);

    ReleaseDC(handle, dc);
    DeleteDC(memdc);
    DeleteDC(formdc);
  End;
End;

{-----------------------------------------------------------------------}

Procedure SaveBackground(A: TWinControl; Var FTempDC, FTempBitmap, FOldBitmap: THandle);
Var
  dc: THandle;
  formdc: THandle;
  oldfbitmap: THandle;
  fbitmap: THandle;
  fdc: THandle;
Begin
  With A do
  Begin

    If Parent = Nil then
      exit;

    dc := GetDC(handle);
    fdc := GetDC(parent.handle);
    formdc := CreateCompatibleDC(fdc);
    fbitmap := CreateCompatibleBitmap(fdc, parent.width, parent.height);
    oldfbitmap := SelectObject(formdc, fbitmap);

    If ftempdc = 0 then
    Begin
      ftempdc := CreateCompatibleDC(dc);
      ftempbitmap := CreateCompatibleBitmap(dc, width, height);
      foldbitmap := SelectObject(ftempdc, ftempbitmap);
    End;
    IntersectClipRect(formdc, left, top, left + width + 1, top + height + 1);
    parent.PaintTo(formdc, 0, 0);
    BitBlt(ftempdc, 0, 0, width, height, formdc, left + 1, top + 1, SRCCOPY);
    SelectObject(formdc, oldfbitmap);
    DeleteObject(fbitmap);
    DeleteDC(formdc);
    ReleaseDC(Parent.Handle, fdc);
    ReleaseDC(handle, dc);
  End;
End;

{---------------------------------------------------------------------}

Function GetTransparentColor(dc: THandle; arect: TRect): longint;
Begin
  result := GetPixel(dc, arect.left, arect.bottom);
End;

{-----------------------------------------------------------------------}

{$IFNDEF VER100}

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC  : THandle;
  MemBmp : THandle;
  Save   : THandle;
  crText : TColorRef;
  crBack : TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
    begin
      MemBmp := CreateCompatibleBitmap(SrcDC, 1, 1);
      MemBmp := SelectObject(MaskDC, MemBmp);
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
              MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
      exit;
    end;

  SavePal := 0;
  MemDC := CreateCompatibleDC(0);
  MemBmp := CreateCompatibleBitmap(SrcDC, SrcW, SrcH);
  Save := SelectObject(MemDC, MemBmp);
  StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
  StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
  crText := SetTextColor(DstDC, $0);
  crBack := SetBkColor(DstDC, $FFFFFF);
  StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
  StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
  SetTextColor(DstDC, crText);
  SetTextColor(DstDC, crBack);
  SelectObject(MemDC, Save);
  DeleteObject(MemBmp);
  DeleteDC(MemDC);
end;

{$ENDIF}

Procedure TransparentBitBlt(sourcedc, destdc: THandle; arect: TRect;
    aorigin: TPoint; atranscolor: longint);
Var
  monobitmap: THandle;
  oldbkcolor: longint;
  monodc: THandle;
  width: integer;
  height: integer;
  oldbitmap: THandle;
Begin
  With arect do
  Begin
    width := right - left;
    height := bottom - top;
    monodc := CreateCompatibleDC(sourcedc);
    monobitmap := CreateCompatibleBitmap(monodc, width, height);
    Try
      oldbitmap := SelectObject(monodc, monobitmap);
      oldbkcolor := SetBkColor(sourcedc, atranscolor);
      BitBlt(monodc, 0, 0, width, height, sourcedc, 0, 0, SRCCOPY);
      SetBkColor(sourcedc, oldbkcolor);
      TransparentStretchBlt(destdc, aorigin.x, aorigin.y, width, height,
        SourceDC, left, top, width, height, monodc, 0, 0);
    Finally
      SelectObject(monodc, oldbitmap);
      DeleteDC(monodc);
      DeleteObject(monobitmap);
    End;
  End;
End;

Constructor TTrGroupButton.InternalCreate(RadioGroup: TCustomTrRadioGroup);
Begin
  Inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  EnterColor := RadioGroup.EnterColor;
  Transparent := RadioGroup.Transparent;
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
  RemoveHook(Self);
End;

Destructor TTrGroupButton.Destroy;
Begin
  TCustomTrRadioGroup(Owner).FButtons.Remove(Self);
  Inherited Destroy;
End;

Procedure TTrGroupButton.CNCommand(Var Message: TWMCommand);
Begin
  If not FInClick then
  Begin
    FInClick := True;
    Try
      If ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
          TCustomTrRadioGroup(Parent).CanModify then
        Inherited;
    Except
      Application.HandleException(Self);
    End;
    FInClick := False;
  End;
End;

Procedure TTrGroupButton.ChangeScale(M, D: Integer);
Begin
End;

Procedure TTrGroupButton.KeyPress(Var Key: Char);
Begin
  Inherited KeyPress(Key);
  TCustomTrRadioGroup(Parent).KeyPress(Key);
  If (Key = #8) or (Key = ' ') then
  Begin
    If not TCustomTrRadioGroup(Parent).CanModify then Key := #0;
  End;
End;

Procedure TTrGroupButton.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  Inherited KeyDown(Key, Shift);
  TCustomTrRadioGroup(Parent).KeyDown(Key, Shift);
End;

{-------------------------------------------------------------}

Procedure Register;
Begin
  RegisterComponents('Fused',
    [TFusedGroupBox, TFusedRadioButton, TFusedCheckBox,
     TFusedRadioGroup]);
End;

{-------------------------------------------------------------}
Initialization
  hooks := TList.Create;
  TransClasses := TList.Create;

  RegisterTransControl(TFusedRadioButton);
  RegisterTransControl(TFusedCheckBox);
  RegisterTransControl(TFusedGroupBox);
  RegisterTransControl(TFusedRadioGroup);
  RegisterTransControl(TTrGroupButton);
//  RegisterTransControl(TTestEdit);
Finalization

  If hooks.Count > 0 then
    UnHookWindowsHookEx(WHook);
  hooks.Free;
  TRansClasses.Free;

End.
