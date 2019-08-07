unit FusedEdit;
{$D+}
{
 This component is a part of the freeware Fusion Package, containing
 a set of visual components. If redistributed, this unit should not be
 separated from the rest of the files in the package. If modified,
 all author credits must remain. Latest version of the package, bug
 reports, info on authors etc. can be found at
 http://www.geocities.com/SiliconValley/Drive/6381/
 Terms of use and other stuff explained in Fusion.hlp and FusionPack.txt.

 Component info:

 TFusedEdit offers new characteristics:

  - can be Transparent          by Milos Dragovic, but I copied
        a lot of code from Tim Lawrenz(tim@lawrenz.com) and Max Muermann
	(muermann@stud.uni-frankfurt.de) and their TTransparentMemo
  - numbers only option         partly by Jim Clark jim@innovonics.com.au
  - finding out cursor's (caret's) position by Larry J. Rutledge
  - keeps history, can create and maintain a speed button and a label
                   by Milos Dragovic
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  StdCtrls, FusedTrCtrls, Buttons;

const
  TMWM__SpecialInvalidate=WM_USER+1111;
  VKIgnore : set of 0.. 100 = [VK_LEFT, VK_RIGHT,
           VK_BACK, VK_DELETE, VK_HOME, VK_END, VK_INSERT, VK_TAB,
           VK_SHIFT];
type

  TFusedSpeedButton = class (TSpeedButton)
    protected
      procedure Paint; override;
    public
      property Canvas;
    end;

  TFusedEdit = class(TEdit)
  private
    procedure SpecialInvalidate(var Message:TMessage); message
              TMWM__SpecialInvalidate;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMSetText(var Message:TWMSetText); message WM_SETTEXT;
    procedure CNCTLCOLOREDIT(var Message:TWMCTLCOLOREDIT); message
              CN_CTLCOLOREDIT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TMessage); message WM_MOVE;
    procedure WMSIZE(var Message: TMessage); message WM_SIZE;
    Procedure WMENABLE(Var Msg: TWMPaint); message WM_ENABLE;
  protected
    FFileName, FStringFormat, FNumberFormat, PreviousText,
               ActualText, FButtonCaption, FCaption: string;
    FValue : extended;
    FCaseSensitiveHistory, FUseHistory, Formatting, FHasValidNumber,
       FHasButton, SpecIn, FTransparent, FNumbersOnly : boolean;
    Items: TStringList;
    FOnItemFound, FOnButtonClick : TNotifyEvent;
    FButton : TFusedSpeedButton;
    FLabel : TLabel;
    FBitmap : TBitmap;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure WMLButtonDown(var Message: TWMKeyDown); message WM_LBUTTONDOWN;
    procedure SetTransparent (b : boolean);
    procedure SetNumbersOnly (b : boolean);
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;

    procedure SetValue (d : extended);
    procedure SetStringFormat (s : string);
    procedure SetNumberFormat (s : string);
    procedure FormatText;
    function GetCursorPos : integer;
    procedure SetCursorPos (i: integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

    procedure ButtonClick (Sender:TObject);
    procedure KeyUp(var Key : Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure SetFileName (fn : string);
    procedure SetUseHistory (b : boolean);
    procedure SetHasButton (b : boolean);
    procedure SetButtonGlyph (g : TBitmap);
    procedure SetParent (AControl : TWinControl); override;
    procedure SetButtonCaption (c : string);
    procedure SetCaption (c : string);
    procedure UpdateControls;
    property HasButton : boolean read FHasButton write SetHasButton;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadHistory;
    procedure SaveHistory;
    property CursorPos : integer read GetCursorPos write SetCursorPos;
    property HasValidNumber : boolean read FHasValidNumber;
  published
    property ButtonCaption : string read FButtonCaption write SetButtonCaption;
    property ButtonGlyph : TBitmap read FBitmap write SetButtonGlyph;
    property Caption : string read FCaption write SetCaption;
    property CaseSensitiveHistory : boolean read FCaseSensitiveHistory write FCaseSensitiveHistory;
    property FileName : string read FFileName write FFileName;
    property NumberFormat: string read FNumberFormat write SetNumberFormat;
    property NumbersOnly : boolean read FNumbersOnly write SetNumbersOnly;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnItemFound : TNotifyEvent read FOnItemFound write FOnItemFound;
    property StringFormat: string read FStringFormat write SetStringFormat;
    property Transparent : boolean read FTransparent write SetTransparent;
    property Value: extended read FValue write SetValue;
    property UseHistory: boolean read FUseHistory write SetUseHistory;
  end;

procedure Register;

implementation
{$R *.res}
procedure Register;
begin
  RegisterComponents('Fused', [TFusedEdit]);
end;

constructor TFusedEdit.Create(AOwner: TComponent);
begin
  inherited;
//ControlStyle:=[csCaptureMouse, csDesignInteractive,
//csClickEvents,  csSetCaption, csDoubleClicks,
  //csReplicatable, csNoStdEvents];
  Text:= '';
  PreviousText:= '';
  ControlStyle:= ControlStyle - [csOpaque];
  FBitmap := TBitmap.Create;
end;

procedure TFusedEdit.SetHasButton (b : boolean);
begin
  if b = FHasButton then exit;
  FHasButton := b;
  if b then
    begin
      FButton := TFusedSpeedButton.Create(self);
      FButton.Caption := FButtonCaption;
      FButton.Glyph := FBitmap;
      FButton.Flat := FTransparent;
      FButton.OnClick := ButtonClick;
      FButton.Parent := Parent;
      UpdateControls;
    end
  else FButton.Free;
end;

procedure TFusedEdit.ButtonClick (Sender:TObject);
begin
     if assigned(FOnButtonClick) then FOnButtonClick(self);
end;

procedure TFusedEdit.SetButtonCaption (c : string);
begin
     if c = FButtonCaption then exit;
     FButtonCaption := c;
     if (c='') and FBitmap.Empty then HasButton := false
     else
       begin
         HasButton := true;
         FButton.Caption := c;
         UpdateControls;
       end
end;

procedure TFusedEdit.SetButtonGlyph (g : TBitmap);
begin
   FBitmap.Assign(g);
   if (FButtonCaption='') and (g=nil) then HasButton := false
   else
     begin
       HasButton := true;
       FButton.Glyph := g;
       UpdateControls;
     end
end;

procedure TFusedEdit.SetParent (AControl : TWinControl);
begin
  if FHasButton then FButton.parent := parent;
  if FCaption<>'' then FLabel.parent := parent;
  inherited;
end;

procedure TFusedEdit.SetCaption (c : string);
begin
  if c = FCaption then exit;
  FCaption := c;
  if c='' then
    begin
      FLabel.Free;
      FLabel := nil;
    end
  else
    begin
      if FLabel = nil then
        begin
          FLabel := TLabel.Create(self);
          FLabel.AutoSize := true;
          FLabel.Transparent := Transparent;
          FLabel.Parent := Parent;
        end;
      FLabel.Caption := c;
      UpdateControls;
    end;
end;

procedure TFusedEdit.UpdateControls;
var w : integer;
begin
   if FHasButton then
     begin
       FButton.Top := Top;
       FButton.Left := Left+Width;
       if FButtonCaption='' then FButton.Width := FButton.Height
       else
         begin
            w := FButton.Canvas.TextWidth(FButtonCaption)+5+FButton.Glyph.Width;
            if w>FButton.Height
               then FButton.Width := w
               else FButton.Width := FButton.Height;
         end;
     end;
   if FLabel<>nil then
     begin
          FLabel.Left := Left;
          FLabel.Top := Top-FLabel.Height;
     end;
end;

procedure TFusedEdit.WMSIZE(var Message: TMessage);
begin
  inherited;
  UpdateControls;
end;

procedure TFusedEdit.WMMove(var Message: TMessage);
begin
  inherited;
  UpdateControls;
  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0)
  else Invalidate;
end;
procedure TFusedEdit.WMENABLE(Var Msg: TWMPaint);
begin
//   inherited;
   if FHasButton then FButton.Enabled := Enabled;
end;
destructor TFusedEdit.Destroy;
begin
  if FUseHistory then
    begin
      SaveHistory;
      Items.Free;
    end;
  FBitmap.Free;
  inherited;
end;
procedure TFusedEdit.Loaded;
begin
     inherited;
     if not FBitmap.Empty then HasButton := true;
     if not (csDesigning in ComponentState) then LoadHistory;
end;
procedure TFusedEdit.SetUseHistory (b : boolean);
begin
     if FUseHistory = b then exit;
     FUseHistory:=b;
     if csDesigning in ComponentState then exit;
     if b
       then
         begin
           Items := TStringList.create;
           Items.Sorted := true;
           if FileExists(FFileName)
              then Items.LoadFromFile (FFileName);
         end
       else Items.Free;
end;
procedure TFusedEdit.SaveHistory;
begin
    if not(csDesigning in ComponentState) and FUseHistory
       and (FFilename<>'')
           then Items.SaveToFile(FFileName);
end;
procedure TFusedEdit.LoadHistory;
begin
    if (FFileName<>'') and (ExtractFilePath(FFilename)='') then
        FFileName := ExtractFilePath(Application.ExeName)+FFileName;
    if not FUseHistory or (csDesigning in ComponentState) then exit;
    Items.Clear;
    if FileExists(FFileName) then
       Items.LoadFromFile(FFileName);
end;

procedure TFusedEdit.SetFileName (fn : string);
begin
  FFileName := fn;
  if (csDesigning in ComponentState) then exit;
  if (FFileName<>'') and (ExtractFilePath(FFilename)='') then
        FFileName := ExtractFilePath(Application.ExeName)+FFileName;
  if not UseHistory then exit;
  Items.Clear();
  if (FileExists(FFileName)) then
     Items.LoadFromFile(FFileName);
end;

procedure TFusedEdit.KeyUp(var Key : Word; Shift: TShiftState);
var t2 : string;i, l : integer;
begin
  if not (key in [VK_DOWN, VK_UP]) then inherited;
  if not UseHistory or (key in VKIgnore) then exit;
  if key in [VK_DOWN, VK_UP] then
     begin
        i := Items.IndexOf(Text);
          if (key=VK_DOWN) and (i<Items.Count-1)
             then  Text := Items.Strings[i+1];
          if (key=VK_UP) and (i>0)
             then  Text := Items.Strings[i-1];
     end
  else
    begin
       l := Length(Text);
       if (l=0) then exit;
       if not FCaseSensitiveHistory then t2 := LowerCase(Text);
       for i := 0 to Items.Count-1 do
           if (FCaseSensitiveHistory and (copy (Items.Strings[i],1,l)=t2)) or
              (not FCaseSensitiveHistory and (copy (LowerCase(Items.Strings[i]),1,l)=t2)) then
              begin // text is recognized
                Text := Items.Strings[i];
                SelStart := l;
                SelLength := Length(Text)-l;
                Change;
                if assigned (FOnItemFound) then FOnItemFound(self);
                exit;
              end;
    end;
end;

function TFusedEdit.GetCursorPos : integer;
begin
     result := LoWord(SendMessage(Handle,EM_GETSEL, 0, 0));
end;
procedure TFusedEdit.SetCursorPos (i: integer);
begin
    SendMessage(Handle,EM_SETSEL, i, i);
end;

procedure TFusedEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FTransparent then
    with Params do
      begin
  	ExStyle:=ExStyle or WS_EX_TRANSPARENT and not WS_EX_WINDOWEDGE
     		and not WS_EX_STATICEDGE and not WS_EX_DLGMODALFRAME and not
           WS_EX_CLIENTEDGE;
      end;
end;

procedure TFusedEdit.SetValue (d : extended);
begin
   if d<>FValue then
      begin
           FValue := d;
           FHasValidNumber := true;
           FormatText;
      end;
end;
procedure TFusedEdit.FormatText;
begin
  Formatting := true;
  if HasValidNumber then
    if FNumberFormat<>''
       then Text := Format (FNumberFormat, [FValue])
       else Text := FloatToStr(FValue)
  else
    if FStringFormat<>''
       then Text := Format (FStringFormat, [ActualText]);
//       else Text := ActualText;
  Formatting := false;
end;

procedure TFusedEdit.SetNumbersOnly (b : boolean);
begin
   FNumbersOnly := b;
   PreviousText := '';
   if not HasValidNumber then Change;
end;
procedure TFusedEdit.KeyPress(var Key: Char);
begin
//   if not FNumbersOnly then exit;
   PreviousText:= Text;
   inherited KeyPress(Key);
end;
procedure TFusedEdit.CMTextChanged(var Message: TMessage);
begin
//  Change;
  inherited;
end;
procedure TFusedEdit.Change;
var pos : integer;
begin
//   if not FNumbersOnly then exit;
   if Formatting then exit;
   FHasValidNumber:= TextToFloat (PChar(Text), FValue, fvextended);
   if not HasValidNumber and FNumbersOnly and (Text <> '-') and (Length(Text) > 0) then
       begin
         pos := GetCursorPos;
         Text:= PreviousText;
         SetCursorPos(pos-1);
       end;
   inherited Change;
   if not Focused and HasValidNumber then FormatText;
end;
procedure TFusedEdit.SetStringFormat (s : string);
begin
     if FStringFormat<>s then
       begin
            FStringFormat:=s;
            FormatText;
       end;
end;
procedure TFusedEdit.SetNumberFormat (s : string);
begin
     if FNumberFormat<>s then
       begin
            FNumberFormat:=s;
            if HasValidNumber then FormatText;
       end;
end;

procedure TFusedEdit.SetTransparent (b : boolean);
begin
  if FTransparent=b then exit;
  FTransparent := b;
  if FHasButton then FButton.Flat := b;
  if FCaption<>'' then FLabel.Transparent := b;  
  if not (csDesigning in ComponentState) then
    begin
      if b
        then ControlStyle := ControlStyle + [csOpaque]
        else ControlStyle := ControlStyle - [csOpaque];
      CreateWnd;
    end;
  Invalidate;
end;
procedure TFusedEdit.DoEnter;
begin
   if HasValidNumber then
      Text := floattostr(FValue)
   else Text := ActualText;
   if AutoSelect then SelectAll;
end;
//  Transparency code - message handling
procedure TFusedEdit.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedEdit.WMVScroll(var Message: TWMVScroll);
begin
//  SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedEdit.CNCTLCOLOREDIT(var Message:TWMCTLCOLOREDIT);
begin
  if FTransparent then
   with Message do
    begin
      SetBkMode(ChildDC,Windows.TRANSPARENT);
      Result:=GetStockObject(HOLLOW_BRUSH)
    end
   else inherited;
end;
procedure TFusedEdit.WMSetText(var Message:TWMSetText);
begin
  inherited;
  if FTransparent and not (csDesigning in ComponentState) then
    PostMessage(Handle,TMWM__SpecialInvalidate,0,0)
end;
procedure TFusedEdit.SpecialInvalidate(var Message:TMessage);
var r:TRect; par : TWinControl;
begin
  if SpecIn then exit;
  SpecIn := true;
  if (Parent<>nil) and FTransparent then
    begin
      r:=ClientRect;
      r.TopLeft:=Parent.ScreenToClient(ClientToScreen(r.TopLeft));
      r.BottomRight:=Parent.ScreenToClient(ClientToScreen(r.BottomRight));
      par := Parent;
      while IsTransControl(par) do
            par := par.parent;
      InvalidateRect(Parent.Handle,@r,true);
      RedrawWindow(Handle,nil,0,RDW_FRAME+RDW_INVALIDATE);
    end;
  SpecIn := false;
end;
procedure TFusedEdit.WMKeyDown(var Message: TWMKeyDown);
begin
//  SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if FTransparent and not (csDesigning in ComponentState) then
    begin
//      Message.Result:=1;
      PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
//      Message.Result:=1;
    end
  else inherited;
end;

procedure TFusedEdit.DoExit;
begin
  inherited;
  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
  if FUseHistory and (Text<>'') and (Items.IndexOf(Text)=-1)
     then Items.Add(Text);
  ActualText := Text;
  FormatText;
end;

procedure TFusedEdit.WMLButtonDown(var Message: TWMKeyDown);
begin
  inherited;
  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
//--------------------------------------

procedure TFusedSpeedButton.Paint;
var  PaintRect: TRect;
begin
//  Canvas.Font := Self.Font;
//  I like it this way, leave only the call to inherited
//  if you want the default behaviour
  PaintRect := Rect(0, 0, Width, Height);
  DrawEdge(Canvas.Handle, PaintRect, BDR_RAISEDINNER, BF_RECT);
  inherited;
end;

end.
