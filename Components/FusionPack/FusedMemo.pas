unit FusedMemo;
{
 This component is a part of the freeware Fusion Package, containing
 a set of visual components. If redistributed, this unit should not be
 separated from the rest of the files in the package. If modified,
 all author credits must remain. Latest version of the package, bug
 reports, info on authors etc. can be found at
 http://www.geocities.com/SiliconValley/Drive/6381/
 Terms of use and other stuff explained in Fusion.hlp and FusionPack.txt.

 Component info:

 This is a memo with following added characteristics
      Characteristic                         Author
  - can be Transparent by Tim Lawrenz(tim@lawrenz.com)
           and Max Muermann (muermann@stud.uni-frankfurt.de),TTransparentMemo
  - finding out cursor's (caret's) position by Larry J. Rutledge
}

interface
uses Messages, Controls, StdCtrls,classes, comctrls;
const TMWM__SpecialInvalidate=WM_USER+1111;
type
  TFusedMemo = class(TMemo)
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
  protected
    FTransparent : boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoExit; override;
    procedure SetTransparent(t : boolean);
    procedure SetRow (i : integer);
    procedure SetColumn (i : integer);
    function GetRow : integer;
    function GetColumn : integer;
    Procedure WMENABLE(Var Msg: TWMPaint); message WM_ENABLE;
  public
    constructor Create(AOwner: TComponent); override;
    property Row : integer read GetRow write SetRow;
    property Column: integer read GetColumn write SetColumn;
  published
    property Transparent : boolean read FTransparent write SetTransparent;
  end;
procedure Register;
implementation
uses Windows;
{$R *.res}
{ TFusedMemo }

procedure TFusedMemo.SetRow (i : integer);
var j : integer;
begin
    j := SendMessage(Handle, EM_LINEINDEX, i, 0);
    SendMessage(Handle,EM_SETSEL, j+Column, j+Column);
end;
procedure TFusedMemo.SetColumn (i : integer);
var j : integer;
begin
    j := SendMessage(Handle, EM_LINEINDEX, Row, 0);
    SendMessage(Handle,EM_SETSEL, j+i, j+i);
end;
function TFusedMemo.GetRow;
begin
  result := SendMessage(Handle, EM_LINEFROMCHAR, -1, 0);
end;
function TFusedMemo.GetColumn;
begin
  result := LoWord(SendMessage(Handle, EM_GETSEL, 0, 0)) -
            SendMessage(Handle, EM_LINEINDEX, -1, 0);
end;
procedure TFusedMemo.SetTransparent(t : boolean);
begin
  if FTransparent=t then exit;
  FTransparent := t;
  if not (csDesigning in ComponentState) then
    begin
      if t
        then ControlStyle := ControlStyle + [csOpaque]
        else ControlStyle := ControlStyle - [csOpaque];
      CreateWnd;
    end;
  Invalidate;
end;
procedure TFusedMemo.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedMemo.WMVScroll(var Message: TWMVScroll);
begin
  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedMemo.CNCTLCOLOREDIT(var Message:TWMCTLCOLOREDIT);
begin
  if FTransparent then
  with Message do
    begin
      SetBkMode(ChildDC,Windows.TRANSPARENT);
      Result:=GetStockObject(HOLLOW_BRUSH)
    end
end;
procedure TFusedMemo.WMSetText(var Message:TWMSetText);
begin
  inherited;
  if not (csDesigning in ComponentState) and FTransparent then
    PostMessage(Handle,TMWM__SpecialInvalidate,0,0)
end;
procedure TFusedMemo.SpecialInvalidate(var Message:TMessage);
var r:TRect;
begin
  if Parent<>nil then
    begin
      r:=ClientRect;
      r.TopLeft:=Parent.ScreenToClient(ClientToScreen(r.TopLeft));
      r.BottomRight:=Parent.ScreenToClient(ClientToScreen(r.BottomRight));
      InvalidateRect(Parent.Handle,@r,true);
      RedrawWindow(Handle,nil,0,RDW_FRAME+RDW_INVALIDATE)
    end;
end;
procedure TFusedMemo.WMKeyDown(var Message: TWMKeyDown);
begin
//  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
  inherited;
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
procedure TFusedMemo.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if FTransparent then PostMessage(Handle,TMWM__SpecialInvalidate,0,0)
  //Message.Result:=1
  else inherited;
end;

constructor TFusedMemo.Create(AOwner: TComponent);
begin
inherited;
ControlStyle:=[csCaptureMouse, csDesignInteractive,
csClickEvents,  csSetCaption, csOpaque, csDoubleClicks,
  csReplicatable, csNoStdEvents];
end;

procedure TFusedMemo.CreateParams(var Params: TCreateParams);
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

procedure TFusedMemo.DoExit;
begin
  inherited;
  if FTransparent then SendMessage(Handle,TMWM__SpecialInvalidate,0,0);
end;
Procedure TFusedMemo.WMENABLE(Var Msg: TWMPaint);
begin

end;

procedure Register;
begin
  RegisterComponents('Fused', [TFusedMemo]);
end;
end.



