{******************************************************************************}
{                                                                              }
{                    TTBNArea - The Tray Icon Component                        }
{                               Version 3.1                                    }
{                               Jan 28, 1999                                   }
{                                                                              }
{                    © NailySoft Reinhard Nägele, 1997 - 99                    }
{                      naegele@fh-nuertingen.de                                }
{                      www.fh-nuertingen.de/~naegele/                          }
{                                                                              }
{******************************************************************************}

unit TBNArea;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Menus;

const
  NSM_TASKBAREVENT: PChar = 'NSM_NewTNIMessage';

type
  TChangeKind = (ckIcon, ckTip);
  TChangeEvent = procedure(Sender: TObject; ChangeKind: TChangeKind) of object;
  TChangingEvent = procedure(Sender: TObject; ChangeKind: TChangeKind;
    var AllowChange: boolean) of object;

  TTBNArea = class(TComponent)
  private
    FPopupMenuL: TPopupMenu;
    FPopupMenuR: TPopupMenu;
    FIcon: TIcon;
    FTip: string;
    FCopyright: string;
    FEnabled: boolean;
    FPopupOnBoth: boolean;
    FOnChange: TChangeEvent;
    FOnChanging: TChangingEvent;
    FOnLeftClick: TNotifyEvent;
    FOnRightClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FWnd: HWnd;
    TNIMessage: UINT;
    procedure SetTNIData(Msg: cardinal; Flags: UINT);
    procedure DoPopup(i: byte);
    procedure SetCopyright(s: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetIcon(const AnIcon: TIcon);
    procedure SetTip(const s: string);
  protected
    procedure WndProc(var Msg: TMessage);
    procedure DoChange(const ChangeKind: TChangeKind); virtual;
    procedure DoChanging(const ChangeKind: TChangeKind; var AllowChange: boolean); virtual;
    procedure DoOnLeftClick; virtual;
    procedure DoOnRightClick; virtual;
    procedure DoOnDblClick; virtual;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Copyright: string read FCopyright write SetCopyright;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupOnBoth: boolean read FPopupOnBoth write FPopupOnBoth default false;
    property PopupMenuL: TPopupMenu read FPopupMenuL write FPopupMenuL;
    property PopupMenuR: TPopupMenu read FPopupMenuR write FPopupMenuR;
    property Tip: string read FTip write SetTip;
    property OnChange: TChangeEvent read FOnChange write FOnChange;
    property OnChanging: TChangingEvent read FOnChanging write FOnChanging;
    property OnLeftClick: TNotifyEvent read FOnLeftClick write FOnLeftClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

procedure Register;

implementation

uses
  ShellAPI;

procedure TTBNArea.SetTNIData(Msg: cardinal; Flags: UINT);
var
  TNIData: TNOTIFYICONDATA;
begin
  with TNIData do begin
//    cbSize := SizeOf(TNOTIFYICONDATA);
    cbSize := TNOTIFYICONDATA.SizeOf;
    Wnd := FWnd;
    uID := 1;
    uFlags := Flags;
    uCallbackMessage := TNIMessage;
    hIcon := FIcon.Handle;
    StrCopy(szTip, PChar(FTip));
    Shell_NotifyIcon(Msg, @TNIData);
  end;
end;

constructor TTBNArea.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  TNIMessage := RegisterWindowMessage(NSM_TASKBAREVENT);
  FWnd := AllocateHWnd(WndProc);
  FIcon := TIcon.Create;
  FEnabled := true;
  FCopyright := '© R. Nägele 1997 - 99, Version 3.1';
  FPopupOnBoth := false;
  {RBW begin change}
  // Remove application from the task bar.
  if not (csDesigning in ComponentState) then
  begin
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
      GetWindowLong(Application.Handle, GWL_EXSTYLE) or
      WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
  end;
  {RBW end change}
end;

procedure TTBNArea.Loaded;
begin
  inherited Loaded;
  if FEnabled and not (csDesigning in ComponentState) then
    SetTNIData(NIM_ADD, NIF_MESSAGE or NIF_ICON or NIF_TIP);
end;

procedure TTBNArea.SetCopyright(s: string);
begin
  s := '© R. Nägele 1997 - 99, Version 3.1';
  FCopyright := s;
end;

procedure TTBNArea.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if csDesigning in ComponentState then Exit;
    if FEnabled then
      SetTNIData(NIM_ADD, NIF_MESSAGE or NIF_ICON or NIF_TIP)
    else
      SetTNIData(NIM_DELETE, NIF_MESSAGE or NIF_ICON or NIF_TIP);
  end;
end;

procedure TTBNArea.WndProc(var Msg: TMessage);
begin
  with Msg do begin
    if Msg = TNIMessage then begin
      case LParamLo of
        WM_LBUTTONDOWN:   DoOnLeftClick;
        WM_RBUTTONDOWN:   DoOnRightClick;
        WM_LBUTTONDBLCLK: DoOnDblClick;
      end;
    end else
      Result := DefWindowProc(FWnd, Msg, wParam, lParam);
  end;
end;

procedure TTBNArea.DoPopup(i: byte);
var
  Point : TPoint;
begin
  GetCursorPos(Point);
  SetForeGroundWindow(FWnd);
  case i of
    1: FPopupmenuL.Popup(Point.X, Point.Y);
    2: FPopupmenuR.Popup(Point.X, Point.Y);
  end;
  PostMessage(0, 0, 0, 0);
end;

procedure TTBNArea.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if Operation = opRemove then begin
    if Component = FPopupMenuL then FPopupMenuL := nil;
    if Component = FPopupMenuR then FPopupMenuR := nil;
  end;
end;

destructor TTBNArea.Destroy;
begin
  if FEnabled and not (csDesigning in ComponentState) then
    SetTNIData(NIM_DELETE, NIF_MESSAGE or NIF_ICON or NIF_TIP);
  FIcon.Free;
  DeallocateHWnd(FWnd);
  inherited Destroy;
end;

procedure TTBNArea.SetIcon(const AnIcon: TIcon);
var
  AllowChange: boolean;
begin
  if FIcon <> AnIcon then begin
    DoChanging(ckIcon, AllowChange);
    if AllowChange then begin
      FIcon.Assign(AnIcon);
      if FEnabled  then begin
        if not (csDesigning in ComponentState) then
          SetTNIData(NIM_MODIFY, NIF_ICON);
        DoChange(ckIcon);
      end;
    end;
  end;
end;

procedure TTBNArea.SetTip(const s: string);
var
  AllowChange: boolean;
begin
  if FTip <> s then begin
    DoChanging(ckTip, AllowChange);
    if AllowChange then begin
      FTip := s;
      if FEnabled then begin
        if not (csDesigning in ComponentState) then
          SetTNIData(NIM_MODIFY, NIF_TIP);
        DoChange(ckTip);
      end;
    end;
  end;
end;

procedure TTBNArea.DoOnLeftClick;
begin
  if Assigned(FPopupMenuL) then
    DoPopup(1)
  else if Assigned(FOnLeftClick) then
    FOnLeftClick(self);
end;

procedure TTBNArea.DoOnRightClick;
begin
  if (Assigned(FPopupMenuL) and FPopupOnBoth) then begin
    DoPopup(1);
    Exit;
  end;
  if Assigned(FPopupMenuR) then
    DoPopup(2)
  else if Assigned(FOnRightClick) then
    FOnRightClick(self);
end;

procedure TTBNArea.DoOnDblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(self);
end;

procedure TTBNArea.DoChange(const ChangeKind: TChangeKind);
begin
  if Assigned(FOnChange) then
    FOnChange(self, ChangeKind);
end;

procedure TTBNArea.DoChanging(const ChangeKind: TChangeKind; var AllowChange: boolean);
begin
  AllowChange := true;
  if Assigned(FOnChanging) then
    FOnChanging(self, ChangeKind, AllowChange);
end;

procedure Register;
begin
  RegisterComponents('NailySoft', [TTBNArea]);
end;

end.

