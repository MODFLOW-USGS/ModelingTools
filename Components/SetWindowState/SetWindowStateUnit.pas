unit SetWindowStateUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  extctrls;

type
  TRBW_SetWindowState = class(TComponent)
  private
    FOnMyWindowStateChange : TNotifyEvent;
    FOldWindowState : TWindowState;
    FTTimer : TTimer;
    function GetMyWindowState : TWindowState;
    procedure SetMyWindowState(Value: TWindowState);
    Function GetMyOwner: TComponent;
    procedure SetOwner(AComponent : TComponent);
    procedure OnMyTimer(Sender: TObject);
  published
    property WindowState: TWindowState read GetMyWindowState write SetMyWindowState
      default wsNormal;
    property OnWindowStateChange: TNotifyEvent read FOnMyWindowStateChange
      write FOnMyWindowStateChange ;
    Property Owner : TComponent read GetMyOwner write SetOwner;
  public
    Constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRBW_SetWindowState]);
end;

{ TRBW_SetWindowState }

constructor TRBW_SetWindowState.Create(AOwner: TComponent);
begin
  inherited;
  Assert (AOwner is TForm);
  FTTimer := TTimer.Create(self);
  FOldWindowState := WindowState;
  FTTimer.Interval := 100;
  FTTimer.OnTimer := OnMyTimer;
  FTTimer.Enabled := True;
end;

function TRBW_SetWindowState.GetMyOwner: TComponent;
begin
  result := inherited Owner;
end;

function TRBW_SetWindowState.GetMyWindowState: TWindowState;
begin
  result := (Owner as TForm).WindowState;

end;

procedure TRBW_SetWindowState.OnMyTimer(Sender: TObject);
begin
  SetMyWindowState(WindowState);
end;

procedure TRBW_SetWindowState.SetMyWindowState(Value: TWindowState);
begin
  if FOldWindowState <> Value then
  begin
    FOldWindowState := Value;
    if Assigned(FOnMyWindowStateChange) then FOnMyWindowStateChange(self);
  end;
end;

procedure TRBW_SetWindowState.SetOwner(AComponent: TComponent);
begin
  Assert(AComponent is TForm);
end;

end.
 