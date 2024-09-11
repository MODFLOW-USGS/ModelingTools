unit MyFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetWindowStateUnit;

type
  TMyForm = class(TForm)
    RBW_SetWindowState1: TRBW_SetWindowState;
    procedure SetWindowState1WindowStateChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    OldWindowState : TWindowState;
    procedure RestoreMethod(Sender: TObject);
  end;

{#BACKUP  ..\Components\SetWindowState\SetWindowStateUnit.pas}

var
  MyForm: TMyForm;

implementation

{$R *.DFM}

procedure TMyForm.SetWindowState1WindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    Application.Minimize;
  end
  else
  begin
    OldWindowState := WindowState;
  end;
end;

procedure TMyForm.RestoreMethod(Sender: TObject);
begin
  WindowState := OldWindowState;
end;


procedure TMyForm.FormActivate(Sender: TObject);
begin
  Application.OnRestore := RestoreMethod;
end;

procedure TMyForm.FormHide(Sender: TObject);
begin
  OldWindowState := WindowState;
end;

end.
