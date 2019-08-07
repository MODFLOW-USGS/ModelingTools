unit frmTreeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls;

type
  TfrmTree = class(TForm)
    procedure TreeEnter(Sender: TObject);
    procedure TreeLeave(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormHide(Sender: TObject);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    StoredMouseDown: TMouseEvent;
    { Public declarations }
  end;


implementation



{$R *.dfm}

procedure TfrmTree.FormHide(Sender: TObject);
begin
  MouseCapture := False;
end;

procedure TfrmTree.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TfrmTree.TreeEnter(Sender: TObject);
begin
  MouseCapture := False;
end;

procedure TfrmTree.TreeLeave(Sender: TObject);
begin
  if Visible then
  begin
    MouseCapture := True;
  end;
end;

procedure TfrmTree.TreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ATree: TVirtualStringTree;
  HitInfo: THitInfo;
begin
  if Assigned(StoredMouseDown) then
  begin
    StoredMouseDown(Sender, Button, Shift, X, Y);
  end;
  ATree := Sender as TVirtualStringTree;
  ATree.GetHitTestInfoAt(X, Y, True, HitInfo);
  if (HitInfo.HitNode <> nil) and (hiOnItemLabel in HitInfo.HitPositions) then
  begin
    ModalResult := mrOK;
  end;
end;

end.
