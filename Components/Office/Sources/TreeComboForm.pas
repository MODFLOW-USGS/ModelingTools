
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  }
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit TreeComboForm;

interface

{$I OFFICEVER.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo, BaseComboForm, ComCtrls;

type

  TfrmTreeCombo = class(TfrmCustomCombo)
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    First: boolean;
    procedure DoChange(Sender: TObject; Node: TTreeNode);
  public
    { Public declarations }
    procedure AfterSetCombo; override;
  end;

var
  frmTreeCombo: TfrmTreeCombo;

implementation {===============================================================}

uses TreeCombo;

{$R *.DFM}

type TksoTreeComboBoxCrack = class(TksoTreeComboBox);

procedure TfrmTreeCombo.AfterSetCombo;
begin
  First := true;
  with (Combo as TksoTreeComboBox) do
  begin
    TreeView.Parent := Self;
    TreeView.Font := Font;
    TreeView.Visible := true;
    TreeView.BoundsRect := Rect(2, 2, Self.Width-2, Self.Height-2);
    TreeView.OnChange := DoChange;
{    Self.Width := BarWidth + 4;
    Self.Height := BarHeight + 4;}
  end;
end;

procedure TfrmTreeCombo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TfrmTreeCombo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    Close;
    Key := #0;
  end;
end;

procedure TfrmTreeCombo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  with (Combo as TksoTreeComboBox) do
  begin
  // RBW The following line was moved to before the assignment of
  // TreeView.Parent to prevent access violations.
    TreeView.OnChange := nil;
    TreeView.Parent := Combo;
    TreeView.Top := Screen.Height;
    AfterDropDown;
  end;
  TksoTreeComboBoxCrack(Combo).FormShown := True;
end;

procedure TfrmTreeCombo.DoChange;
begin
  // rbw begin change
  // This change allows the form to close the first time
  // the user selects a tree node.
//  if First then
  if First and TksoTreeComboBoxCrack(Combo).FormShown then
  // rbw end change
  begin
    First := false;
    Exit;
  end;
  Close;
end;

end.
