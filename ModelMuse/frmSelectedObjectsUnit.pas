{@abstract(The main purpose of @name is to define @link(TfrmSelectedObjects)
  which is used to display the selected @link(TScreenObject)s.)}
unit frmSelectedObjectsUnit;  

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls;

type
  {@abstract(@name is used to display the selected @link(TScreenObject)s.)}
  TfrmSelectedObjects = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname.
    btnClose: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TListBox;
    // @name displays the selected @link(TScreenObject)s.
    lbSelected: TListBox;
    // @name: TPanel;
    // @name holds the buttons on the bottom of @classname.
    pnlBottom: TPanel;
    // @name calls @link(UpdateDisplay).
    // The first time it is called, it sets the position of the form.
    procedure FormShow(Sender: TObject);
  private
    // @name indicates whether the form has been displayed or not.
    FShown: boolean;
    { Private declarations }
  public
    // @name fills @link(lbSelected) with the selected @link(TScreenObject)s.
    procedure UpdateDisplay;
    { Public declarations }
  end;

  function frmSelectedObjects: TfrmSelectedObjects;
  procedure FreeFrmSelectedObjects;

implementation

uses frmGoPhastUnit, ScreenObjectUnit;

{$R *.dfm}

var
  // @name is the instance of @link(TfrmSelectedObjects) used in GoPhast.
  FfrmSelectedObjects: TfrmSelectedObjects;

function frmSelectedObjects: TfrmSelectedObjects;
begin
  if FfrmSelectedObjects = nil then
  begin
    FfrmSelectedObjects := TfrmSelectedObjects.Create(nil);
  end;
  result := FfrmSelectedObjects;
end;

procedure FreeFrmSelectedObjects;
begin
  FreeAndNil(FfrmSelectedObjects);
end;

{ TfrmSelectedObjects }

procedure TfrmSelectedObjects.UpdateDisplay;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  if not Visible then
    Exit;
  lbSelected.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      lbSelected.Items.AddObject(AScreenObject.Name, AScreenObject);
    end;
  end;
end;

procedure TfrmSelectedObjects.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDisplay;
  if not FShown then
  begin
    FShown := True;
    AdjustFormPosition(dpLeft);
  end;
end;

initialization

finalization
  FfrmSelectedObjects.Free;

end.


