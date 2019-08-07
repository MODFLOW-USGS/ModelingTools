unit frmManageSettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Grids,
  RbwDataGrid4, DisplaySettingsUnit;

type
  TfrmManageSettings = class(TfrmCustomGoPhast)
    rdgSettings: TRbwDataGrid4;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    lblInstructions: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgSettingsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FDisplaySettings: TDisplaySettingsCollection;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmManageSettings: TfrmManageSettings;

implementation

uses
  frmGoPhastUnit, UndoItems;

resourcestring
  StrName = 'Name';

{$R *.dfm}

{ TfrmManageSettings }

procedure TfrmManageSettings.btnDeleteClick(Sender: TObject);
var
  ASetting: TDisplaySettingsItem;
begin
  inherited;
  if (rdgSettings.SelectedRow > 0)
    and (rdgSettings.SelectedRow < rdgSettings.RowCount) then
  begin
    if rdgSettings.Objects[0,rdgSettings.SelectedRow] <> nil then
    begin
      ASetting := rdgSettings.Objects[0,rdgSettings.SelectedRow]
        as TDisplaySettingsItem;
      FDisplaySettings.Delete(ASetting.Index);
    end;
    if rdgSettings.RowCount = 2 then
    begin
      rdgSettings.Cells[0, rdgSettings.SelectedRow] := '';
      rdgSettings.Objects[0,rdgSettings.SelectedRow] := nil;
      btnDelete.Enabled := False;
    end
    else
    begin
      rdgSettings.DeleteRow(rdgSettings.SelectedRow);
    end;
  end;
end;

procedure TfrmManageSettings.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmManageSettings.FormCreate(Sender: TObject);
begin
  inherited;
  rdgSettings.Cells[0,0] := StrName;
  FDisplaySettings := TDisplaySettingsCollection.Create(nil);
  GetData;
end;

procedure TfrmManageSettings.FormDestroy(Sender: TObject);
begin
  FDisplaySettings.Free;
  inherited;
end;

procedure TfrmManageSettings.GetData;
var
  SettingsList: TStringList;
  Index: Integer;
  ASetting: TDisplaySettingsItem;
begin
  FDisplaySettings.Assign(frmGoPhast.PhastModel.DisplaySettings);
  SettingsList := TStringList.Create;
  try
    SettingsList.Capacity := FDisplaySettings.Count;
    for Index := 0 to FDisplaySettings.Count - 1 do
    begin
      ASetting := FDisplaySettings.Items[Index]
        as TDisplaySettingsItem;
      SettingsList.AddObject(ASetting.Name, ASetting);
    end;
    SettingsList.CaseSensitive := False;
    SettingsList.Sorted := True;
    rdgSettings.RowCount := SettingsList.Count + 1;
    for Index := 0 to SettingsList.Count - 1 do
    begin
      rdgSettings.Cells[0,Index+1] := SettingsList[Index];
      rdgSettings.Objects[0,Index+1] := SettingsList.Objects[Index];
    end;
  finally
    SettingsList.Free;
  end;
end;

procedure TfrmManageSettings.rdgSettingsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ASetting: TDisplaySettingsItem;
begin
  inherited;
  if (ARow > 0) and (ACol = 0) then
  begin
    ASetting := rdgSettings.Objects[ACol,ARow] as TDisplaySettingsItem;
    if ASetting <> nil then
    begin
      ASetting.Name := Value;
    end;
  end;
end;

procedure TfrmManageSettings.SetData;
var
  Undo: TUndoEditDisplaySettings;
begin
  Undo := TUndoEditDisplaySettings.Create(FDisplaySettings);
  frmGoPhast.UndoStack.Submit(Undo);
end;

end.
