unit frameRowGridUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, Vcl.Buttons, Vcl.ExtCtrls;

type
  TframeRowGrid = class(TFrame)
    Panel: TPanel;
    lblNumber: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    seNumber: TJvSpinEdit;
    Grid: TRbwRowDataGrid;
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure GridEndUpdate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    procedure ClearSelectedCol;
    procedure ClearGrid;
  end;

implementation

uses
  System.Math;

{$R *.dfm}

procedure TframeRowGrid.ClearGrid;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex, RowIndex] := '';
        Grid.Checked[ColIndex, RowIndex] := False;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TframeRowGrid.ClearSelectedCol;
var
  RowIndex: Integer;
begin
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    Grid.Cells[Grid.SelectedCol, RowIndex] := '';
    Grid.Checked[Grid.SelectedCol, RowIndex] := False;
    Grid.Objects[Grid.SelectedCol, RowIndex] := nil;
  end;
end;

procedure TframeRowGrid.GridEndUpdate(Sender: TObject);
begin
  if seNumber <> nil then
  begin
    seNumber.AsInteger := Grid.ColCount -1;
  end;
end;

procedure TframeRowGrid.sbAddClick(Sender: TObject);
begin
  seNumber.AsInteger := seNumber.AsInteger +1;
end;

procedure TframeRowGrid.sbDeleteClick(Sender: TObject);
begin
  if Grid.SelectedCol >= Grid.FixedCols  then
  begin
    if Grid.ColCount > Grid.FixedCols + 1 then
    begin
      ClearSelectedCol;
      Grid.DeleteColumn(Grid.SelectedCol);
//      GridEndUpdate(nil);
    end
    else
    begin
      ClearSelectedCol;
      seNumber.AsInteger := seNumber.AsInteger -1;
    end;
  end;
end;

procedure TframeRowGrid.sbInsertClick(Sender: TObject);
begin
  if Grid.SelectedCol >= Grid.FixedCols then
  begin
    Grid.InsertColumn(Grid.SelectedCol);
    ClearSelectedCol;
//    GridEndUpdate(nil);
  end;
end;

procedure TframeRowGrid.seNumberChange(Sender: TObject);
var
  NewColCount: integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewCount: Integer;
begin
  NewCount := seNumber.AsInteger;
  NewColCount := Max(2, NewCount+1);
  if (NewColCount < Grid.ColCount) then
  begin
    Grid.BeginUpdate;
    try
      for ColIndex := NewColCount to Grid.ColCount - 1 do
      begin
        for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
        begin
          Grid.Cells[ColIndex, RowIndex] := '';
          Grid.Checked[ColIndex, RowIndex] := False;
          Grid.Objects[ColIndex, RowIndex] := nil;
        end;
      end;
    finally
      Grid.EndUpdate;
    end;
  end;
  Grid.ColCount := NewColCount;
  seNumber.AsInteger := NewCount;
  sbDelete.Enabled := seNumber.AsInteger > 0;
end;

type
  TRbwRowDataGridCrack = class(TRbwRowDataGrid);


procedure TframeRowGrid.SetEnabled(Value: boolean);
begin
  inherited;
  sbAdd.Enabled := Value;
  sbInsert.Enabled := Value;
  Grid.Enabled := Value;
  seNumber.Enabled := Value;
  sbDelete.Enabled := Value and (seNumber.AsInteger >= 1);
  if Grid.Enabled then
  begin
    Grid.Color := clWindow;
    Grid.ColorSelectedColumn := True;
  end
  else
  begin
    Grid.Color := clBtnFace;
    TRbwRowDataGridCrack(Grid).HideEditor;
    Grid.ColorSelectedColumn := False;
  end;
end;

end.
