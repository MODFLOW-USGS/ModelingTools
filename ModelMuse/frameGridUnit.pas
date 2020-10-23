unit frameGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask,
  JvSpin;

type
  TframeGrid = class(TFrame)
    Panel: TPanel;
    Grid: TRbwDataGrid4;
    seNumber: TJvSpinEdit;
    lbNumber: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    procedure GridEndUpdate(Sender: TObject);
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    procedure ClearSelectedRow;
    procedure ClearGrid;
    { Public declarations }
  end;

implementation

uses
  Math;

{$R *.dfm}

procedure TframeGrid.GridEndUpdate(Sender: TObject);
begin
  if seNumber <> nil then
  begin
    seNumber.AsInteger := Grid.RowCount -1;
  end;
end;

procedure TframeGrid.sbAddClick(Sender: TObject);
begin
  seNumber.AsInteger := seNumber.AsInteger +1;
end;

procedure TframeGrid.sbDeleteClick(Sender: TObject);
begin
  if Grid.SelectedRow >= Grid.FixedRows  then
  begin
    if Grid.RowCount > Grid.FixedRows + 1 then
    begin
      ClearSelectedRow;
      Grid.DeleteRow(Grid.SelectedRow);
      GridEndUpdate(nil);
    end
    else
    begin
      ClearSelectedRow;
      seNumber.AsInteger := seNumber.AsInteger -1;
    end;
  end;
end;

procedure TframeGrid.sbInsertClick(Sender: TObject);
begin
  if Grid.SelectedRow >= Grid.FixedRows then
  begin
    Grid.InsertRow(Grid.SelectedRow);
    ClearSelectedRow;
    GridEndUpdate(nil);
  end;
end;

type
  TRbwDataGrid4Crack = class(TRbwDataGrid4);

procedure TframeGrid.SetEnabled(Value: boolean);
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
    Grid.ColorSelectedRow := True;
  end
  else
  begin
    Grid.Color := clBtnFace;
    TRbwDataGrid4Crack(Grid).HideEditor;
    Grid.ColorSelectedRow := False;
//    Grid.Invalidate;
  end;
end;

procedure TframeGrid.ClearSelectedRow;
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Cells[ColIndex, Grid.SelectedRow] := '';
    Grid.Checked[ColIndex, Grid.SelectedRow] := False;
    Grid.Objects[ColIndex, Grid.SelectedRow] := nil;
  end;
end;

procedure TframeGrid.seNumberChange(Sender: TObject);
var
  NewRowCount: integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewCount: Integer;
begin
  NewCount := seNumber.AsInteger;
  NewRowCount := Max(2, NewCount+1);
  if (NewRowCount < Grid.RowCount) then
  begin
    Grid.BeginUpdate;
    try
      for RowIndex := NewRowCount to Grid.RowCount - 1 do
      begin
        for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
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
  Grid.RowCount := NewRowCount;
  seNumber.AsInteger := NewCount;
  sbDelete.Enabled := seNumber.AsInteger > 0;
end;

procedure TframeGrid.ClearGrid;
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

initialization
  RegisterClass(TframeGrid);

end.
